import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import io.netty.bootstrap.Bootstrap;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.channel.*;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.channel.socket.nio.NioSocketChannel;
import io.netty.handler.codec.string.StringEncoder;
import io.netty.util.concurrent.Future;
import io.netty.util.concurrent.GenericFutureListener;

public class Main {

    static final int PAYLOAD_SIZE = Integer.valueOf(System.getenv("BS_LEN"));
    static final long MAX_CLIENTS = Integer.valueOf(System.getenv("MAX_CLIENTS"));
    static final String REMOTE_HOST = System.getenv("REMOTE_HOST");
    static final int PORT = Integer.valueOf(System.getenv("PORT"));
    static final int reportPer = 10;
    static final int SO_LINGER = Integer.valueOf(System.getenv("SO_LINGER"));

    static class StringSender {
        private final String content;
        private final EventLoopGroup reactor;
        private Channel channel;

        StringSender(EventLoopGroup reactor, String content) {
            this.reactor = reactor;
            this.content = content;
        }
        
        ChannelFuture connect(String host, int port) {
            Bootstrap clientBootstrap = new Bootstrap();

            ChannelInitializer<Channel> clientInitializer = new ChannelInitializer<Channel>() {
                @Override
                protected void initChannel(Channel ch) throws Exception {
                    ch.pipeline().addLast(new StringEncoder(Charset.forName("UTF-8")));
                }
            };

            return clientBootstrap.group(reactor)
                    .channel(NioSocketChannel.class)
                    .option(ChannelOption.SO_LINGER, SO_LINGER)
                    .handler(clientInitializer)
                    .connect(host, port)
                    .addListener((ChannelFuture cf) -> {
                        channel = cf.channel();
                        if (channel == null) {
                            System.out.println("XXX: channel is null? " + cf);
                        }
                    });
        }
        
        ChannelFuture sendAll(final boolean andClose) {
            final ChannelPromise cf = channel.newPromise();
            channel.writeAndFlush(content).addListener(new ChannelFutureListener() {
                @Override
                public void operationComplete(ChannelFuture future) throws Exception {
                    if (andClose) {
                        future.channel().close().addListener(new ChannelFutureListener() {
                            @Override
                            public void operationComplete(ChannelFuture future) throws Exception {
                                cf.setSuccess();
                            }
                        });
                    }
                    else {
                        cf.setSuccess();
                    }
                }
            });

            return cf;
        }
    }
    
    public static void main(String[] args) {
        final NioEventLoopGroup reactor = new NioEventLoopGroup();

        String bs = new String(new char[PAYLOAD_SIZE]);

        System.out.println("Connecting...");
        List<StringSender> clients = new ArrayList<>();
        AtomicLong nConnected = new AtomicLong(0);
        AtomicLong nSent = new AtomicLong(0);

        for (int i = 0; i < MAX_CLIENTS; ++i) {
            StringSender client = new StringSender(reactor, bs);
            client.connect(REMOTE_HOST, PORT).addListener((ChannelFuture f) -> {
                //System.out.println("connect done: " + f.channel());
                long connected = nConnected.incrementAndGet();
                if (connected >= MAX_CLIENTS) {
                    System.out.println("All connected, start sending...");
                    clients.forEach(c -> c.sendAll(true /* Seems to not matter a lot */).addListener(sendOk -> {
                        //System.out.println("sendAll done: " + c.channel);
                        long sent = nSent.incrementAndGet();
                        if (sent >= MAX_CLIENTS) {
                            System.out.println("All sent, stopping the reactor...");
                            reactor.shutdownGracefully().addListener((Future<Object> shutdownF) -> {
                                System.out.println("Reactor stopped.");
                            });
                        }
                        else if (sent % reportPer == 0) {
                            System.out.printf("sendAll: finished (%d/%d)\n", sent, MAX_CLIENTS);
                        }
                    }));
                }
                else if (connected % reportPer == 0) {
                    System.out.printf("connect: finished (%d/%d)\n", connected, MAX_CLIENTS);
                }
            });
            clients.add(client);
        }
    }
}

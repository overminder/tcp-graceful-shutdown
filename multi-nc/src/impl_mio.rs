use std::net::ToSocketAddrs;
use std::collections::HashMap;
use std::io::Read;

use mio::{EventLoop, Handler, Token, EventSet, PollOpt, TryRead, TryWrite};
use mio::tcp::{TcpListener, TcpStream};

struct Reactor {
    listener: TcpListener,
    success_count: isize,
    total_count: isize,
    expected_len: isize,
    next_token: usize,
    peers: HashMap<Token, TcpStream>,
    peer_read_counts: HashMap<Token, isize>,
    read_buf: Vec<u8>,
}

impl Reactor {
    fn new(listener: TcpListener, expected_len: isize) -> Self {
        Reactor {
            listener: listener,
            success_count: 0,
            total_count: 0,
            expected_len: expected_len,
            next_token: 2,
            peers: HashMap::new(),
            peer_read_counts: HashMap::new(),
            read_buf: vec![0; 4096],
        }
    }

    fn remove_peer(&mut self, event_loop: &mut EventLoop<Self>, token: Token) {
        {
            let peer = match self.peers.get(&token) {
                Some(peer) => peer,
                None => {
                    //println!("XXX: peer {:?} already removed", token);
                    return;
                },
            };
            event_loop.deregister(peer);
        }
        self.peers.remove(&token);

        let read_counts = self.peer_read_counts[&token];
        self.peer_read_counts.remove(&token);

        let ok = read_counts == self.expected_len;

        if ok {
            self.success_count += 1;
        }
        self.total_count += 1;

        if self.total_count % 100 == 0 {
            println!("Stat (success / total): ({}/{}); read: ({}/{}); ok = {}",
                      self.success_count, self.total_count,
                      read_counts, self.expected_len, ok);
        }

        //println!("Removed peer: {:?}, read_counts = {}, ok = {}", token, read_counts, ok);
    }
}

impl Handler for Reactor {
    type Timeout = ();
    type Message = ();

    fn ready(&mut self, event_loop: &mut EventLoop<Self>, token: Token,
             events: EventSet) {
        //println!("accepter ready: {:?}, {:?}", token, events);

        if token == Token(1) {
            // Accept
            assert!(events.is_readable());

            loop {
                let (peer, addr) = match self.listener.accept().unwrap() {
                    Some(x) => x,
                    None => {
                        // EWOULDBLOCK
                        break;
                    }
                };
                //println!("peer = {:?}", peer);

                let peer_token = Token(self.next_token);
                self.next_token += 1;
                event_loop.register(&peer, peer_token, EventSet::all(), PollOpt::edge()).unwrap();

                self.peers.insert(peer_token, peer);
                self.peer_read_counts.insert(peer_token, 0);
            }
        }
        else {
            // Peers
            if events.is_readable() {
                let mut stop_reason = None;
                let mut read_count = 0;
                {
                    let peer = match self.peers.get_mut(&token) {
                        Some(peer) => peer,
                        None => {
                            println!("XXX: peer.get_mut({:?}) gives None", token);
                            return;
                        },
                    };
                    let mut buf = &mut self.read_buf;

                    loop {
                        match peer.read(&mut buf) {
                            Ok(n_read) => {
                                if n_read == 0 {
                                    // EAGAIN?
                                    break;
                                }
                                else {
                                    read_count += n_read as isize;
                                }
                            },
                            Err(e) => {
                                let os_err = e.raw_os_error();
                                if os_err == Some(35) || os_err == Some(11) {
                                    // EAGAIN || EWOULDBLOCK
                                }
                                else {
                                    stop_reason = Some(format!("peer.read: {:?}", e));
                                }
                                break;
                            },
                        }
                    }
                }
                match self.peer_read_counts.get_mut(&token) {
                    Some(count) => *count += read_count,
                    None => {
                        println!("XXX: peer_read_counts.get_mut({:?}) gives None", token);
                        return;
                    },
                };
                if let Some(reason) = stop_reason {
                    //println!("Stop reason: {}", reason);
                    self.remove_peer(event_loop, token);
                }
            }
            if events.is_writable() {
            }
            if events.is_hup() {
                self.remove_peer(event_loop, token);
            }
            if events.is_error() {
                self.remove_peer(event_loop, token);
            }
        }
    }
}

pub fn spawn_checkers(port: u16, expected_len: isize) {
    let addr = ("0.0.0.0", port).to_socket_addrs().unwrap().next().unwrap();
    let listener = TcpListener::bind(&addr).unwrap();
    let mut event_loop = EventLoop::new().unwrap();
    event_loop.register(&listener, Token(1), EventSet::all(), PollOpt::edge()).unwrap();

    let mut reactor = Reactor::new(listener, expected_len);
    println!("EventLoop listening on :{}.", port);
    event_loop.run(&mut reactor).unwrap();
    println!("Done running reactor.");
}

#import <Foundation/Foundation.h>

#import "StringSender.h"

StringSender *mk(NSString *host, int port, NSString *toSend, void (^onDone)()) {
    StringSender *c = [[StringSender alloc] init];
    c.host = host;
    c.port = port;
    c.stringToSend = toSend;
    c.onDone = onDone;
    if ([c setup]) {
        [c open];
    }

    return c;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSRunLoop *reactor = [NSRunLoop currentRunLoop];
        __block BOOL shouldKeepRunning = YES;

        NSString *payload = @"1234567890";
        NSString *host = [NSString stringWithCString:getenv("HOST") encoding:NSASCIIStringEncoding];
        //NSString *host = @"10.2.129.53";
        //NSString *host = @"localhost";
        int port = 6789;
        NSString *stringToSend = [@"" stringByPaddingToLength:10000 * [payload length]
                                                   withString:payload startingAtIndex:0];
        NSMutableArray *clients = [[NSMutableArray alloc] init];

        ssize_t MAX_CLIENTS = atoi(getenv("MAX_CLIENTS"));
        NSLog(@"MAX_CLIENTS = %ld", MAX_CLIENTS);
        __block ssize_t openedClients = 0;
        __block ssize_t finishedClients = 0;

        void (^onWriteAvailable)() = ^() {
            if (++openedClients >= MAX_CLIENTS) {
                // Send all at once.
                for (StringSender *client in clients) {
                    [client doSend];
                }
            }
        };

        void (^onDone)() = ^() {
            if (++finishedClients >= MAX_CLIENTS) {
                NSLog(@"All %zd clients have finished.", MAX_CLIENTS);
                shouldKeepRunning = NO;
                //raise(SIGKILL);
            }
        };
        
        for (ssize_t i = 0; i < MAX_CLIENTS; ++i) {
            StringSender *client = mk(host, port, stringToSend, onDone);
            client.onWriteAvailable = onWriteAvailable;
            [clients addObject:client];
        }

        while (shouldKeepRunning && [reactor runMode:NSDefaultRunLoopMode beforeDate:[NSDate distantFuture]]) {
            // Loop!
        }
    }
	
	return 0;
}

#import "StringSender.h"

#import <sys/socket.h>
#import <netinet/in.h>
#import <netinet/tcp.h>

@implementation StringSender {
    ssize_t sendIx;
}

- (instancetype)init {
    if (self = [super init]) {
        sendIx = 0;
    }
    return self;
}

static void configureSocket(NSOutputStream *s) {
    // Get socket fd
    CFDataRef socketData = CFWriteStreamCopyProperty((__bridge CFWriteStreamRef) s, kCFStreamPropertySocketNativeHandle);
    if (!socketData) {
        NSLog(@"Oops, can't get socketData.");
        return;
    }
    //NSLog(@"Got socketData");
    CFSocketNativeHandle handle;
    CFDataGetBytes(socketData, CFRangeMake(0, sizeof(CFSocketNativeHandle)), (void *) &handle);
    CFRelease(socketData);

    // Set NO_LINGERING
    int no = 0;
    int yes = 1;
    struct linger lopt;

    // The only bad conf: onoff = 1; linger = 0
    lopt.l_onoff = 1;
    lopt.l_linger = 1;
    //setsockopt(handle, SOL_SOCKET, SO_LINGER, (void *) &lopt, sizeof(lopt));
}

- (BOOL)setup {
    //NSLog(@"Setting up connection to %@ : %i", self.host, self.port);

    CFReadStreamRef readStream;
    CFWriteStreamRef writeStream;

    CFStreamCreatePairWithSocketToHost(kCFAllocatorDefault,
            CFBridgingRetain(self.host), self.port, &readStream, &writeStream);
    
    self.inputStream = CFBridgingRelease(readStream);
    self.outputStream = CFBridgingRelease(writeStream);
    
    //NSLog(@"Status of outputStream: %lu", [self.outputStream streamStatus]);
    
    return YES;
}

- (void)open {
    //NSLog(@"Opening streams.");
    
    [self.inputStream setDelegate:self];
    [self.outputStream setDelegate:self];

    NSRunLoop *reactor = [NSRunLoop currentRunLoop];
    
    [self.inputStream scheduleInRunLoop:reactor forMode:NSDefaultRunLoopMode];
    [self.outputStream scheduleInRunLoop:reactor forMode:NSDefaultRunLoopMode];
    
    [self.inputStream open];
    [self.outputStream open];
}

- (void)closeWrite {
    //NSLog(@"closeWrite called");
    //[self.outputStream close];
    [self close];
}

- (void)close {
    //NSLog(@"Closing streams.");

    if (self.onDone) {
        self.onDone();
        self.onDone = nil;
    }
    
    [self.inputStream close];
    [self.outputStream close];

    NSRunLoop *reactor = [NSRunLoop currentRunLoop];
    
    [self.inputStream removeFromRunLoop:reactor forMode:NSDefaultRunLoopMode];
    [self.outputStream removeFromRunLoop:reactor forMode:NSDefaultRunLoopMode];
    
    [self.inputStream setDelegate:nil];
    [self.outputStream setDelegate:nil];
    
    self.inputStream = nil;
    self.outputStream = nil;
}

- (void)stream:(NSStream *)stream handleEvent:(NSStreamEvent)event {
    switch (event) {
        case NSStreamEventHasSpaceAvailable:
            if (stream == self.outputStream) {
                configureSocket(self.outputStream);
                if (self.onWriteAvailable) {
                    self.onWriteAvailable();
                    self.onWriteAvailable = nil;
                }
                else {
                    [self doSend];
                }
            }
            break;
        case NSStreamEventHasBytesAvailable:
            if (stream == self.inputStream) {
                //NSLog(@"inputStream is ready."); 
            break;
        }
        default:
            //NSLog(@"Event: %lu", event);
            break;
    }
}

- (void)doSend {
    uint8_t *buf = (uint8_t *) [self.stringToSend UTF8String];
    ssize_t bufLen = strlen((char *) buf);

    if (sendIx >= bufLen) {
        return;
    }

    ssize_t maxLength = bufLen - sendIx;
    ssize_t nWrote = [self.outputStream write:buf + sendIx maxLength:maxLength];
    sendIx += nWrote;
    //NSLog(@"Sending %lu/%lu", sendIx, bufLen);

    if (nWrote == 0 || sendIx >= bufLen) {
        [self closeWrite];
    }
}

@end

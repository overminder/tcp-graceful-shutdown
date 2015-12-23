#import <Foundation/Foundation.h>

@interface StringSender : NSObject <NSStreamDelegate>

@property NSString *host;
@property int port;
@property NSString *stringToSend;
@property int soLinger;

@property NSInputStream *inputStream;
@property NSOutputStream *outputStream;
@property (copy) void (^onDone)();
@property (copy) void (^onWriteAvailable)();

- (BOOL)setup;
- (void)open;
- (void)close;
- (void)stream:(NSStream *)stream handleEvent:(NSStreamEvent)event;

- (void)doSend;

- (void)configureSocket:(NSOutputStream *)s;

@end

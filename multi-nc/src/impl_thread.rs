
use std::io::Read;
use std::net::{TcpListener, TcpStream};
use std::thread;
use std::sync::{Arc, Mutex};

fn handle_client(mut stream: TcpStream, expected_len: isize, counter_ref: Arc<Mutex<(isize, isize)>>) {
    let mut n_read = 0;
    let mut reason = None;
    loop {
        let mut buf = [0_u8];
        match stream.read(&mut buf) {
            Ok(read_len) => {
                if read_len == 0 {
                    break;
                }
                n_read = n_read + read_len as isize;
            },
            Err(e) => {
                reason = Some(format!("expected: {}, got: {}, stream.read: {:?}",
                                      expected_len, n_read, e));
                break;
            },
        }
    }
    let ok = n_read == expected_len;
    let counter = {
        let mut counter = counter_ref.lock().unwrap();
        counter.1 += 1;
        if ok {
            counter.0 += 1
        }
        *counter
    };
    println!("(ok / all): {:?}, reason = {:?}", counter, reason);
}

pub fn spawn_checkers(port: u16, expected_len: isize) {
    let listener = TcpListener::bind(("0.0.0.0", port)).unwrap();
    println!("Listening at :{}", port);
    let client_count_ref = Arc::new(Mutex::new((0, 0)));
    for stream in listener.incoming() {
         match stream {
             Ok(stream) => {
                 let client_count_ref = client_count_ref.clone();
                 thread::spawn(move|| { // connection succeeded
                     handle_client(stream, expected_len, client_count_ref)
                 });
             }
             Err(e) => { /* connection failed */ }
         }
    }
}

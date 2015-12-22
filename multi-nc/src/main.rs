extern crate multi_nc;

use std::env;

//use multi_nc::impl_thread::spawn_checkers;
use multi_nc::impl_mio::spawn_checkers;

fn main() {
    let port = env::var("PORT")
        .unwrap_or("6789".to_owned())
        .parse().unwrap();
    let expected_len = env::var("BS_LEN")
        .unwrap_or("1000".to_owned())
        .parse().unwrap();
    spawn_checkers(port, expected_len);
}

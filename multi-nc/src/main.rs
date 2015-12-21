extern crate multi_nc;

//use multi_nc::impl_thread::spawn_checkers;
use multi_nc::impl_mio::spawn_checkers;

fn main() {
    spawn_checkers(6789, 100000);
}

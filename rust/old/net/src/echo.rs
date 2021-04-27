mod echo_server {
    fn handle_client(stream: TcpStream){
        let mut buf = String::new();
        loop {
            let nbytes = stream.read_to_string(&buf);
            match nbytes {
                Ok(nbytes) => {
                    stream.write(buf.as_bytes()).unwrap();
                }
                Err(e) => {
                    break;
                }
            }
            buf.clear();
        }
    }
                
    pub fn run<T: net::ToSocketAddrs>(addr: T) -> ! {
        //panic on error, not the best thing to do but oh well
        let sock = TcpListener::bind(addr).unwrap();
        for connect in sock.incoming() {
            match connect {
                Ok(connect) => {
                    thread::spawn(move || {handle_client(connect);});
                }
                Err(e) => {
                    println!("Error connecting to client: {:?}", e);
                }
            }
        }
    }           
}
mod echo_client {
    pub fn run<T: net::ToSocketAddrs>(addr: T) -> ! {
        let sock = TcpStream::connect(addr).unwrap();
        let mut buf = String::new();
        while(io::stdin().read_line(&mut buf).is_ok()){
            match io::stdout().write_all(buf.as_bytes()) {
                Ok(_) => _;
                Err(e) => break;
            }
        }
    }
}
            
        

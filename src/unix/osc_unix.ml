module IO = struct
  type 'a t = 'a

  let ( >>= ) x f = f x
  let ( >|= ) x f = f x
  let ( let* ) = ( >>= )
  let return x = x
end

module UdpTransport = struct
  module IO = IO

  type sockaddr = Unix.sockaddr

  module Client = struct
    type t = Client of Unix.file_descr

    let create () =
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_DGRAM (Unix.getprotobyname "udp").Unix.p_proto
      in
      Client socket
    ;;

    let destroy (Client fd) = Unix.close fd

    let send_string (Client fd) addr data =
      let length = String.length data in
      let sent = Unix.sendto fd (Bytes.unsafe_of_string data) 0 length [] addr in
      if sent <> length then failwith "IO error"
    ;;
  end

  module Server = struct
    type t =
      { buffer_length : int
      ; buffer : bytes
      ; socket : Unix.file_descr
      }

    let create addr buffer_length =
      let buffer = Bytes.create buffer_length in
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_DGRAM (Unix.getprotobyname "udp").Unix.p_proto
      in
      Unix.bind socket addr;
      { buffer_length; buffer; socket }
    ;;

    let destroy server = Unix.close server.socket

    let recv_string server =
      match Unix.recvfrom server.socket server.buffer 0 server.buffer_length [] with
      | length, sockaddr ->
        Bytes.unsafe_to_string (Bytes.sub server.buffer 0 length), sockaddr
    ;;
  end
end

module Udp = Osc.Transport.Make (UdpTransport)

module TcpTransport = struct
  module IO = IO

  type sockaddr = Unix.sockaddr

  module Client = struct
    type t = Client of Unix.file_descr

    let create () =
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Unix.getprotobyname "tcp").Unix.p_proto
      in
      Client socket
    ;;

    let destroy (Client fd) = Unix.close fd

    let send_string (Client fd) addr data =
      let length = String.length data in
      let sent = Unix.sendto fd (Bytes.unsafe_of_string data) 0 length [] addr in
      if sent <> length then failwith "Partial write"
    ;;
  end

  module Server = struct
    type t =
      { buffer_length : int
      ; buffer : bytes
      ; socket : Unix.file_descr
      }

    let create addr buffer_length =
      let buffer = Bytes.create buffer_length in
      let socket =
        Unix.socket Unix.PF_INET Unix.SOCK_DGRAM (Unix.getprotobyname "udp").Unix.p_proto
      in
      Unix.bind socket addr;
      { buffer_length; buffer; socket }
    ;;

    let destroy server = Unix.close server.socket

    let recv_string server =
      let len, sockaddr =
        Unix.recvfrom server.socket server.buffer 0 server.buffer_length []
      in
      Bytes.unsafe_to_string (Bytes.sub server.buffer 0 len), sockaddr
    ;;
  end
end

module Tcp = Osc.Transport.Make (TcpTransport)

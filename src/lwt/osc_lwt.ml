module IO = struct
  type 'a t = 'a Lwt.t

  let ( >>= ) = Lwt.( >>= )
  let ( >|= ) = Lwt.( >|= )
  let ( let* ) = ( >>= )
  let return = Lwt.return
end

module UdpTransport = struct
  module IO = IO
  open IO

  type sockaddr = Lwt_unix.sockaddr

  module Client = struct
    type t = Client of Lwt_unix.file_descr

    let create () =
      let socket =
        Lwt_unix.socket
          Lwt_unix.PF_INET
          Lwt_unix.SOCK_DGRAM
          (Unix.getprotobyname "udp").Unix.p_proto
      in
      return @@ Client socket
    ;;

    let destroy (Client fd) = Lwt_unix.close fd

    let send_string (Client fd) addr data =
      let length = String.length data in
      let* bytes_sent =
        Lwt_unix.sendto fd (Bytes.unsafe_of_string data) 0 length [] addr
      in
      if bytes_sent <> length then Lwt.fail (Failure "IO error") else return ()
    ;;
  end

  module Server = struct
    type t =
      { buffer_length : int
      ; buffer : bytes
      ; socket : Lwt_unix.file_descr
      }

    let create addr buffer_length =
      let buffer = Bytes.create buffer_length in
      Lwt_unix.getprotobyname "udp"
      >>= fun proto ->
      let socket =
        Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM proto.Lwt_unix.p_proto
      in
      Lwt_unix.bind socket addr >>= fun () -> return { buffer_length; buffer; socket }
    ;;

    let destroy server = Lwt_unix.close server.socket

    let recv_string server =
      Lwt_unix.recvfrom server.socket server.buffer 0 server.buffer_length []
      >>= fun (length, sockaddr) ->
      return (Bytes.unsafe_to_string (Bytes.sub server.buffer 0 length), sockaddr)
    ;;
  end
end

module Udp = Osc.Transport.Make (UdpTransport)

module TcpTransport = struct
  module IO = IO
  open IO

  type sockaddr = Lwt_unix.sockaddr

  module Client = struct
    type t = Client of Lwt_unix.file_descr

    let create () =
      let socket =
        Lwt_unix.socket
          Lwt_unix.PF_INET
          Lwt_unix.SOCK_STREAM
          (Unix.getprotobyname "tcp").Unix.p_proto
      in
      return @@ Client socket
    ;;

    let destroy (Client fd) = Lwt_unix.close fd

    let send_string (Client fd) addr data =
      let length = String.length data in
      let* sent = Lwt_unix.sendto fd (Bytes.unsafe_of_string data) 0 length [] addr in
      if sent <> length then Lwt.fail (Failure "Partial write") else return ()
    ;;
  end

  module Server = struct
    type t =
      { buffer_length : int
      ; buffer : bytes
      ; socket : Lwt_unix.file_descr
      }

    let create addr buffer_length =
      let buffer = Bytes.create buffer_length in
      let* protocol_entry = Lwt_unix.getprotobyname "tcp" in
      let socket =
        Lwt_unix.socket
          Lwt_unix.PF_INET
          Lwt_unix.SOCK_STREAM
          protocol_entry.Lwt_unix.p_proto
      in
      let* _ = Lwt_unix.bind socket addr in
      return { buffer_length; buffer; socket }
    ;;

    let destroy server = Lwt_unix.close server.socket

    let recv_string server =
      let* len, sockaddr =
        Lwt_unix.recvfrom server.socket server.buffer 0 server.buffer_length []
      in
      return (Bytes.unsafe_to_string (Bytes.sub server.buffer 0 len), sockaddr)
    ;;
  end
end

module Tcp = Osc.Transport.Make (TcpTransport)

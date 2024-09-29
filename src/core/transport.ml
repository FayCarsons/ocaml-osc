module type TRANSPORT = sig
  module IO : sig
    type 'a t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end

  type sockaddr

  module Client : sig
    type t

    val create : unit -> t IO.t
    val destroy : t -> unit IO.t
    val send_string : t -> sockaddr -> string -> unit IO.t
  end

  module Server : sig
    type t

    val create : sockaddr -> int -> t IO.t
    val destroy : t -> unit IO.t
    val recv_string : t -> (string * sockaddr) IO.t
  end
end

module Make (T : TRANSPORT) = struct
  open T.IO

  module Client = struct
    type t = T.Client.t

    let create = T.Client.create
    let destroy = T.Client.destroy

    let send client sockaddr packet =
      let data = Codec.of_packet packet in
      T.Client.send_string client sockaddr data
    ;;
  end

  module Server = struct
    type t = T.Server.t

    let create = T.Server.create
    let destroy = T.Server.destroy

    let recv server =
      T.Server.recv_string server
      >|= fun (data, addr) ->
      Result.map (fun packet -> packet, addr) (Codec.to_packet data)
    ;;
  end
end

type typetag_error =
  | Unsupported_typetag of char
  | Missing_typetag_string

module Udp : sig
  module Client : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val send : t -> Unix.sockaddr -> Osc.Types.packet -> unit
  end

  module Server : sig
    type t

    val create : Unix.sockaddr -> int -> t
    val destroy : t -> unit
    val recv : t -> (Osc.Types.packet * Unix.sockaddr, typetag_error) Result.t
  end
end

module Tcp : sig
  module Client : sig
    type t

    val create : unit -> t
    val destroy : t -> unit
    val send : t -> Unix.sockaddr -> Osc.Types.packet -> unit
  end

  module Server : sig
    type t

    val create : Unix.sockaddr -> int -> t
    val destroy : t -> unit
    val recv : t -> (Osc.Types.packet * Unix.sockaddr, typetag_error) Result.t
  end
end

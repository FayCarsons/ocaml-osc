type typetag_error =
  | Unsupported_typetag of char
  | Missing_typetag_string

module Udp : sig
  module Client : sig
    type t

    val create : unit -> t Lwt.t
    val destroy : t -> unit Lwt.t
    val send : t -> Lwt_unix.sockaddr -> Osc.Types.packet -> unit Lwt.t
  end

  module Server : sig
    type t

    val create : Lwt_unix.sockaddr -> int -> t Lwt.t
    val destroy : t -> unit Lwt.t
    val recv : t -> (Osc.Types.packet * Lwt_unix.sockaddr, typetag_error) result Lwt.t
  end
end

module Tcp : sig
  module Client : sig
    type t

    val create : unit -> t Lwt.t
    val destroy : t -> unit Lwt.t
    val send : t -> Lwt_unix.sockaddr -> Osc.Types.packet -> unit Lwt.t
  end

  module Server : sig
    type t

    val create : Lwt_unix.sockaddr -> int -> t Lwt.t
    val destroy : t -> unit Lwt.t
    val recv : t -> (Osc.Types.packet * Lwt_unix.sockaddr, typetag_error) result Lwt.t
  end
end


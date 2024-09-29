(** IO-independent code for handling OSC packets and bundles. *)

module Types : sig
  (** Types representing OSC packets. *)

  (** An NTP-style timestamp. *)
  type time =
    { seconds : int32 (** Number of seconds since midnight on January 1st, 1900. *)
    ; fraction : int32 (** Fractional part of the timestamp. *)
    }

  (** An OSC time tag. *)
  type timetag =
    | Immediate
    (** A special time tag value, indicating that associated OSC methods should
        be invoked immediately. *)
    | Time of time (** A time at which an OSC method should be invoked. *)

  (** An argument contained by an OSC message. *)
  type argument =
    | Float32 of float (** 32-bit float argument. *)
    | Int32 of int32 (** 32-bit integer argument. *)
    | String of string (** String argument. *)
    | Blob of string (** Binary blob argument. *)
    | Timetag of timetag (** Timetag argument. *)

  (** An OSC message. *)
  type message =
    { address : string (** The OSC address pattern to which the message is aimed. *)
    ; arguments : argument list (** The arguments associated with the message. *)
    }

  (** An OSC bundle. *)
  type bundle =
    { timetag : timetag
    (** A timetag, representing the time at which any actions triggered by this
        bundle should begin. *)
    ; packets : packet list (** The list of packets contained within this bundle. *)
    }

  (** An OSC packet. *)
  and packet =
    | Message of message (** A single OSC message. *)
    | Bundle of bundle
    (** An OSC bundle, containing a timetag and zero or more child packets. *)
end

module Codec : sig
  (** Conversion of OSC packets to and from strings. *)

  (** Serialise an OSC packet into a string. *)
  val of_packet : Types.packet -> string

  (** Attempt to deserialise a string into an OSC packet. *)
  val to_packet
    :  string
    -> (Types.packet, [ `Missing_typetag_string | `Unsupported_typetag of char ]) Result.t
end

module Transport : sig
  (** Generic OSC transport library, functorised over the
      {{:Transport.TRANSPORT.html} TRANSPORT} module. *)

  (** Type of modules which can be used to create an OSC library, via the
      {{:Osc.Transport.Make.html} Osc.Transport.Make} functor. *)
  module type TRANSPORT = sig
    module IO : sig
      type 'a t

      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
      val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
      val return : 'a -> 'a t
    end

    (** A generic socket type. *)
    type sockaddr

    (** Types and functions for handling clients for this type of transport. *)
    module Client : sig
      (** A type encapsulating a client for this type of transport, and its
          state. *)
      type t

      (** Create a new transport client. *)
      val create : unit -> t IO.t

      (** Destroy a transport client. *)
      val destroy : t -> unit IO.t

      (** [send_string client addr data] uses [client] to send [data] to an OSC
          server listening on address [sockaddr]. *)
      val send_string : t -> sockaddr -> string -> unit IO.t
    end

    (** Types and functions for handling servers for this type of transport. *)
    module Server : sig
      (** A type encapsulating a server for this type of transport, and its
          state. *)
      type t

      (** [create addr buffer_length] creates a new server listening on
          [sockaddr]. The server will be able to receive messages of length up
          to [buffer_length]. *)
      val create : sockaddr -> int -> t IO.t

      (** Destroy a transport server. *)
      val destroy : t -> unit IO.t

      (** Retrieve data sent to the server, as well as the sending
          client's address. *)
      val recv_string : t -> (string * sockaddr) IO.t
    end
  end

  (** Create OSC client/server modules from a module of type TRANSPORT. *)
  module Make : functor (T : TRANSPORT) -> sig
    (** Types and functions for handling OSC clients. *)
    module Client : sig
      (** An OSC client. *)
      type t

      (** Create a new OSC client. *)
      val create : unit -> t T.IO.t

      (** Destroy an OSC client. *)
      val destroy : t -> unit T.IO.t

      (** [send client addr packet] uses [client] to send OSC packet [packet] to
          a server listening at address [addr]. *)
      val send : t -> T.sockaddr -> Types.packet -> unit T.IO.t
    end

    (** Types and functions for handling OSC servers. *)
    module Server : sig
      (** An OSC server. *)
      type t

      (** [create addr buffer_length] creates a new server listening on
          [sockaddr]. The server will be able to receive messages of length up
          to [buffer_length]. *)
      val create : T.sockaddr -> int -> t T.IO.t

      (** Destroy an OSC server. *)
      val destroy : t -> unit T.IO.t

      (** Retrieve a packet sent to the server, as well as the sending client's
          address. *)
      val recv
        :  t
        -> ( Types.packet * T.sockaddr
             , [ `Missing_typetag_string | `Unsupported_typetag of char ] )
             Result.t
             T.IO.t
    end
  end
end

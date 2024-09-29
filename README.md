ocaml-osc
=========

[![Build status](https://github.com/johnelse/ocaml-osc/actions/workflows/workflow.yml/badge.svg)](https://github.com/johnelse/ocaml-osc/actions)

Pure OCaml implementation of the
[Open Sound Control](https://opensoundcontrol.stanford.edu/) protocol.

Dependencies:

* [lwt](http://ocsigen.org/lwt/) (optional)
* [ocplib-endian](https://github.com/OCamlPro/ocplib-endian)

Usage
=====

If you just need to parse and serialise OSC packets, use the `Osc.Codec`
module:

``` ocaml
# require "osc";;

# open Osc.Types;;

# let data = Osc.Codec.of_packet (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]});;
val data : bytes = "/hello/world\000\000\000\000,is\000\000\000\000{foo\000"

# let packet = Osc.Codec.to_packet data;;
val packet : (packet, [ `Missing_typetag_string | `Unsupported_typetag of char ]) Rresult.result =
  Rresult.Ok (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]})
```

To simplify sending and receiving OSC packets over the network, the modules
`Osc_lwt` and `Osc_unix` are available:

``` ocaml
# require "osc.lwt";;

# require "lwt.syntax";;

# open Osc.Types;;

# lwt client = Osc_lwt.Udp.Client.create ();;
val client : Osc_lwt.Udp.Client.t = <abstr>

# let addr = Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 57120);;
val addr : Lwt_unix.sockaddr = Unix.ADDR_INET (<abstr>, 57120)

# lwt () = Osc_lwt.Udp.Client.send client addr (Message {address = "/hello/world"; arguments = [Int32 123l; String "foo"]});;
```

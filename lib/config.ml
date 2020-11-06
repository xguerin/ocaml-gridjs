(*
 * Copyright (c) 2020 Xavier R. Guérin <copyright@applepine.org>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Js_of_ocaml

module Column = struct
  type arg

  class type ['selector] t =
    object
      method id : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method selector : 'selector Js.callback Js.Optdef.t Js.readonly_prop

      method name : Js.js_string Js.t Js.readonly_prop

      method width : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method sort : bool Js.t Js.Optdef.t Js.readonly_prop

      method hidden : bool Js.t Js.Optdef.t Js.readonly_prop

      method formatter :
        (arg Js.t -> Js.js_string Js.t) Js.callback Js.Optdef.t Js.readonly_prop
    end

  let make ?id ?selector ?width ?sort ?hidden ?formatter name : 'selector t Js.t
      =
    let ( >>= ) e fn = Option.map fn e in
    object%js
      val id = Js.Optdef.option (id >>= Js.string)

      val selector = Js.Optdef.option (selector >>= Js.wrap_callback)

      val name = Js.string name

      val width = Js.Optdef.option (width >>= Js.string)

      val sort = Js.Optdef.option (sort >>= Js.bool)

      val hidden = Js.Optdef.option (hidden >>= Js.bool)

      val formatter = Js.Optdef.option (formatter >>= Js.wrap_callback)
    end
end

module ClassName = struct
  let concat lst = Some (String.concat " " lst)
  let str s = Some (Js.string s)

  module Header = struct
    class type t =
      object
        method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method search : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
      end

    let make ?container ?search () : t Js.t =
      let ( >>= ) = Option.bind in
      object%js
        val container = Js.Optdef.option (container >>= concat >>= str)

        val search = Js.Optdef.option (search >>= concat >>= str)
      end
  end

  module Footer = struct
    module Pagination = struct
      module Buttons = struct
        class type t =
          object
            method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

            method default : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

            method current : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

            method prev : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

            method next : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
          end

        let make ?container ?default ?current ?prev ?next () : t Js.t =
          let ( >>= ) = Option.bind in
          object%js
            val container = Js.Optdef.option (container >>= concat >>= str)

            val default = Js.Optdef.option (default >>= concat >>= str)

            val current = Js.Optdef.option (current >>= concat >>= str)

            val prev = Js.Optdef.option (prev >>= concat >>= str)

            val next = Js.Optdef.option (next >>= concat >>= str)
          end
      end

      class type t =
        object
          method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

          method summary : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

          method buttons : Buttons.t Js.t Js.Optdef.t Js.readonly_prop
        end

      let make ?container ?summary ?buttons () : t Js.t =
        let ( >>= ) = Option.bind in
        object%js
          val container = Js.Optdef.option (container >>= concat >>= str)

          val summary = Js.Optdef.option (summary >>= concat >>= str)

          val buttons = Js.Optdef.option buttons
        end
    end

    class type t =
      object
        method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method pagination : Pagination.t Js.t Js.Optdef.t Js.readonly_prop
      end

    let make ?container ?pagination () : t Js.t =
      let ( >>= ) = Option.bind in
      object%js
        val container = Js.Optdef.option (container >>= concat >>= str)

        val pagination = Js.Optdef.option pagination
      end
  end

  module Table = struct
    module State = struct
      class type t =
        object
          method loading : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

          method notfound : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

          method error : Js.js_string Js.t Js.Optdef.t Js.readonly_prop
        end

      let make ?loading ?notfound ?error () : t Js.t =
        let ( >>= ) = Option.bind in
        object%js
          val loading = Js.Optdef.option (loading >>= concat >>= str)

          val notfound = Js.Optdef.option (notfound >>= concat >>= str)

          val error = Js.Optdef.option (error >>= concat >>= str)
        end
    end

    class type t =
      object
        method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method sort : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method td : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method th : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method thead : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

        method state : State.t Js.t Js.Optdef.t Js.readonly_prop
      end

    let make ?container ?sort ?td ?th ?thead ?state () : t Js.t =
      let ( >>= ) = Option.bind in
      object%js
        val container = Js.Optdef.option (container >>= concat >>= str)

        val sort = Js.Optdef.option (sort >>= concat >>= str)

        val td = Js.Optdef.option (td >>= concat >>= str)

        val th = Js.Optdef.option (th >>= concat >>= str)

        val thead = Js.Optdef.option (thead >>= concat >>= str)

        val state = Js.Optdef.option state
      end
  end

  class type t =
    object
      method container : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method table : Table.t Js.t Js.Optdef.t Js.readonly_prop

      method header : Header.t Js.t Js.Optdef.t Js.readonly_prop

      method footer : Footer.t Js.t Js.Optdef.t Js.readonly_prop
    end

  let make ?container ?table ?header ?footer () : t Js.t =
    let ( >>= ) = Option.bind in
    object%js
      val container = Js.Optdef.option (container >>= concat >>= str)

      val table = Js.Optdef.option table

      val header = Js.Optdef.option header

      val footer = Js.Optdef.option footer
    end
end

module Server = struct
  class type ['data] t =
    object
      method url : Js.js_string Js.t Js.readonly_prop

      method method_ : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method then_ : 'data Js.callback Js.Optdef.t Js.readonly_prop
    end

  let make ?method_ ?then_ url : 'data t Js.t =
    let ( >>= ) e fn = Option.map fn e in
    object%js
      val url = Js.string url

      val method_ = Js.Optdef.option (method_ >>= Js.string)

      val then_ = Js.Optdef.option (then_ >>= Js.wrap_callback)
    end
end

module Pagination = struct
  class type ['server] t =
    object
      method enabled : bool Js.t Js.readonly_prop

      method limit : int Js.Optdef.t Js.readonly_prop

      method page : int Js.Optdef.t Js.readonly_prop

      method summary : bool Js.t Js.Optdef.t Js.readonly_prop

      method nextButton : bool Js.t Js.Optdef.t Js.readonly_prop

      method prevButton : bool Js.t Js.Optdef.t Js.readonly_prop

      method buttonsCount : int Js.Optdef.t Js.readonly_prop

      method resetPageOnUpdate : bool Js.t Js.Optdef.t Js.readonly_prop

      method server : 'server Server.t Js.t Js.Optdef.t Js.readonly_prop
    end

  let make ?limit ?page ?summary ?nextButton ?prevButton ?buttonsCount
      ?resetPageOnUpdate ?server enabled : 'server t Js.t =
    let ( >>= ) e fn = Option.map fn e in
    object%js
      val enabled = Js.bool enabled

      val limit = Js.Optdef.option limit

      val page = Js.Optdef.option page

      val summary = Js.Optdef.option (summary >>= Js.bool)

      val nextButton = Js.Optdef.option (nextButton >>= Js.bool)

      val prevButton = Js.Optdef.option (prevButton >>= Js.bool)

      val buttonsCount = Js.Optdef.option buttonsCount

      val resetPageOnUpdate = Js.Optdef.option (resetPageOnUpdate >>= Js.bool)

      val server = Js.Optdef.option server
    end
end

module Sort = struct
  class type ['server] t =
    object
      method enabled : bool Js.t Js.readonly_prop

      method multiColumn : bool Js.t Js.Optdef.t Js.readonly_prop

      method server : 'server Server.t Js.t Js.Optdef.t Js.readonly_prop
    end

  let make ?multiColumn ?server enabled : 'server t Js.t =
    object%js
      val enabled = Js.bool enabled

      val multiColumn = Js.Optdef.option multiColumn

      val server = Js.Optdef.option server
    end
end

module Search = struct
  class type ['server] t =
    object
      method enabled : bool Js.t Js.readonly_prop

      method keyword : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

      method debounce_timeout : int Js.Optdef.t Js.readonly_prop

      method server : 'server Server.t Js.t Js.Optdef.t Js.readonly_prop
    end

  let make ?keyword ?debounce_timeout ?server enabled : 'server t Js.t =
    let ( >>= ) e fn = Option.map fn e in
    object%js
      val enabled = Js.bool enabled

      val keyword = Js.Optdef.option (keyword >>= Js.string)

      val debounce_timeout = Js.Optdef.option debounce_timeout

      val server = Js.Optdef.option server
    end
end

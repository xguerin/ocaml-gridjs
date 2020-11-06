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

module Options = struct
  class type ['dat, 'sel, 'srv] t =
    object
      method data : 'dat Js.callback Js.Optdef.t Js.readonly_prop

      method from : Dom_html.element Js.t Js.Optdef.t Js.readonly_prop

      method columns :
        'sel Config.Column.t Js.t Js.js_array Js.t Js.Optdef.t Js.readonly_prop

      method server : 'dat Config.Server.t Js.t Js.Optdef.t Js.readonly_prop

      method className : Config.ClassName.t Js.t Js.Opt.t Js.readonly_prop

      method width : Js.js_string Js.t Js.readonly_prop

      method height : Js.js_string Js.t Js.readonly_prop

      method autoWidth : bool Js.t Js.readonly_prop

      method fixedHeader : bool Js.t Js.Optdef.t Js.readonly_prop

      method search : 'srv Config.Search.t Js.t Js.Optdef.t Js.readonly_prop

      method sort : 'srv Config.Sort.t Js.t Js.Optdef.t Js.readonly_prop

      method pagination :
        'srv Config.Pagination.t Js.t Js.Optdef.t Js.readonly_prop
    end

  let make ?data ?from ?columns ?server ?className ?(width = "100%")
      ?(height = "auto") ?(autoWidth = Js.bool true) ?fixedHeader ?search ?sort
      ?pagination () : ('dat, 'sel, 'srv) t Js.t =
    let ( >>= ) e fn = Option.map fn e in
    object%js
      val data = Js.Optdef.option (data >>= Js.wrap_callback)

      val from = Js.Optdef.option from

      val columns = Js.Optdef.option (columns >>= Js.array)

      val server = Js.Optdef.option server

      val className = Js.Opt.option className

      val width = Js.string width

      val height = Js.string height

      val autoWidth = autoWidth

      val fixedHeader = Js.Optdef.option fixedHeader

      val search = Js.Optdef.option search

      val sort = Js.Optdef.option sort

      val pagination = Js.Optdef.option pagination
    end
end

class type ['dat, 'sel, 'srv] t =
  object
    method on :
         Js.js_string Js.t
      -> (('dat, 'sel, 'srv) t Js.t, 'a Js.t -> unit) Js.meth_callback
      -> unit Js.meth

    method render : Dom_html.element Js.t -> unit Js.meth

    method updateConfig : ('dat, 'sel, 'srv) Options.t Js.t -> unit Js.meth

    method forceRender : unit Js.meth
  end

let t = Js.Unsafe.global##.gridjs##._Grid

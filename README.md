# Grid.js bindings for JSoO

Does as much as it says in the title. [Documentation](https://gridjs.io/docs/index) for the `gridjs` project.
Currently requires the modified version of `gridjs` located [here](https://github.com/xguerin/gridjs). [Here](https://github.com/grid-js/gridjs/pull/330) is the
related PR.

## Example
```ocaml
open Js_of_ocaml
open Gridjs

let options ()=
  Grid.Options.make
    ~columns:
    [| Config.Column.make "First_Name"
     ; Config.Column.make "Last_Name"
     ; Config.Column.make "Email" |]
    ~server:(Config.Server.make ~then_:(fun a -> a) "/some/data.json")
    ~className:(Config.ClassName.make ())
    ~pagination:(Config.Pagination.make ~limit:5 true)
    ~search:(Config.Search.make true) () in

let _ =
  let target = Dom_html.document##getElementById (Js.string "target") in
  Js.Opt.case target
    (fun () -> ())
    (fun target ->
      let grid : _ Grid.t Js.t = new%js Grid.t options in
      grid##render target)
```

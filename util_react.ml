open Reactjs
open Reactjs.Infix

let node_key tag class_name key children =
    DOM.make
        ~elem_spec: (object%js val key = !* key end)
        ~tag: tag
        ~class_name: class_name children

let el_key tag class_name key children = Elem (node_key tag class_name key children)

let node tag class_name children =
    DOM.make
        ~tag: tag
        ~class_name: class_name children

let el tag class_name children = Elem (node tag class_name children)
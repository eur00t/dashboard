open Core.Std;;

let result_to_or_error value =
    let module R = Ppx_deriving_yojson_runtime.Result in
    match value with
        | R.Ok value -> Ok value
        | R.Error str -> Or_error.error_string str;;
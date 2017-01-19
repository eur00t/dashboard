(*open Api_types;;

let v = {
    str = "dddd11"
};;

Format.printf "%s" (Yojson.Safe.to_string (x_to_yojson v));;*)

let get_websocket url =
    let url_js = Js.string url in
    new%js WebSockets.webSocket url_js;;

let ws = get_websocket "ws://localhost:8081/api";;

let send_message ws msg =
    ws##send (Js.string (Api_types.raw_msg_from_client msg));;

ws##.onopen := Dom.handler begin
    fun e ->
        let open Api_types in
        send_message ws (Initial_request 1);
        Firebug.console##log (Js.string "Connected");
        Js._true
end;;

ws##.onmessage := Dom.handler begin
    fun e ->
        let open Api_types in
        let data = Js.to_string e##.data in
        Api_types.client_handler data ~handler: begin
            function
                | Initial_response { Initial_response.test } ->
                    Firebug.console##log (Js.string ("Initial_response: " ^ (string_of_int test)));
        end;
        Js._true
end;;
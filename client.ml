let get_websocket url =
    let url_js = Js.string url in
    new%js WebSockets.webSocket url_js;;

let ws = get_websocket "ws://localhost:8081/api";;

ws##.onopen := Dom.handler begin
    fun e ->
        Dispatcher_client.Client.initial_request Dispatcher_client.client ws;
        Firebug.console##log (Js.string "Connected");
        Js._true
end;;

ws##.onmessage := Dom.handler begin
    fun e ->
        Firebug.console##log e##.data;
        let open Api_types in
        let data = Js.to_string e##.data in
        Api_types.client_handler data ~handler: (Dispatcher_client.Client.process_server_payload Dispatcher_client.client ws);
        Js._true
end;;
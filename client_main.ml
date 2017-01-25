let get_websocket url =
    let url_js = Js.string url in
    new%js WebSockets.webSocket url_js;;

let retry_initial_s = 5

let attach_handlers ws retry_s connect =
    ws##.onopen := Dom.handler begin
        fun e ->
            Dispatcher_client.Client.initial_request Dispatcher_client.client ws;
            Firebug.console##log (Js.string "Connected");
            Js._true
    end;

    ws##.onclose := Dom.handler begin
        fun e ->
            Firebug.console##log (Js.string ("Disconnected, retrying in " ^ (string_of_int retry_s) ^ "s"));
            connect retry_s;
            Js._true
    end;

    ws##.onmessage := Dom.handler begin
        fun e ->
            Firebug.console##log e##.data;
            let open Api_types in
            let data = Js.to_string e##.data in
            Api_types.client_handler data ~handler: (Dispatcher_client.Client.process_server_payload Dispatcher_client.client ws);
            Js._true
    end

let rec connect retry_s =
    let ws = get_websocket "ws://192.168.1.157:8081/api" in
    attach_handlers ws retry_s connect

let () =
    connect retry_initial_s;

    Dispatcher_client.Client.render
        Dispatcher_client.client
        "container"
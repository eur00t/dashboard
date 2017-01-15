open Core.Std;;
open Async.Std;;

module type Api_module = sig
    val do_request: string ->
        (string * string) list ->
        (Yojson.Safe.json, Error.t) Result.t
        Async.Std.Deferred.t

    val do_request_inner: ?add_params:(Uri.t -> Uri.t) ->
        string -> (string * string) list -> Yojson.Safe.json Async.Std.Deferred.t
end;;

type api_config = {
    access_token: string;
    api_version: string;
    api_url: string
} [@@deriving of_yojson];;

module type Api_config_module = sig
    val access_token: string
    val api_version: string
    val api_url: string
end;;

let make_config_module { access_token; api_version; api_url } =
    (module struct
        let access_token = access_token
        let api_version = api_version
        let api_url = api_url
    end: Api_config_module);;

module Make_api (Config: Api_config_module): Api_module = struct
    type api_error = {
        error_code: int;
        error_msg: string
    } [@@deriving of_yojson { strict = false }]

    type api_response = {
        error: api_error option [@default None];
        response: Yojson.Safe.json option [@default None]
    } [@@deriving of_yojson { strict = false }]

    let rec add_query_params params uri =
        match params with
            | (key, value)::l -> add_query_params l (Uri.add_query_param uri (key, [value]))
            | [] -> uri;;

    let add_base_query_params uri =
        add_query_params [("access_token", Config.access_token); ("v", Config.api_version)] uri;;

    let construct_error_str { error_code; error_msg } =
        sprintf "Code: %d\nMessage: %s\n" error_code error_msg;;

    let process_json_response json =
        match api_response_of_yojson json with
            | Ok { error = Some error } -> Or_error.error_string (construct_error_str error)
            | Ok { error = None; response = Some response } -> Ok response
            | Error err -> Or_error.error_string ("Wrong API response: " ^ err)
            | Ok { error = None; response = None } -> Or_error.error_string "Wrong API response";;

    let rec do_request_raw ?add_params ?(retry_after=2.0) url additional_params =
        let url_ = Uri.of_string (url)
            |> (match add_params with | Some funct -> funct | None -> ident)
            |> add_query_params additional_params in
        (*printf "Doing request: %s\n" (Uri.to_string url);*)
        Cohttp_async.Client.get url_
        >>= fun (response, body) ->
            match Cohttp.Code.code_of_status (Cohttp_async.Response.status response) with
                | 200 -> return (response, body)
                | _ ->
                    printf "Error with request: %s\nRetry in: %fs\n" (Uri.to_string url_) retry_after;
                    after (Time.Span.of_sec retry_after)
                    >>= fun () ->
                    do_request_raw
                        ?add_params
                        ~retry_after:(retry_after *. 2.0)
                        url
                        additional_params;;

    let do_request_inner ?add_params url additional_params =
        do_request_raw ?add_params url additional_params
        >>= fun (_, body) -> Cohttp_async.Body.to_string body
        >>= fun str -> return (Yojson.Safe.from_string str)

    let do_request methodName additional_params =
        do_request_inner (Config.api_url ^ methodName) additional_params
            ~add_params:add_base_query_params
        >>| fun json -> process_json_response json;;
end;;

let get_api_config_from_file file_name =
    let config_file = In_channel.create file_name in
    let config_str = In_channel.input_all config_file in
    api_config_of_yojson (Yojson.Safe.from_string config_str);;

include Make_api(
    (val (make_config_module (match get_api_config_from_file "config.json" with
        | Ok v -> v
        | Error _ -> failwith "Can't read config.json")))
);;
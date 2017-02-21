type report_config = {
    report_email: string;
} [@@deriving of_yojson { strict = false }];;

let get_report_config_from_file file_name =
    let config_file = Core.Std.In_channel.create file_name in
    let config_str = Core.Std.In_channel.input_all config_file in
    report_config_of_yojson (Yojson.Safe.from_string config_str);;

let report_config = match get_report_config_from_file "config.json" with
    | Ok v -> v
    | Error _ -> failwith "Can't read config.json"

let send_mail subj message =
    let command = Printf.sprintf "echo \"%s\" | mail -s \"%s\" %s"
        message
        subj
        report_config.report_email in
    Sys.command command;;

let report type_ message = send_mail ("Error message: " ^ type_) message
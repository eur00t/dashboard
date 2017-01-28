open Util_react
open Reactjs
open Reactjs.Infix;;

Js.Unsafe.global##.Highcharts##setOptions (object%js
    val global = (object%js
        val useUTC = Js._false
    end)
end)

let get_current_time_plot_line () = object%js
    val color = !* "red"
    val value = new%js Js.date_now
    val width = 1
    val zIndex = 5
    val id = !* "current"
    val label = (object%js
        val text = !* "now"
    end)
end

let update_serie chart data interval_s =
    let update = (object%js
        val xAxis = Js.Unsafe.obj (Array.of_list [
            ("plotLines", Js.Unsafe.inject (Js.array (Array.of_list [
                get_current_time_plot_line ()
            ])))
        ])
        val series = Js.array (Array.of_list [(object%js
            val data = data
        end)])
    end) in
    chart##update update Js._false;
    let serie_opt = Js.array_get chart##.series 0 in
    Js.Optdef.bind serie_opt (fun serie ->
        serie##addPoint (object%js
            val x = (new%js Js.date_now)##valueOf +. ((Js.float_of_number interval_s +. 1.) *. 1000.)
            val y = 0
            val id = !* "guide"
        end);
        Js.Optdef.return ())

let set_update_timer this =
    this##.update_cur_timer = Js.Unsafe.global##setInterval (Js.wrap_callback (fun () ->
        update_serie this##.chart this##.props##.data this##.props##.interval_s
    )) 10000

let clear_update_timer this =
    Js.Unsafe.global##clearInterval this##.update_cur_timer

let chart_factory = (Reactjs.make_class_spec
    ~component_did_mount: (fun ~this ->
        let config = (object%js
            val chart = Js.Unsafe.obj (Array.of_list [
                ("type", Js.Unsafe.inject !* "area");
                ("zoomType", Js.Unsafe.inject !* "x")
            ])

            val xAxis = Js.Unsafe.obj (Array.of_list [
                ("type", Js.Unsafe.inject !* "datetime");
                ("tickPixelInterval", Js.Unsafe.inject (Js.number_of_float 150.));
                ("plotLines", Js.Unsafe.inject (Js.array (Array.of_list [
                    get_current_time_plot_line ()
                ])))
            ])

            val yAxis = (object%js
                val title = (object%js
                    val text = !* "messages"
                end)

                val allowDecimals = Js._false
            end)

            val series = Js.array (Array.of_list [(object%js
                val data = this##.props##.data
                val name = !* "messages"
            end)])

            val plotOptions = (object%js
                val series = (object%js
                    val step = !* "left"
                end)
                val area = (object%js
                    val lineWidth = 1
                    val color = !* "#000"
                    val fillColor = !* "#ddd"
                    val states = (object%js
                        val hover = (object%js
                            val lineWidth = 1
                        end)
                    end)
                end)
                val marker = (object%js
                    val radius = 1
                end)
            end)

            val legend = (object%js
                val enabled = Js._false
            end)

            val exporting = (object%js
                val enabled = Js._false
            end)

            val title = (object%js
                val text = !* ""
            end)

            val tooltip = (object%js
                val formatter = Js.wrap_meth_callback (fun this -> (fun () ->
                    let date_str = Js.Unsafe.global##.Highcharts##dateFormat !* "%H:%M:%S" this##.x in
                    Js.string (
                        (Js.to_string date_str) ^ "<br/>" ^
                        (Printf.sprintf "%d" (int_of_float (Js.float_of_number this##.y)))
                    )
                ))
            end)
        end) in
        this##.chart := Js.Unsafe.global##.Highcharts##chart
            this##.refs##.chart
            config;

        ignore (set_update_timer this);
        ()
    )
    ~component_will_unmount: (fun ~this ->
        ignore (clear_update_timer this);
        ()
    )
    ~component_did_update: (fun ~this ~prev_prop ~prev_state ->
        update_serie this##.chart this##.props##.data this##.props##.interval_s;
        ()
    )
    (fun ~this ->
        DOM.make
            ~elem_spec: (object%js val ref = !* "chart" end)
            ~tag: `div
            ~class_name: "chart" [
        ]
    ))
    |> create_class
    |> create_factory

module Core = struct
    include Frequency_processor.Core

    let render ?title client_state ~config =
        let data = (List.map (fun (time, count) ->
            object%js
                val x = new%js Js.date_fromTimeValue ((float_of_int time) *. 1000.)
                val y = Js.number_of_float (float_of_int count)
            end
        ) client_state)
        |> Array.of_list
        |> Js.array in

        node `div "processor-frequency" [
            Elem (chart_factory ~props: (object%js
                val data = data
                val interval_s = config.interval_s
            end))
        ]
end

include Message_processor_client.Make_processor(Core)
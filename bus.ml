type 'a t = {
    mutable next_id: int;
    mutable last: 'a;
    table: (int, 'a -> unit) Hashtbl.t
}

let on t f =
    let id = t.next_id in
    Hashtbl.add t.table id f;
    t.next_id <- t.next_id + 1;
    id

let off t id =
    Hashtbl.remove t.table id

let emit t value =
    t.last <- value;
    Hashtbl.iter (fun id f -> f value) t.table

let create initial = {
    next_id = 0;
    table = Hashtbl.create 5;
    last = initial
}

let get_last t = t.last
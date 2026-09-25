open Js_of_ocaml
open Lwt.Infix

module type CREET = sig
  type t
  val reset_healthy_time : t -> unit
  val create : Dom_html.divElement Js.t -> t
  val move : t -> unit Lwt.t
  val is_healthy_for : t -> int -> bool
  val is_contaminated : t -> bool
  val collides : t -> t -> bool
  val contaminated : t -> unit
  val heal : t -> unit
  val is_heal : t -> bool
  val is_dead : t -> bool
  val is_mean : t -> bool
  val is_in : t -> float -> float -> bool 
  val set_position : t -> float -> float -> unit
  val set_held : t -> bool -> unit
  val follow_nearest_healthy : t -> t list -> unit
end

module Creet : CREET = struct
  type state = Healthy | Contaminated | Berserk | Mean | Dead
  type t = {
    div : Dom_html.divElement Js.t;
    game : Dom_html.divElement Js.t;
    mutable x : float;
    mutable y : float;
    mutable dx : float;
    mutable dy : float;
    mutable angle : float;
    mutable state : state;
    mutable size : int;
    healthySize : int;
    mutable speed : float;
    mutable healthySpeed : float;
    mutable healthy_time : int;
    mutable contamined_time : int;
    mutable held : bool;
  }

  let reset_healthy_time creet = 
    creet.healthy_time <- 0
    
  let contaminated creet =
    if creet.held then
      ()
    else
      match creet.state with
      | Contaminated
      | Berserk
      | Mean 
      | Dead -> ()
      | Healthy -> (
        let random = Random.int 100 in
        creet.contamined_time <- 0;
        creet.healthySpeed <- creet.speed;
        creet.speed <- creet.speed *. 0.85;
        creet.dx <- creet.speed *. cos creet.angle;
        creet.dy <- creet.speed *. sin creet.angle;
        if random <= 9 then (
          creet.state <- Berserk;
          creet.div##.style##.backgroundColor := Js.string "orange"
        )
        else if random <= 19 then (
          creet.state <- Mean;
          creet.div##.style##.backgroundColor := Js.string "purple";
          creet.size <- int_of_float(float_of_int(creet.size) *. 0.85);
          creet.div##.style##.width := Js.string (string_of_int creet.size ^ "px");
          creet.div##.style##.height := Js.string (string_of_int creet.size ^ "px")
        )
        else (
          creet.state <- Contaminated;
          creet.div##.style##.backgroundColor := Js.string "red"
        )
      )

  let create game = 
    let size = 50 in
    let speed = 2.0 in
    let angle = Random.float (2.0 *. Float.pi) in
    let healthy_time = 0 in
    let doc = Dom_html.window##.document in
    let div = Dom_html.createDiv doc in
    
    div##.style##.position := Js.string "absolute";
    div##.style##.width := Js.string (string_of_int size ^ "px");
    div##.style##.height := Js.string (string_of_int size ^ "px");
    div##.style##.backgroundColor := Js.string "green";
    div##.style##.borderRadius := Js.string "50%";
    Dom.appendChild game div;
    let width = game##.clientWidth in
    let height = game##.clientHeight in
    let riverY = int_of_float((15.0 *. ((float_of_int height) /. 100.0))) in
    let x = float_of_int(Random.int (width - (size * 2)) + size) in
    let y = float_of_int(Random.int (height - (size * 2) - riverY) + size + riverY) in
    {
      div;
      game;
      x;
      y;
      dx = speed *. (cos angle);
      dy = speed *. (sin angle);
      angle;
      state = Healthy;
      size;
      healthySize = size;
      speed;
      healthySpeed = speed;
      healthy_time;
      contamined_time = 0;
      held = false;
    }


  let move creet =
    let largeur = creet.game##.clientWidth in
    let hauteur = creet.game##.clientHeight in
    
    let rec loop () = 
      if creet.held = false && creet.state != Dead then
        begin
            (* Random dir *)
          if Random.int 100 = 0 && creet.state != Mean then begin
            creet.angle <- Random.float (2.0 *. Float.pi);
            creet.speed <- creet.speed *. 1.02;
            creet.dx <- creet.speed *. cos creet.angle;
            creet.dy <- creet.speed *. sin creet.angle
          end;
          (* Move *)
          creet.x <- creet.x +. creet.dx;
          creet.y <- creet.y +. creet.dy;
          
          (* Update dir *)
          if (int_of_float creet.x >= (largeur - creet.size)) then begin
            creet.x <- float_of_int (largeur - creet.size);
            creet.dx <- creet.dx *. -1.0
          end;
          if (creet.x <= 0.0) then begin
            creet.x <- 0.0;
            creet.dx <- creet.dx *. -1.0
          end;
          if (int_of_float creet.y >= (hauteur - creet.size)) then begin
            creet.y <- float_of_int (hauteur - creet.size);
            creet.dy <- creet.dy *. -1.0
          end;
          if (creet.y <= 0.0) then begin
            creet.y <- 0.0;
            creet.dy <- creet.dy *. -1.0
          end;
          
          (* H42N42 River *)
          if creet.y < (14.0 *. ((float_of_int hauteur) /. 100.0)) then
            contaminated creet;
          
          if creet.state = Healthy then
            creet.healthy_time <- (creet.healthy_time + 1)
          else
            begin
                if creet.state = Berserk then
                  begin
                    let creetSize = int_of_float((float_of_int creet.healthySize) *. 0.85) in
                    creet.size <- creetSize + int_of_float(float_of_int(creetSize * 3) *. (float_of_int(creet.contamined_time) /. 1500.0));
                    creet.div##.style##.width := Js.string (string_of_int creet.size ^ "px");
                    creet.div##.style##.height := Js.string (string_of_int creet.size ^ "px")
                  end;
              creet.contamined_time <- (creet.contamined_time + 1);
            end;
          if (creet.contamined_time >= 1500) then
            begin
              creet.div##.style##.display := Js.string "none";
              creet.state <- Dead;
            end;
          creet.div##.style##.left := Js.string ((string_of_int (int_of_float creet.x)) ^ "px");
          creet.div##.style##.top := Js.string ((string_of_int (int_of_float creet.y)) ^ "px");
        end;
      Js_of_ocaml_lwt.Lwt_js.sleep 0.01 >>= fun () -> loop ()
    in
    if creet.state != Dead then
      loop ()
    else
      Lwt.return_unit
    
  let is_healthy_for creet ms = creet.healthy_time >= ms

  let is_contaminated creet =
    match creet.state with
    | Contaminated
    | Berserk
    | Mean -> true
    | Dead
    | Healthy -> false

  let collides c1 c2 =
    let dx = (c1.x +. ((float_of_int c1.size) /. 2.0)) -. (c2.x +. ((float_of_int c2.size) /. 2.0)) in
    let dy = (c1.y +. ((float_of_int c1.size) /. 2.0)) -. (c2.y +. ((float_of_int c2.size) /. 2.0)) in
    let distance_squared = (dx *. dx) +. (dy *. dy) in
    let radius = float_of_int ((c1.size + c2.size) / 2) in

    if distance_squared <= radius *. radius then
      true
    else
      false

  let is_heal creet = 
    creet.held

  let is_mean creet =
    creet.state = Mean

  let is_dead creet =
    creet.state = Dead

  let heal creet = 
    if creet.state != Healthy then 
    begin
      creet.state <- Healthy;
      creet.size <- creet.healthySize;
      creet.speed <- creet.healthySpeed;
      creet.dx <- creet.speed *. cos creet.angle;
      creet.dy <- creet.speed *. sin creet.angle;
      creet.div##.style##.backgroundColor := Js.string "green";
      creet.div##.style##.height := Js.string (string_of_int creet.size ^ "px");
      creet.div##.style##.width := Js.string (string_of_int creet.size ^ "px")
    end

  let is_in creet x y =
    let centerX = creet.x +. float_of_int (creet.size / 2) in
    let centerY = creet.y +. float_of_int (creet.size / 2) in
    let dist = ((x -. centerX) *. (x -. centerX)) +. ((y -. centerY) *. ((y -. centerY))) in
    dist <= float_of_int(creet.size * creet.size) && creet.state != Dead
  let set_position creet x y = 
    creet.x <- x -. float_of_int(creet.size / 2);
    creet.y <- y -. float_of_int(creet.size / 2);
    creet.div##.style##.left := Js.string ((string_of_int (int_of_float creet.x)) ^ "px");
    creet.div##.style##.top := Js.string ((string_of_int (int_of_float creet.y)) ^ "px")
  let set_held creet held = 
    creet.held <- held

  let follow_nearest_healthy creet creets =
    let distance_squared c1 c2 =
      let dx = c1.x -. c2.x in
      let dy = c1.y -. c2.y in
      dx *. dx +. dy *. dy
    in

    let healthy_creets = List.filter (fun c -> c != creet && c.state = Healthy) creets in
    match healthy_creets with
    | [] -> ()
    | first :: rest ->
      let nearest =
        List.fold_left
        (fun current c ->
          if distance_squared creet c < distance_squared creet current
          then c
          else current
        )
        first
        rest
      in

      let dx = nearest.x -. creet.x in
      let dy = nearest.y -. creet.y in
      let angle = atan2 dy dx in

      creet.angle <- angle;
      creet.dx <- creet.speed *. cos angle;
      creet.dy <- creet.speed *. sin angle
end


let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->

    Random.self_init ();
    let doc = Dom_html.window##.document in
    let game = Js.Unsafe.coerce (Dom_html.getElementById "game") in
    let start_button = Js.Unsafe.coerce (Dom_html.getElementById "start") in
    let river = Dom_html.createDiv doc in
    let map = Dom_html.createDiv doc in
    let hospital = Dom_html.createDiv doc in
    let creets = ref [] in
    let held_creet = ref None in
    let game_started = ref false in
    let game_over = ref false in

    game##.style##.userSelect := Js.string "none";
    game##.style##.webkitUserSelect := Js.string "none";
    river##.style##.width := Js.string "100%";
    river##.style##.height := Js.string "15%";
    river##.style##.backgroundColor := Js.string "blue";

    map##.style##.width := Js.string "100%";
    map##.style##.height := Js.string "70%";
    map##.style##.backgroundColor := Js.string "lightgreen";

    hospital##.style##.width := Js.string "100%";
    hospital##.style##.height := Js.string "15%";
    hospital##.style##.backgroundColor := Js.string "lightgray";

    Dom.appendChild game river;
    Dom.appendChild game map;
    Dom.appendChild game hospital;


    let game_over_div = Dom_html.createDiv doc in

    game_over_div##.style##.position := Js.string "absolute";
    game_over_div##.style##.top := Js.string "0";
    game_over_div##.style##.left := Js.string "0";
    game_over_div##.style##.width := Js.string "100%";
    game_over_div##.style##.height := Js.string "100%";
    game_over_div##.style##.backgroundColor :=
      Js.string "rgba(0, 0, 0, 0.75)";
    game_over_div##.style##.display := Js.string "none";
    (* game_over_div##.style##.alignItems := Js.string "center"; *)
    (* game_over_div##.style##.justifyContent := Js.string "center"; *)
    game_over_div##.style##.color := Js.string "white";
    game_over_div##.style##.fontSize := Js.string "60px";
    game_over_div##.style##.fontWeight := Js.string "bold";
    game_over_div##.style##.zIndex := Js.string "1000";

    game_over_div##.innerHTML :=
      Js.string "GAME OVER";

    let show_game_over () =
      if not !game_over then begin
        game_over := true;

        game_over_div##.style##.display := Js.string "flex";

        start_button##.style##.display := Js.string "none"
      end
    in

    Dom.appendChild game game_over_div;

    let has_healthy_creet () =
      List.exists
        (fun c -> not (Creet.is_contaminated c) && not (Creet.is_dead c))
        !creets
    in

    let rec contamination_loop () =
      Js_of_ocaml_lwt.Lwt_js.sleep 0.01 >>= fun () ->

      List.iter (fun c -> if Creet.is_mean c then Creet.follow_nearest_healthy c !creets)!creets;
        List.iter
            (fun c1 ->
              if Creet.is_contaminated c1 && Creet.is_heal c1 = false then
                List.iter
                  (fun c2 ->
                    if c1 != c2 && Creet.collides c1 c2 then begin
                      let random = Random.int (50) in (*2 % de chance par iteration*)
                      if random = 0 then
                        Creet.contaminated c2;
                    end
                  )
                  !creets
            )
            !creets;
      creets := List.filter (fun c -> not (Creet.is_dead c)) !creets;
      if !game_started && not (has_healthy_creet ()) then
        show_game_over ();
      contamination_loop ()
    in

    let rec reproduction_loop () =
      Js_of_ocaml_lwt.Lwt_js.sleep 0.1 >>= fun () ->
        (* let maxSpeed = List.fold_left (fun acc x -> max acc x) 0 [4; 12; 3; 8] *)
        if List.exists (fun c -> Creet.is_healthy_for c 300) !creets
        then begin
          let c = Creet.create game in
          creets := c :: !creets;
          Lwt.async (fun () -> Creet.move c);
          List.iter
          (fun c -> Creet.reset_healthy_time c)
          !creets
        end;
        reproduction_loop ()
    in

    start_button##.onclick := Dom_html.handler (fun _ ->
        game_started := true;
        let c = Creet.create game in
        Lwt.async (fun () -> Creet.move c);
        creets := c :: !creets;
        start_button##.style##.display := Js.string "none";
      Js._true
    );

    Lwt.async reproduction_loop;
    Lwt.async contamination_loop;

    game##.onmousemove := Dom_html.handler (fun ev ->
      if !game_started = true  && !game_over = false then 
        begin
          match !held_creet with
          | None -> Js._true
          | Some c ->
            let rect = game##getBoundingClientRect in
            let x = float_of_int ev##.clientX -. rect##.left in
            let y = float_of_int ev##.clientY -. rect##.top in

            Creet.set_position c x y;
            Js._true
        end
      else
        Js._true;
      );

    game##.onmousedown := Dom_html.handler (fun ev ->
      if !game_started = true  && !game_over = false then 
        begin
          Dom.preventDefault ev;
          let rect = game##getBoundingClientRect in
          let x = float_of_int ev##.clientX -. rect##.left in
          let y = float_of_int ev##.clientY -. rect##.top in
          
          held_creet := List.find_opt(fun c -> Creet.is_in c x y) !creets;
          match !held_creet with
          | None -> Js._true
          | Some c -> Creet.set_held c true;
          Js._true
        end
      else
        Js._true
      );

    game##.onmouseup := Dom_html.handler (fun ev ->
      if !game_started = true  && !game_over = false then 
        begin
          let rect = game##getBoundingClientRect in
          let y = float_of_int ev##.clientY -. rect##.top in
          match !held_creet with
          | None -> Js._true
          | Some c -> (
            Creet.set_held c false;
            held_creet := None;
            let hauteur = game##.clientHeight in
            if y >= (float_of_int hauteur) *. 0.85 then
              Creet.heal c;
              Js._true)
        end
      else
        Js._true
          );

    game##.onmouseleave := Dom_html.handler (fun ev ->
      if !game_started = true  && !game_over = false then 
        begin
          let rect = game##getBoundingClientRect in
          let y = float_of_int ev##.clientY -. rect##.top in
          match !held_creet with
          | None -> Js._true
          | Some c -> (
            Creet.set_held c false;
            held_creet := None;
            let hauteur = game##.clientHeight in
            if y >= (float_of_int hauteur) *. 0.85 then
              Creet.heal c;
            Js._true)
        end
      else
        Js._true
          );
  Js._true)
open Js_of_ocaml
open Lwt.Infix

module type CREET = sig
  type t
  val reset_healthy_time : t -> unit
  val create : Dom_html.divElement Js.t -> int -> float -> t
  val move : t -> int -> unit Lwt.t
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
        creet.div##.style##.zIndex := Js.string "100";
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

  let create game size speed = 
    let angle = Random.float (2.0 *. Float.pi) in
    let healthy_time = 0 in
    let doc = Dom_html.window##.document in
    let div = Dom_html.createDiv doc in

    div##.style##.border := Js.string "2px solid #ffffffa8";
    div##.style##.position := Js.string "absolute";
    div##.style##.width := Js.string (string_of_int size ^ "px");
    div##.style##.height := Js.string (string_of_int size ^ "px");
    div##.style##.backgroundColor := Js.string "green";
    div##.style##.borderRadius := Js.string "50%";
    div##.style##.zIndex := Js.string "1";
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

  let move creet time_to_die =
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
          
          (* River *)
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
          if (creet.contamined_time >= time_to_die) then
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
      creet.div##.style##.zIndex := Js.string "1";
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

    let best_score = ref 0 in
    let score_display = Js.Unsafe.coerce (Dom_html.getElementById "score") in

    let speed_value = Js.Unsafe.coerce (Dom_html.getElementById "speed-value") in
    let size_value = Js.Unsafe.coerce (Dom_html.getElementById "size-value") in
    let time_value = Js.Unsafe.coerce (Dom_html.getElementById "time-value") in

    let speed_slider = Js.Unsafe.coerce (Dom_html.getElementById "speed") in
    let size_slider = Js.Unsafe.coerce (Dom_html.getElementById "size") in
    let time_slider = Js.Unsafe.coerce (Dom_html.getElementById "timeToDie") in

    speed_slider##.oninput := Dom_html.handler (fun _ ->
      speed_value##.textContent :=
        Js.some (Js.string (Js.to_string speed_slider##.value));
      Js._true
    );

    size_slider##.oninput := Dom_html.handler (fun _ ->
      size_value##.textContent :=
        Js.some (Js.to_string size_slider##.value ^ " px");
      Js._true
    );

    time_slider##.oninput := Dom_html.handler (fun _ ->
      time_value##.textContent :=
        Js.some (Js.to_string time_slider##.value);
      Js._true
    );

    let speed = ref 2.0 in (* 1.5 - 3.0 *)
    let size = ref 50 in (* 40 - 80 *)
    let timeToDie = ref 1000 in (* 500 - 2000 *)

    game##.style##.userSelect := Js.string "none";
    game##.style##.webkitUserSelect := Js.string "none";
    river##.style##.width := Js.string "100%";
    river##.style##.height := Js.string "15%";
    river##.style##.backgroundImage := Js.string "url('assets/river.jpg')";

    Js.Unsafe.set river##.style "backgroundSize" (Js.string "cover");
    river##.style##.backgroundPosition := Js.string "center";
    river##.style##.backgroundRepeat := Js.string "no-repeat";
    
    map##.style##.width := Js.string "100%";
    map##.style##.height := Js.string "70%";
    
    hospital##.style##.width := Js.string "100%";
    hospital##.style##.height := Js.string "15%";
    hospital##.style##.backgroundImage := Js.string "url('assets/hospital.jpg')";
    (* river##.style##.background :=
    Js.string "linear-gradient(180deg, #42b9e9, #1676b8)"; *)

    map##.style##.background :=
    Js.string "linear-gradient(135deg, #555555, #555555)";

    (* hospital##.style##.background :=
    Js.string "linear-gradient(180deg, #f0f3e9, #cbd8c6)"; *)
    Js.Unsafe.set hospital##.style "backgroundSize" (Js.string "cover");
    hospital##.style##.backgroundPosition := Js.string "center";
    hospital##.style##.backgroundRepeat := Js.string "no-repeat";
    Dom.appendChild game river;
    Dom.appendChild game map;
    Dom.appendChild game hospital;

    let game_over_div = Dom_html.createDiv doc in
    game_over_div##.style##.position := Js.string "absolute";
    game_over_div##.style##.top := Js.string "0";
    game_over_div##.style##.left := Js.string "0";
    game_over_div##.style##.width := Js.string "100%";
    game_over_div##.style##.height := Js.string "100%";
    game_over_div##.style##.backgroundColor := Js.string "rgba(0, 0, 0, 0.8)";
    game_over_div##.style##.zIndex := Js.string "1000";
    game_over_div##.style##.display := Js.string "none"; (* Masqué au début *)
    Js.Unsafe.set game_over_div##.style "alignItems" (Js.string "center");
    Js.Unsafe.set game_over_div##.style "justifyContent" (Js.string "center");

    game_over_div##.innerHTML :=
      Js.string (
        "<div style='text-align:center;padding:30px'>" ^
        "<div style='font-size:clamp(36px,6vw,60px);color:#ff7272;" ^
        "text-shadow:0 0 20px #ff333366;margin-bottom:14px'>" ^
        "GAME OVER</div>" ^
        "<p style='font-size:18px;color:#eee;margin-bottom:28px'>" ^
        "All the Healthy Creets have disappeared...</p>" ^
        "<p style='font-size:22px;color:#a8f06b;font-weight:bold;margin-bottom:28px'>" ^
        "🏆 Score : <span id='final-score'>0</span></p>" ^
        "<button id='replay' style='padding:13px 30px;border:0;" ^
        "border-radius:10px;background:#a8f06b;color:#17301b;" ^
        "font-size:17px;font-weight:bold;cursor:pointer'>" ^
        "↻ Rejouer</button></div>"
      );

    Dom.appendChild game game_over_div;

    Lwt.async (fun () ->
      Js_of_ocaml_lwt.Lwt_js.yield () >>= fun () ->
      let replay_button = Js.Unsafe.coerce (Dom_html.getElementById "replay") in
      replay_button##.onclick := Dom_html.handler (fun _ ->
        let location = Js.Unsafe.get Dom_html.window "location" in
        ignore (Js.Unsafe.meth_call location "reload" [||]);
        Js._false
      );
      Lwt.return_unit
    );

    let show_game_over () =
    if not !game_over then 
      begin
        game_over := true;
        game_started := false;
        held_creet := None;

        let final_score = Js.Unsafe.coerce (Dom_html.getElementById "final-score") in
        final_score##.textContent := Js.some (Js.string (string_of_int !best_score));
        game_over_div##.style##.display := Js.string "flex";
        start_button##.style##.display := Js.string "none"
      end
    in

    let has_healthy_creet () =
      List.exists
        (fun c -> not (Creet.is_contaminated c) && not (Creet.is_dead c))
        !creets
    in

    let healthy_count () =
      List.length (
        List.filter
          (fun c ->
            not (Creet.is_contaminated c) &&
            not (Creet.is_dead c))
          !creets
      )
    in

    let update_score () =
      let current = healthy_count () in
      if current > !best_score then (
        best_score := current;
        score_display##.textContent :=
          Js.some (Js.string (string_of_int !best_score))
      )
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
        if List.exists (fun c -> Creet.is_healthy_for c 300) !creets
        then begin
          let c = Creet.create game !size !speed in
          creets := c :: !creets;
          update_score();
          Lwt.async (fun () -> Creet.move c !timeToDie);
          List.iter
          (fun c -> Creet.reset_healthy_time c)
          !creets
        end;
        reproduction_loop ()
    in

    start_button##.onclick := Dom_html.handler (fun _ ->
        speed := float_of_string (Js.to_string speed_slider##.value);
        size := int_of_string (Js.to_string size_slider##.value);
        timeToDie := int_of_string (Js.to_string time_slider##.value);

        speed_slider##.disabled := Js._true;
        size_slider##.disabled := Js._true;
        time_slider##.disabled := Js._true;

        game_started := true;
        let c = Creet.create game !size !speed in
        Lwt.async (fun () -> Creet.move c !timeToDie);
        creets := c :: !creets;
        update_score();
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
            update_score ();
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
            update_score ();
            Js._true)
        end
      else
        Js._true
          );
  Js._true)
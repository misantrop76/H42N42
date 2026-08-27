open Js_of_ocaml
open Lwt.Infix

module type CREET = sig
	type t
	
	val create : unit -> t
	val move : t -> unit Lwt.t
end

module Creet : CREET = struct
	type t = {
		div : Dom_html.divElement Js.t;
		mutable x : float;
		mutable y : float;
		mutable dx : float;
		mutable dy : float;
		mutable size : int;
		mutable speed : float;
	}
		
	let create () = 
		let doc = Dom_html.document in
		let body = doc##.body in
		let size = 50 in
		let speed = 6.0 in
		let angle = Random.float (2.0 *. Float.pi) in
		let div = Dom_html.createDiv doc in

		div##.style##.position := Js.string "absolute";
		div##.style##.width := Js.string (string_of_int size ^ "px");
		div##.style##.height := Js.string (string_of_int size ^ "px");
		div##.style##.backgroundColor := Js.string "red";
		div##.style##.borderRadius := Js.string "50%";
		Dom.appendChild body div;
		let width = Dom_html.window##.innerWidth in
		let height = Dom_html.window##.innerHeight in
		let x = float_of_int(Random.int (width - size)) in
		let y = float_of_int(Random.int (height - size)) in
		{
			div;
			x;
			y;
			dx = speed *. (cos angle);
			dy = speed *. (sin angle);
			size;
			speed;
		}
			
	let move creet =
		let largeur = Dom_html.window##.innerWidth in
		let hauteur = Dom_html.window##.innerHeight in
		
		let rec loop () = 
			(* Random dir *)
			if Random.int 500 = 0 then begin
				let angle = Random.float (2.0 *. Float.pi) in
				creet.speed <- creet.speed *. 1.1;
				creet.dx <- creet.speed *. cos angle;
				creet.dy <- creet.speed *. sin angle;
			end;
		
			(* Move *)
			creet.x <- creet.x +. creet.dx;
			creet.y <- creet.y +. creet.dy;

			(* Update dir *)
			if (int_of_float creet.x >= (largeur - creet.size)) then begin
				creet.x <- float_of_int (largeur - creet.size);
				creet.dx <- creet.dx *. -1.0;
			end;
			if (creet.x <= 0.0) then begin
				creet.x <- 0.0;
				creet.dx <- creet.dx *. -1.0;
			end;
			if (int_of_float creet.y >= (hauteur - creet.size)) then begin
				creet.y <- float_of_int (hauteur - creet.size);
				creet.dy <- creet.dy *. -1.0;
			end;
			if (creet.y <= 0.0) then begin
				creet.y <- 0.0;
				creet.dy <- creet.dy *. -1.0;
			end;

			(* Display *)
			creet.div##.style##.left := Js.string ((string_of_int (int_of_float creet.x)) ^ "px");
			creet.div##.style##.top := Js.string ((string_of_int (int_of_float creet.y)) ^ "px");

			Js_of_ocaml_lwt.Lwt_js.sleep 0.01 >>= fun () -> loop ()
		in
		loop ()
end

let () =
Random.self_init ();

	let rec aux n =
		if n >= 0 then begin
			let c = Creet.create () in
			Lwt.async (fun () -> Creet.move c);
			aux (n - 1)
		end;
	in

	Dom_html.window##.onload := Dom_html.handler (fun _ ->
		aux 20;
		Js._true)

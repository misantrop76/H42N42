open Js_of_ocaml
open Lwt.Infix

let () =
	Random.self_init ();
	let doc = Dom_html.document in
	let body = doc##.body in
	let div = Dom_html.createDiv doc in
	let taille = 50 in
	let vitesse = ref 5.0 in
	let angle = ref (Random.float (2.0 *. Float.pi)) in

	div##.style##.position := Js.string "absolute";
	div##.style##.width := Js.string (string_of_int taille ^ "px");
	div##.style##.height := Js.string (string_of_int taille ^ "px");
	div##.style##.backgroundColor := Js.string "red";
	div##.style##.borderRadius := Js.string "50%";
	Dom.appendChild body div;
	
	let largeur = Dom_html.window##.innerWidth in
	let hauteur = Dom_html.window##.innerHeight in

	let x = ref (float_of_int(Random.int (largeur - taille))) in
	let dx = ref (!vitesse *. cos !angle) in
	let y = ref (float_of_int(Random.int (hauteur - taille))) in
	let dy = ref (!vitesse *. sin !angle) in

	let rec move () =
		(* Random dir *)
		if Random.int 500 = 0 then begin
			angle := Random.float (2.0 *. Float.pi);
			vitesse := !vitesse *. 1.5;
			dx := !vitesse *. cos !angle;
			dy := !vitesse *. sin !angle;
		end;

		(* Update dir *)
		if (int_of_float !x >= (largeur - taille)) then begin
			x := float_of_int (largeur - taille);
			dx := !dx *. -1.0;
		end;
		if (int_of_float !x <= 0) then begin
			x := 0.0;
			dx := !dx *. -1.0;
		end;
		if (int_of_float !y >= (hauteur - taille)) then begin
			y := float_of_int (hauteur - taille);
			dy := !dy *. -1.0;
		end;
		if (int_of_float !y <= 0) then begin
			y := 0.0;
			dy := !dy *. -1.0;
		end;

		(* Move *)
		x := !x +. !dx;
		y := !y +. !dy;

		(* Display *)
		div##.style##.left := Js.string ((string_of_int (int_of_float !x)) ^ "px");
		div##.style##.top := Js.string ((string_of_int (int_of_float !y)) ^ "px");

		Js_of_ocaml_lwt.Lwt_js.sleep 0.01 >>= fun () -> move ()
	in

	Lwt.async move

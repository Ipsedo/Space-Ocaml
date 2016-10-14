type fire = First | Second | Third | Fourth | Fifth | Sixth | Seventh | Eighth | Nineth
type direction = VeryVeryLeft | VeryLeft | Left | Center | Right | VeryRight | VeryVeryRight | Cos | Sin | CosLeft | CosRight | SinLeft | SinRight
type bonus_type = Weapon | Life | Frequence

let current_dir = Sys.getcwd ()

let music_explosion = current_dir^"/Songs/shot.wav"
let music_loop = current_dir^"/Songs/loop_funky_sam.wav"
let lepen_cry = current_dir^"/Songs/boss_01.wav"
let trump_cry = current_dir^"/Songs/boss_02.wav"
let hitler_cry = current_dir^"/Songs/boss_03.wav"
let kim_cry = current_dir^"/Songs/boss_04.wav"
let game_over = current_dir^"/Songs/game_over.wav"
let congratulation = current_dir^"/Songs/win.wav"
let power_up = current_dir^"/Songs/power_up.wav"
let life_up = current_dir^"/Songs/life_up.wav"

let font = current_dir^"/Fonts/Rosewood.ttf"

let background = current_dir^"/Images/Milky_Way_IR_Spitzer.jpg"
let lepen = current_dir^"/Images/boss_01.jpg"
let trump = current_dir^"/Images/boss_02.jpg"
let hitler = current_dir^"/Images/boss_03.png"
let kim = current_dir^"/Images/boss_04.jpg"

class base_item (x : int) (y : int) (height : int) (width :int) (right : int) (up : int) (lives : int) ((r, g, b) : int * int * int) (screen : Sdlvideo.surface) =

	let color = Int32.of_int ((255 lsl 24)+(r lsl 16)+(g lsl 8)+(b)) in
	object (self)
		val mutable x = x
		val mutable y = y
		val h = height
		val w = width
		val r = right
		val u = up
		val mutable l = lives
		val c = color
		val s = screen
		method x = x
		method y = y
		method h = h
		method w = w
		method is_bonus = false
		method decrement_life x = l <- l - x
		method private is_hitted x2 y2 w2 h2 = if x + w > x2 && x < x2 + w2 && y + h > y2 && y < y2 + h2 then true else false
		method private explosion = let music = Sdlmixer.loadWAV music_explosion in
								Sdlmixer.setvolume_chunk music 0.05;
								Sdlmixer.play_channel music
		method out_of_bounce = if y < 0 || y + h > u then true else false
		method collision (other : base_item) = match (self#is_hitted other#x other#y other#w other#h) with 
												true -> self#explosion; other#decrement_life 1; self#decrement_life 1
												|_ -> other#decrement_life 0; self#decrement_life 0
		method collison_bonus (other : base_item) = match (self#is_hitted other#x other#y other#w other#h) with
													true -> other#decrement_life 1; true
													|_ -> false
		method draw = Sdlvideo.fill_rect ~rect:(Sdlvideo.rect x y w h) screen c
		method move_base xd yd = x <- x + xd; y <- y + yd
		method is_alive = l > 0
		method move = ()
		method b = First
		method bt = Weapon
	end

class bonus (x : int) (y : int) (height : int) (width :int) (right : int) (up : int) (lives : int) ((r, g, b) as color : int * int * int) (screen : Sdlvideo.surface) (bonus_type : bonus_type) =
	object (self)
		inherit base_item x y height width right up lives color screen as super
		val bt = bonus_type
		method bt = bt
		method is_bonus = true
		method move = super#move_base 0 10
	end

class bonus_list (bonus : bonus list) (right : int) (up : int) (screen : Sdlvideo.surface) =
	object (self)
		val mutable b = bonus
		val r = right
		val u = up
		val s = screen
		method private add (bonus : bonus) = b <- bonus::b
		method private random_bonus_type = let x = Random.int 4 in match x with
										0 | 1 -> Weapon
										|2 -> Life
										|_ -> Frequence
		method private random_bonus = new bonus (Random.int r) 15 10 10 r u 1 (20,255,20) s self#random_bonus_type
		method get = b
		method spawn_bonus = let rand = Random.int 250 in
										match rand with
										5 -> self#add self#random_bonus
										|_ -> ()
		method move = List.iter (fun x -> x#move) b
		method draw = List.iter (fun x -> x#draw) b
		method filter_exploded = b <- List.filter (fun x -> x#is_alive) b
		method filter_out = b <- List.filter (fun x -> not x#out_of_bounce) b
	end

class ship (x : int) (y : int) (height : int) (width :int) (right : int) (up : int) (lives : int) ((r, g, b) as color : int * int * int) (screen : Sdlvideo.surface) (bonus : fire) (frequence : int list) =
	object (self)
		inherit base_item x y height width right up lives color screen as super
		val mutable b = bonus
		val mutable f = frequence
		method b = b
		method f = f
		method private get_x = let x,_,_ = Sdlmouse.get_state () in x
		method private upgrade_bonus = match b with
										First -> b <- Second
										|Second -> b <- Third
										|Third -> b <- Fourth
										|Fourth -> b <- Fifth
										|Fifth -> b <- Sixth
										|Sixth -> b <- Seventh
										|Seventh -> b <- Eighth
										|Eighth -> b <- Nineth
										|Nineth -> b <- Nineth
		method private upgrade_freq = match f with
									[10] -> f <- [5;10]
									|[5;10] -> f <- [3;7;10]
									|[3;7;10] -> f <- [0;3;6;9]
									|_ -> f <- [0;3;6;9]
		method private power_up_song = let music = Sdlmixer.loadWAV power_up in
								Sdlmixer.setvolume_chunk music 0.15;
								Sdlmixer.play_channel music
		method private life_up_song = let music = Sdlmixer.loadWAV life_up in
								Sdlmixer.setvolume_chunk music 0.15;
								Sdlmixer.play_channel music
		method move = x <- self#get_x
		method draw_live = let font = Sdlttf.open_font font 20 in
    					let text = Sdlttf.render_text_blended font (string_of_int l) ~fg:(1,220,242) in
    					Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 4) (u - 35) 30 30) ~src:text ~dst:s ()
		method private bonus_upgrade (other :base_item) = match other#bt with
														Weapon -> self#power_up_song; self#upgrade_bonus
														|Life -> self#life_up_song; self#decrement_life (-1)
														|Frequence -> self#power_up_song; self#upgrade_freq
		method private collision (other : base_item) = if other#is_bonus then match self#collison_bonus other with
																				true -> self#bonus_upgrade other
																				|_ -> ()
														else super#collision other
		method collision_list (others : base_item list) = List.iter (fun x -> self#collision x) others
	end

class ball (x : int) (y : int) (height : int) (width :int) (right : int) (up : int) (lives : int) ((r, g, b) as color : int * int * int) (screen : Sdlvideo.surface) (direction : direction) (to_up : bool) =
	object (self)
		inherit base_item x y height width right up lives color screen as super
		val d = direction
		val tu = to_up
		method private sin_int j = int_of_float (sin (float_of_int ((y + j) / 15)) *. 30.)
		method private cos_int j = int_of_float (cos (float_of_int ((y + j) / 15)) *. 30.)
		method move = if tu then match d with
								VeryVeryLeft -> super#move_base (-3) (-7)
								|VeryLeft -> super#move_base (-2) (-8)
								|Left -> super#move_base (-1) (-9)
								|Center -> super#move_base 0 (-10)
								|Right -> super#move_base 1 (-9)
								|VeryRight -> super#move_base 2 (-8)
								|VeryVeryRight -> super#move_base 3 (-7)
								|Cos -> super#move_base (self#cos_int 10) (-10)
								|Sin -> super#move_base (self#sin_int 10) (-10)
								|SinLeft -> super#move_base ((self#sin_int 9) - 1) (-9)
								|SinRight -> super#move_base (1 + (self#sin_int 9)) (-9)
								|CosLeft -> super#move_base ((self#cos_int 9) - 1) (-9)
								|CosRight -> super#move_base (1 + (self#cos_int 9)) (-9)
							else match d with
								VeryVeryLeft -> super#move_base (-3) 7
								|VeryLeft -> super#move_base (-2) 8
								|Left -> super#move_base (-1) 9
								|Center -> super#move_base 0 10
								|Right -> super#move_base 1 9
								|VeryRight -> super#move_base 2 8
								|VeryVeryRight -> super#move_base 3 7
								|Cos -> super#move_base (self#cos_int (-10)) 10
								|Sin -> super#move_base (self#sin_int (-10)) 10
								|SinLeft -> super#move_base ((self#sin_int (-9)) - 1) 9
								|SinRight -> super#move_base (1 + (self#sin_int (-9))) 9
								|CosLeft -> super#move_base ((self#cos_int (-9)) - 1) 9
								|CosRight -> super#move_base (1 + (self#cos_int (-9))) 9
	end

class block (x : int) (y : int) (height : int) (width :int) (right : int) (up : int) (lives : int) ((r, g, b) as color : int * int * int) (screen : Sdlvideo.surface) =
	object (self)
		inherit base_item x y height width right up lives color screen as super
		method move = super#move_base 0 10
	end

class boss (x : int) (y : int) (right : int) (up : int) (lives : int) ((r, g, b) as color : int * int * int) (screen : Sdlvideo.surface) (image : string) (bonus : fire) =
	let surface = Sdlloader.load_image image in
	let (_,height,_) = Sdlvideo.surface_dims surface in
	let (width,_,_) = Sdlvideo.surface_dims surface in
	object (self)
		inherit base_item x y height width right up lives color screen as super
		val i = surface
		val b = bonus
		val max_lives = lives
		method b = b
		method draw = Sdlvideo.blit_surface ~src:i ~dst:s ~dst_rect:(Sdlvideo.rect x y w h) ()
		method draw_lives = let font = Sdlttf.open_font font 20 in
    						let text = Sdlttf.render_text_blended font ((string_of_int l)^" / "^(string_of_int max_lives)) ~fg:(1,252,110) in
    						Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r * 3 / 4) (u-35) 60 30) ~src:text ~dst:s ()
		method move = let ran = Random.int 20 in
					let x1 = r / 2 in
					let y1 = u * 1 / 4 in
					match ran with
					10 -> super#move_base ((x1 - x) / 10) ((y1 - y) / 10)
					|_ -> super#move_base (Random.int 20 - 10) (Random.int 20 - 10)
		method collision_balls (balls : base_item list) = List.fold_left (fun x y -> self#collision y; ()) () balls 
	end

class blocks (blocks : block list) (right : int) (up : int) (nb_block : int) (screen : Sdlvideo.surface) =
	object (self)
		val mutable b = blocks
		val r = right
		val u = up
		val nb = nb_block
		val s = screen
		method private add (block : block) = b <- block::b
		method private random_block = new block (Random.int nb * r / nb) 11 10 (r / nb) r u 1 (205,170,74) s
		method get = b
		method spawn_block (x : int) (blocks_spawn : int list) = if List.mem x blocks_spawn then self#add self#random_block else ()
		method move = List.iter (fun b -> b#move) b
		method draw = List.iter (fun b -> b#draw) b
		method filter_exploded = b <- List.filter (fun x -> x#is_alive) b
		method filter_out = b <- List.filter (fun x -> not x#out_of_bounce) b
	end

class balls (ball : ball list) (right : int) (up : int) (screen : Sdlvideo.surface) =
	object (self)
		val mutable b = ball
		val r = right
		val u = up
		val s = screen
		method private add (ball : ball) = b <- ball::b
		method private blocks_and_ball (blocks : block list) (ball : ball) = List.fold_left (fun x y -> x#collision y; x) ball blocks
		method get = b
		method private fire_now x1 y1 bonus to_up color = match bonus with
											First -> self#add (new ball x1 y1 10 10 r u 1 color s Center to_up)		
											|Second -> self#add (new ball x1 y1 10 10 r u 1 color s Sin to_up); self#add (new ball x1 y1 10 10 r u 1 color s Cos to_up)
											|Third -> self#add (new ball x1 y1 10 10 r u 1 color s Center to_up); self#add (new ball x1 y1 10 10 r u 1 color s Right to_up); self#add (new ball x1 y1 10 10 r u 1 color s Left to_up)
											|Fourth -> self#add (new ball x1 y1 10 10 r u 1 color s VeryLeft to_up); self#add (new ball x1 y1 10 10 r u 1 color s Left to_up); self#add (new ball x1 y1 10 10 r u 1 color s Center to_up); self#add (new ball x1 y1 10 10 r u 1 color s Right to_up); self#add (new ball x1 y1 10 10 r u 1 color s VeryRight to_up)
											|Fifth -> self#add (new ball x1 y1 10 10 r u 1 color s SinLeft to_up); self#add (new ball x1 y1 10 10 r u 1 color s CosLeft to_up); self#add (new ball x1 y1 10 10 r u 1 color s Cos to_up); self#add (new ball x1 y1 10 10 r u 1 color s Sin to_up); self#add (new ball x1 y1 10 10 r u 1 color s CosRight to_up); self#add (new ball x1 y1 10 10 r u 1 color s SinRight to_up)
											|Sixth -> self#add (new ball x1 y1 20 10 r u 2 color s Sin to_up); self#add (new ball x1 y1 20 10 r u 2 color s Cos to_up)
											|Seventh -> self#add (new ball x1 y1 20 10 r u 2 color s Center to_up); self#add (new ball x1 y1 20 10 r u 2 color s Right to_up); self#add (new ball x1 y1 20 10 r u 2 color s Left to_up)
											|Eighth -> self#add (new ball x1 y1 20 10 r u 2 color s VeryLeft to_up); self#add (new ball x1 y1 20 10 r u 2 color s Left to_up); self#add (new ball x1 y1 20 10 r u 2 color s Center to_up); self#add (new ball x1 y1 20 10 r u 2 color s Right to_up); self#add (new ball x1 y1 20 10 r u 2 color s VeryRight to_up)
											|Nineth -> self#add (new ball x1 y1 20 10 r u 2 color s SinLeft to_up); self#add (new ball x1 y1 20 10 r u 2 color s CosLeft to_up); self#add (new ball x1 y1 20 10 r u 2 color s Cos to_up); self#add (new ball x1 y1 20 10 r u 2 color s Sin to_up); self#add (new ball x1 y1 20 10 r u 2 color s CosRight to_up); self#add (new ball x1 y1 20 10 r u 2 color s SinRight to_up)
		method fire x1 y1 bonus to_up color (cpt : int) (freq : int list) = if List.mem cpt freq then self#fire_now x1 y1 bonus to_up color else ()
		method move = List.iter (fun b -> b#move) b
		method collision (blocks : block list) = b <- List.map (fun x -> self#blocks_and_ball blocks x) b 
		method draw = List.iter (fun x -> x#draw) b
		method filter_exploded = b <- List.filter (fun x -> x#is_alive) b
		method filter_out = b <- List.filter (fun x -> not x#out_of_bounce) b
	end

class game (right : int) (up : int) =
	let screen = Sdlvideo.set_video_mode right up [`DOUBLEBUF] in
	let ship = new ship 50 (up - 10) 10 10 right up 10 (1,220,242) screen First [10] in
	let balls = new balls [] right up screen in
	let bonus = new bonus_list [] right up screen in
	let bc = Sdlloader.load_image background in
	object (self)
		val r = right
		val u = up
		val fps = 60.
		val scr = screen
		val back_ground = bc
		val mutable channel_music = 0
		val mutable s = ship
		val mutable ba = balls
		val mutable bo = bonus
		method private draw_background = Sdlvideo.blit_surface ~src:back_ground ~src_rect:(Sdlvideo.rect 0 0 r u) ~dst:scr () 
		method private init = Sdl.init [`VIDEO; `AUDIO];
							Sdlttf.init ();
							Sdlmixer.open_audio ();
							let event_m = Sdlevent.make_mask (Sdlevent.of_mask Sdlevent.keydown_mask) in
							Sdlevent.enable_events event_m;
							Sdlkey.enable_unicode true;
							at_exit Sdl.quit;
    						at_exit Sdlmixer.close_audio;
    						at_exit Sdlttf.quit;
    						channel_music <- Sdlmixer.allocate_channels 1000;
							let music = Sdlmixer.loadWAV music_loop in
							Sdlmixer.play_channel ~channel:(channel_music-1) ~loops:(-1) music
		method private game_over (lose : bool) (fst_time : bool) = if lose && (not fst_time) then begin
																Sdlmixer.pause_channel (channel_music-1);
																let music = Sdlmixer.loadWAV game_over in
																Sdlmixer.play_channel ~channel:(channel_music-2) music;
																let font = Sdlttf.open_font font 50 in
																let text1 = Sdlttf.render_text_blended font "Game Over :(" ~fg:(210,67,37) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 6) (u / 10) 30 30) ~src:text1 ~dst:scr () end
															else ()
		method private win (win : bool) (fst_time : bool) = if win && (not fst_time) then begin
																Sdlmixer.pause_channel (channel_music-1);
																let music = Sdlmixer.loadWAV congratulation in
																Sdlmixer.play_channel ~channel:(channel_music-2) music;
																let font = Sdlttf.open_font font 50 in
																let text1 = Sdlttf.render_text_blended font "Congratulation !" ~fg:(210,67,37) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 6) (u / 10) 30 30) ~src:text1 ~dst:scr () end
															else ()
		method private opening (lose : bool) (fst_time : bool) = self#draw_background;
																let font = Sdlttf.open_font font 20 in

																Sdlvideo.fill_rect ~rect:(Sdlvideo.rect (r / 10) (u / 4) 10 10) scr (Int32.of_int ((200 lsl 24)+(20 lsl 16)+(255 lsl 8)+(20)));
    															let text1 = Sdlttf.render_text_blended font "Fire / Fire freq Bonus, Life Bonus" ~fg:(20,255,20) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 6) (u / 4) 30 30) ~src:text1 ~dst:scr ();

    															Sdlvideo.fill_rect ~rect:(Sdlvideo.rect (r / 10) (u * 2 / 4) 20 10) scr (Int32.of_int ((200 lsl 24)+(205 lsl 16)+(170 lsl 8)+(75)));
    															let text2 = Sdlttf.render_text_blended font "Block, decreases Ship's Life" ~fg:(205,170,75) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 6) (u * 2 / 4) 30 30) ~src:text2 ~dst:scr ();

    															Sdlvideo.fill_rect ~rect:(Sdlvideo.rect (r / 10) (u * 3 / 4) 10 10) scr (Int32.of_int ((200 lsl 24)+(1 lsl 16)+(220 lsl 8)+(242)));
    															let text3 = Sdlttf.render_text_blended font "Ship, control and fire with Mouse" ~fg:(1,220,242) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 6) (u * 3 / 4) 30 30) ~src:text3 ~dst:scr ();

    															let text4 = Sdlttf.render_text_blended font "In game, Press space to pause" ~fg:(242,242,7) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 3) (u * 5 / 6) 30 30) ~src:text4 ~dst:scr ();

    															let text4 = Sdlttf.render_text_blended font "Press Enter to start" ~fg:(242,242,7) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 15) (u - 40) 30 30) ~src:text4 ~dst:scr ();

    															let text4 = Sdlttf.render_text_blended font "Press q to quit game" ~fg:(242,242,7) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r / 2) (u - 40) 30 30) ~src:text4 ~dst:scr ();

    															self#game_over lose fst_time;
																self#win (not lose) fst_time;

																Sdlvideo.flip scr;
																let rec aux () =
																	let event = Sdlevent.wait_event () in
																	match event with
																	Sdlevent.KEYDOWN (_) when Sdlkey.is_key_pressed Sdlkey.KEY_q -> exit 0
																	|Sdlevent.KEYDOWN (_) when Sdlkey.is_key_pressed Sdlkey.KEY_RETURN -> Sdlmixer.expire_channel (channel_music-2) None; Sdlmixer.resume_channel (channel_music-1); ()
																	|_ -> aux ()
																in aux ()
		method private draw_score (score : int) (score_max : int) = let font = Sdlttf.open_font font 20 in
																let text1 = Sdlttf.render_text_blended font ((string_of_int score)^" / "^(string_of_int score_max)) ~fg:(20,255,20) in
    															Sdlvideo.blit_surface ~dst_rect:(Sdlvideo.rect (r * 3 / 4) (u - 35) 30 30) ~src:text1 ~dst:scr ()
    	method private quit_pause_1 =  match Sdlevent.poll () with
								None -> ()
								|Some evt when Sdlkey.is_key_pressed Sdlkey.KEY_SPACE -> self#quit_pause_2
								|Some evt when evt = Sdlevent.QUIT -> exit 0
								|Some _ -> ()
		method private quit_pause_2 = let rec aux () =
									match Sdlevent.poll () with
									None -> aux ()
									|Some evt when Sdlkey.is_key_pressed Sdlkey.KEY_SPACE -> ()
									|Some evt when evt = Sdlevent.QUIT -> exit 0
									|Some _ -> aux ()
								in aux ()
		method private wait (t1 : float) = let rec aux t1 =
											self#quit_pause_1;
											let t2 = Sys.time () in
											if (t2 -. t1 > (1. /. fps)) then Sdlvideo.flip scr
											else aux t1 
										in aux t1
		method private compt_fire (x : int) = if x = 10 then 0 else x + 1
		method private boss_cry (num : int) = match num with
											1 -> let music = Sdlmixer.loadWAV lepen_cry in Sdlmixer.setvolume_chunk music 0.5; Sdlmixer.play_channel music
											|3 -> let music = Sdlmixer.loadWAV trump_cry in Sdlmixer.play_channel music
											|5 -> let music = Sdlmixer.loadWAV hitler_cry in Sdlmixer.play_channel music
											|_ -> let music = Sdlmixer.loadWAV kim_cry in Sdlmixer.play_channel music
		method private create_boss (num : int) = match num with
												1 -> self#boss_cry num; new boss (r / 2) (u * 1 / 4)  r u (num * 150) (0,0,0) scr lepen Third
												|3 -> self#boss_cry num; new boss (r / 2) (u * 1 / 4)  r u (num * 150) (0,0,0) scr trump Fifth
												|5 -> self#boss_cry num; new boss (r / 2) (u * 1 / 4)  r u (num * 150) (0,0,0) scr hitler Seventh
												|_ -> self#boss_cry num; new boss (r / 2) (u * 1 / 4)  r u (num * 150) (0,0,0) scr kim Nineth
		method private create_blocks_spawning_freq (num : int) = match num with
																0 -> [5;10]
																|2 -> [0;4;7;10]
																|4 -> [0;2;4;6;9]
																|_ -> [0;2;4;6;8;10]
																		
		method private game_level (blocks : blocks) (score_max : int) (blocks_spawn : int list) = let rec aux bl cpt score score_max blocks_spawn =
													Sdlevent.pump ();

													self#draw_background;

													let t = Sys.time () in

													ba#move;
													bl#move;
													s#move;
													bo#move;

													let s1 = List.length blocks#get in

													s#collision_list bl#get;
													s#collision_list bo#get;
													ba#collision bl#get;

													ba#filter_exploded;
													bl#filter_exploded;
													bo#filter_exploded;

													let s2 = List.length blocks#get in

													ba#filter_out;
													bl#filter_out;
													bo#filter_out;

													bo#spawn_bonus;
													bl#spawn_block cpt blocks_spawn;

													ba#draw;
													bl#draw;
													s#draw;
													bo#draw;
													self#draw_score score score_max;
													s#draw_live;
													
													self#wait t;

													let (_,_,button) = Sdlmouse.get_state () in
													match (button, score, s#is_alive) with
													(_,_,false) -> false
													|(_,sc,_) when sc > score_max -> true
													|(x,_,_) when List.mem Sdlmouse.BUTTON_LEFT x -> ba#fire s#x (s#y - 10) s#b true (255,134,14) cpt s#f; aux bl (self#compt_fire cpt) (score + s1 - s2) score_max blocks_spawn
													|_ -> aux bl (self#compt_fire cpt) (score + s1 - s2) score_max blocks_spawn
												in aux blocks 0 0 score_max blocks_spawn
		method private game_boss (boss : boss) (boss_balls : balls) = let rec aux boss boss_balls cpt =
													Sdlevent.pump ();

													self#draw_background;

													let t = Sys.time () in
													ba#move;
													boss_balls#move;
													s#move;
													boss#move;
													bo#move;

													s#collision_list boss_balls#get;
													s#collision_list bo#get;
													boss#collision_balls ba#get;

													boss_balls#filter_exploded;
													ba#filter_exploded;
													bo#filter_exploded;

													boss_balls#filter_out;
													ba#filter_out;
													bo#filter_out;

													bo#spawn_bonus;
													boss_balls#fire (boss#x + boss#w / 2) (boss#y + boss#h) boss#b false (205,170,74) cpt [5;10];

													ba#draw;
													s#draw;
													bo#draw;
													boss#draw;
													boss_balls#draw;
													s#draw_live;
													boss#draw_lives;

													self#wait t;

													let (_,_,button) = Sdlmouse.get_state () in
													match (button, boss#is_alive , s#is_alive) with
													(_,_,false) -> false
													|(_,boss_alive,_) when not boss_alive -> true
													|(x,_,_) when List.mem Sdlmouse.BUTTON_LEFT x -> ba#fire s#x (s#y - 10) s#b true (255,134,14) cpt s#f; aux boss boss_balls (self#compt_fire cpt)
													|_ -> aux boss boss_balls (self#compt_fire cpt)
												in aux boss boss_balls 0
		method private give_level (num : int) = match num with
												num when num mod 2 = 0 -> self#game_level (new blocks [] r u (10 * (num + 1) / 2) scr) ((num + 1) * 100) (self#create_blocks_spawning_freq num)
												|_ -> self#game_boss (self#create_boss num) (new balls [] r u scr)
		method play = let rec aux (num : int) (fst_time : bool) =
						let win = self#give_level num in
						match (num, win) with
						(7,true) -> self#opening (not win) false; s <- new ship s#x s#y s#h s#w r u 10 (1,220,242) scr First [10]; ba <- new balls [] r u scr; bo <- new bonus_list [] r u scr; ()
						|(_,true) ->  aux (num + 1) false
						|_ -> self#opening (not win) false; s <- new ship s#x s#y s#h s#w r u 10 (1,220,242) scr First [10]; ba <- new balls [] r u scr; bo <- new bonus_list [] r u scr; aux 0 false
					in aux 0 true
		initializer self#init; self#opening true true
	end

let rec continue_without_quit (x : game) = x#play; continue_without_quit x


(* 
	ocamlc bigarray.cma -I +sdl sdl.cma sdlloader.cma sdlttf.cma sdlmixer.cma -thread -o ship ship.ml 
 *)

let () =
	Printf.printf "Enter Window's Width (Good ~500) = ";
	let r = read_int () in
	if r < 0 then failwith "Cannot be Negative";
	Printf.printf "Enter Window's Height (Good ~700) = ";
	let u = read_int () in
	if u < 0 then failwith "Cannot be Negative";
	Random.init 1234;
	let x = new game r u in
	continue_without_quit x

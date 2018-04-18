type ppm_file_channel = out_channel * bool

exception Error of string

let openfile ?(binary=true) hsize vsize name =
  try
    let c_out = open_out (name^".ppm") in
    let header = if binary then "P6" else "P3" in
    Printf.fprintf c_out "%s %d %d 255\n" header hsize vsize;
    (c_out,binary)
  with Sys_error s -> raise (Error ("File Error: "^s))

let closefile (c_out,_) = close_out c_out

let char0 = Char.code '0'

let output_ascii ch i =
  output_char ch (Char.chr (char0 + i/100));
  output_char ch (Char.chr (char0 + (i/10) mod 10));
  output_char ch (Char.chr (char0 + i mod 10))

let put_next_pixel (c_out,binary) (r,g,b) =
  if binary then
    begin
      output_byte c_out r;
      output_byte c_out g;
      output_byte c_out b
    end
  else
    begin
      output_ascii c_out r;
      output_char c_out ' ';
      output_ascii c_out g;
      output_char c_out ' ';
      output_ascii c_out b;
      output_char c_out '\n'
      (* NB: ne *pas* changer ce qui précède en un seul Printf.fprintf
         (40% plus lent sur l'exemple violet_ballon_on_grass.ray) *)
    end

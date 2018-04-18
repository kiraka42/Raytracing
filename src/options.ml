let filename = ref ""
let hsize = ref 200
let vsize = ref 200
let depth = ref 5
let anim = ref 0

let options_list = ref []

let set_filename f = filename := f

let set_hsize h = hsize := h

let set_vsize v = vsize := v

let set_depth d = depth := d

let set_anim a = anim := a

let generic_options = 
  [
    ("-hsize",
     Arg.Int set_hsize,
     "Set the horizontal size of screen"
    );
    ("-vsize",
     Arg.Int set_vsize,
     "Set the vertical size of screen");
    ("-depth",
     Arg.Int set_depth,
     "Set the max number of bounces for a ray");
    ("-anim",
     Arg.Int set_anim,
     "Set the number of images");
  ]

let usage_msg =
  "./ray.native filename [options]"

let parse () =
  Arg.parse !options_list set_filename usage_msg

let initialize =
  options_list := !options_list @ generic_options

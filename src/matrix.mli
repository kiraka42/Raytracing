type line =
{ 
  c1 : float; 
  c2 : float; 
  c3 : float; 
}

type matrix = 
{ 
  l1 : line; 
  l2 : line; 
  l3 : line; 
}

val make : 
  (float * float * float) 
  -> (float * float * float) 
  -> (float * float * float) 
  -> matrix

val id : matrix

val prod : matrix -> matrix -> matrix

val print : matrix -> unit

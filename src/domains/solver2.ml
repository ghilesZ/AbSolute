module Make (D:Domain_signature2.AbstractCP) = struct

  let solve (abs:D.t) =
    let rec aux abs =
      let filtered = D.propagation abs in
      if D.is_solution filtered then filtered
      else if D.is_small filtered then filtered
      else failwith ""
    in ()
end

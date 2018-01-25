module Make (D:Domain_signature2.AbstractCP) = struct

  (* Module that handles few informations over the result of the solving *)
  include Result.Make(D)

  (* propagation / exploration loop *)
  let explore (abs:D.t) =
    let rec aux abs res depth =
      match D.consistency abs with
      | D.Empty       -> res
      | D.Full abs'   -> add_s res abs'
      | D.Maybe (frontier, abs') ->
         if D.is_small abs' || depth >= !Constant.max_iter
         then add_u res abs'
         else
           List.fold_left (fun res elem ->
               aux elem (incr_step res) (depth +1)
	           ) res (D.exploration abs')
    in
    aux abs empty_res 0

  (* propagation / elimination / exploration loop *)
  let explore_pruning (abs:D.t) =
    let rec aux abs res depth =
      match D.consistency abs with
      | D.Empty       -> res
      | D.Full abs'   -> add_s res abs'
      | D.Maybe (frontier, abs') ->
         if D.is_small abs' || depth >= !Constant.max_iter
         then add_u res abs'
         else
           let ls,lu = D.prune abs' frontier in
           let res = List.fold_left (fun r x -> add_s r x) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux elem (incr_step res) (depth +1)
                 ) res (D.exploration x)
	           ) res lu
    in
    aux abs empty_res 0

  let solve prob =
    let abs = D.init prob in
    if !Constant.pruning then explore_pruning abs
    else explore abs
end

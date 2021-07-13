let merge_threads (layers_a : Types.layer list) layers_b =
  let is_mergeable (a : Types.layer) (b : Types.layer) = 
    DMC.Thread.equal a.Types.thread b.Types.thread &&
    Types.equal_stitch a.stitch b.stitch
  in  
  let merge (a : Types.layer) (b : Types.layer) =
    {a with stitches = Types.CoordinateSet.union a.stitches b.stitches}
  in  
  List.fold_left (fun deduplicated layer ->
      match List.partition (is_mergeable layer) deduplicated with
      | [], deduplicated -> layer :: deduplicated
      | duplicates, sans_duplicates ->
        (* there shouldn't be more than one duplicate here, but it's not hard
           to do the right thing if there is, so let's just go for it *)
        let new_layer = List.fold_left (fun l dupe -> merge l dupe) layer duplicates in
        new_layer :: sans_duplicates
    ) [] (layers_a @ layers_b)

let merge_backstitch_threads (bs_a : Types.backstitch_layer list) bs_b =
  let is_mergeable a b = 
    DMC.Thread.equal a.Types.thread b.Types.thread
  in  
  let merge (a : Types.backstitch_layer) (b : Types.backstitch_layer) =
    {a with Types.stitches = Types.SegmentSet.union a.stitches b.stitches}
  in  
  List.fold_left (fun deduplicated layer ->
      match List.partition (is_mergeable layer) deduplicated with
      | [], deduplicated -> layer :: deduplicated
      | duplicates, sans_duplicates ->
        let new_layer = List.fold_left (fun l dupe -> merge l dupe) layer duplicates in
        new_layer :: sans_duplicates
    ) [] (bs_a @ bs_b)

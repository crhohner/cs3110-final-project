open Model

module type CPUType = sig
  val sort_by_num : tile list -> tile list
  val sort_by_color : tile list -> tile list
  val check_threes : tile list -> tile list option
  val check_pairs : tile list -> tile list list
  val place_three : tile list list -> tile list -> tile list list
  val place_pair : tile list list -> tile list -> tile list list
  val place_one : tile list list -> tile -> tile list list
  val turn : player -> tile list list -> player * tile list list
end

module CPUType = struct
  let sort_by_num l = failwith "unimplemented"
  let sort_by_color l = failwith "unimplemented"
  let check_threes l = failwith "unimplmeneted"
  let check_pairs l = failwith "unimplemented"
  let place_three b l = failwith "unimplemented"
  let place_pair b l = failwith "unimplemented"
  let place_one b t = failwith "unimplemented"
  let turn p b = failwith "unimplemented"
end

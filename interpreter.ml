open Printf

module type Tape = sig
  type t
  val init : unit -> t
  val move_right : t -> t
  val move_left : t -> t
  val increment : t -> t
  val decrement : t -> t
  val read : t -> int
  val write : t -> int -> t
  val print : t -> string
  val print_span : t -> int -> string
end

module DataTape : Tape = struct

  type t = {
        left: int list;
        current: int;
        right: int list;
    }

  let init () = { left = []; current = 0; right = [] }

  let move_right tape =
    match tape.right with
    | [] -> { left = tape.current :: tape.left; current = 0; right = [] }
    | x :: xs -> { left = tape.current :: tape.left; current = x; right = xs }

  let move_left tape =
    match tape.left with
    | [] -> raise (Failure "Cannot move left from the initial position")
    | x :: xs -> { left = xs; current = x; right = tape.current :: tape.right }

  let increment tape = { tape with current = if tape.current != 255 then tape.current + 1 else 0 }

  let decrement tape = { tape with current = if tape.current != 0 then tape.current - 1 else 255 }

  let read tape = tape.current

  let write tape value = { tape with current = value }

  let print_span tape n =
    let left_span = String.concat "|" (List.map string_of_int (List.rev (List.take n tape.left))) in
    let right_span = String.concat "|" (List.map string_of_int (List.take n tape.right)) in
    Printf.sprintf "%s|{%d}|%s" left_span tape.current right_span

  let print tape =
    print_span tape (max (List.length tape.left) (List.length tape.right))

end

module InstructionTape = struct
    type t = {
        tape: string;
        pointer: int;
      }

    let size tape = String.length tape.tape

    let is_valid_right_move tape =
      tape.pointer < size tape

    let next_index tape =
      if is_valid_right_move tape then
        min (tape.pointer + 1) (size tape - 1)
      else
        tape.pointer
    (* print the current in bold the others around as regular *)
    let print_span tape n =
        let left_span = String.sub tape.tape (max 0 (tape.pointer - n)) (min n tape.pointer) in
        let right_span =
          if is_valid_right_move tape then
            String.sub tape.tape (next_index tape) (min n (size tape - tape.pointer - 1))
          else
            ""
        in
        let current_char = if is_valid_right_move tape then String.get tape.tape tape.pointer else '!' in
        Printf.sprintf "%s{%c}%s" left_span current_char right_span

    let init s = { tape = s; pointer = 0 }
    let move_right tape =
      if tape.pointer <= String.length tape.tape - 1 then
        { tape with pointer = tape.pointer + 1 }
      else
        tape (* Stay at the end of the tape if we try to move right beyond it *)

    let move_left tape =
        if tape.pointer > 0 then
            { tape with pointer = tape.pointer - 1 }
        else
          raise (Failure "Cannot move left from the initial position")

    let read tape =
      if tape.pointer <= String.length tape.tape - 1 then
        Some (String.get tape.tape tape.pointer)
      else
        None


    let find_matching_bracket_right tape =
      let rec find_matching pointer depth =
            if pointer >= String.length tape.tape then
                None
            else
                let c = String.get tape.tape pointer in
                if c = '[' then
                    find_matching (pointer + 1) (depth + 1)
                else if c = ']' then
                    if depth = 0 then Some pointer
                    else find_matching (pointer + 1) (depth - 1)
                else
                    find_matching (pointer + 1) depth
        in
        find_matching (tape.pointer + 1) 0

    let find_matching_bracket_left tape =
      let rec find_matching pointer depth =
            if pointer < 0 then
                None
            else
                let c = String.get tape.tape pointer in
                if c = ']' then
                    find_matching (pointer - 1) (depth + 1)
                else if c = '[' then
                    if depth = 0 then Some pointer
                    else find_matching (pointer - 1) (depth - 1)
                else
                    find_matching (pointer - 1) depth
        in
        find_matching (tape.pointer - 1) 0

    let jump_right tape =
        match find_matching_bracket_right tape with
        | Some new_pointer -> { tape with pointer = new_pointer + 1 }
        | None -> raise (Failure "Unmatched '['")

    let jump_left tape =
        match find_matching_bracket_left tape with
        | Some new_pointer -> { tape with pointer = new_pointer + 1 }
        | None -> raise (Failure "Unmatched ']'")

end

module BrainfuckInterpreter = struct

  let init program = (DataTape.init (), InstructionTape.init program)

  let step (tape, program) =
    let command = InstructionTape.read program in
    if command = None then
      raise (Failure "End of program reached")
    else
    let command = Option.get command in
    let new_tape =
      match command with
        | '>' -> DataTape.move_right tape 
        | '<' -> DataTape.move_left tape 
        | '+' -> DataTape.increment tape 
        | '-' -> DataTape.decrement tape
        | '.' -> let value = DataTape.read tape in
                 Printf.printf "%d\n" value;
                 tape
        | ',' -> let input = read_line () in
                 let v = match int_of_string_opt input with
                 | None -> print_endline "Invalid input, writing 0 to tape"; 0
                 | Some value -> value
                 in
                 if v < 0 || v > 255 then
                   (Printf.printf "Input out of range (0-255), writing 0 to tape\n"; DataTape.write tape 0)
                 else
                    DataTape.write tape v
        | '?' -> let data_str = DataTape.print tape in
                 Printf.printf "data: %s\n" data_str;
                 let instr_str = InstructionTape.print_span program 10 in
                 Printf.printf "instr: %s\n" instr_str;
                 let _ = read_line() in tape
        (* '[' and  ']' doenst modify tape so skip it  *)
        | _ -> tape
    in
    let new_program =
      if command = '[' && DataTape.read tape = 0 then
        InstructionTape.jump_right program
      else if command = ']' && DataTape.read tape <> 0 then
        InstructionTape.jump_left program
      else
        InstructionTape.move_right program
    in
      (new_tape, new_program) 

end

let pprint (tape, program) =
  let data_str = DataTape.print tape in
  Printf.printf "Data Tape: %s\n" data_str;

  let instr_str = InstructionTape.print_span program 10 in
  Printf.printf "Instruction Tape: %s\n" instr_str;

  (tape, program)

let run (tape, program) =
  let rec go n (tape, program) =
    Printf.printf "Step %d:\n" n;
    let _ = pprint (tape, program) in
    if InstructionTape.read program = None then
      (tape, program)
    else
      BrainfuckInterpreter.step (tape, program) |> go (n + 1)
  in
    go 0 (tape, program)


module Examples = struct
  let move = ",>>[-]<<[->>+<<]"
end

module Max = struct
  (* mem_layout *)
  (*0 | 1 | a | a_1 | a_2 | underflow_flag | b_2 | b_1 | b *)
  (* notice underflow flag will be 1 if not underflowed *)

  (* a b a_1 b_1 a_2 b_2 0 1 *)
  let read = ","
  let init = read ^ ">" ^ read
  let copy = "[->>+>>+<<<<]"
  let zero_case = "+>+<"
  (* let lop = "+[<[?-<]>>]" *)
  (* r1 r2 r3 i1 i2 *)
  (* +[>>>>[-<]<<]<[...] *)
  let lop = ">>+[<<<<[->]>>]"
  let x = ">[-<<<<<<.>>>>>>]>[-<<<<<.>>>>>]"
  let program = String.concat "" [
        init;
        "<";
        copy;
        ">";
        copy;
        ">>>>>";
        lop;
        x;
  ]

  (* let move_ptr_a_to_b = ">>>>>>" *)
  (* let move_ptr_b_to_a = "<<<<<<" *)

  (* (\* after init *\) *)
  (* (\* a | 0 | 0 | 0 | 0 | b *\) *)
  (* let init = ">+>" ^ read ^ move_ptr_a_to_b ^ read *)

  (* (\* assumes pointing to a *\) *)
  (* let copy_a = "[->+>+<<]" *)

  (* (\* assumes pointing to b *\) *)
  (* let copy_b = "[-<+<+>>]" *)

  (* let setup_loop = ">>>" *)

  (* (\* if b = 0 then move back to a *\) *)
  (* (\* else setp flag and move distance away from a *\) *)

  (* let decrement_tmps = *)
  (*   (\* while a != 0 *\) *)
  (*   (\*   a-- *\) *)
  (*   (\*   while --b != 0 *\) *)
  (*   (\*     move_b *\) *)
  (*   (\*  *\) *)
  (*   "[<[->+>-[?<-<<<]<<]]" *)

  (* let program = String.concat "" [ *)
  (*       init; *)
  (*       copy_b; *)
  (*       move_ptr_b_to_a; *)
  (*       copy_a; *)
  (*       setup_loop; *)
  (*       decrement_tmps; *)
        
  (* ] *)
end 

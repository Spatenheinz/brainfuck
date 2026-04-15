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

module type IO = sig
  type t
  type s
  val init : s -> t
  val read : t -> int option * t
  val write : int -> t -> t
end

module RealIO : IO = struct
  type t = unit
  type s = unit
  let init () = ()
  let write value () =
    Printf.printf "%d\n" value;
    ()
  let read () =
    let input = read_line () in
    int_of_string_opt input, ()
end

module TestIO : (IO with type s = int list) = struct
  type t = {
    inputs: int list;
    outputs: int list;
  }
  type s = int list

  let init inputs = { inputs = inputs; outputs = [] }

  let read state =
    match state.inputs with
    | [] -> failwith "TestIO: No more input to read from"
    | x :: rest ->
        let value = if 0 <= x && x <= 255 then Some x else None in
        value, { state with inputs = rest }
  
  let write v state = { state with outputs = v :: state.outputs }
end

type 'io exec_state =
  | Running of DataTape.t * InstructionTape.t * 'io
  | Halted of DataTape.t * InstructionTape.t * 'io

module MakeBrainfuckInterpreter (IO: IO) = struct
  type input_state = IO.s

  let init prog state =
    Running (DataTape.init (), InstructionTape.init prog, IO.init state)

  let step = function
    | Halted (_,_,_) as final -> final
    | Running (tape, program, io) ->
    match InstructionTape.read program with
    | None -> Halted (tape, program, io)
    | Some command ->
      let (new_tape, next_io) =
        match command with
          | '>' -> DataTape.move_right tape, io
          | '<' -> DataTape.move_left tape, io
          | '+' -> DataTape.increment tape, io
          | '-' -> DataTape.decrement tape, io
          | ',' -> let (input, new_state) = IO.read io in
                  let value = match input with
                    | None -> print_endline "Invalid input, writing 0 to tape"; 0
                    | Some v -> v
                  in
                  DataTape.write tape value,  new_state
          | '.' -> let value = DataTape.read tape in
                  let new_state = IO.write value io in
                  tape, new_state
          | '?' -> let data_str = DataTape.print tape in
                  Printf.printf "data: %s\n" data_str;
                  let instr_str = InstructionTape.print_span program 10 in
                  Printf.printf "instr: %s\n" instr_str;
                  let _ = read_line() in
                  tape, io
          (* '[' and  ']' doenst modify tape so skip it *)
          | _ -> tape, io
      in
      let new_program =
        if command = '[' && DataTape.read tape = 0 then
          InstructionTape.jump_right program
        else if command = ']' && DataTape.read tape <> 0 then
          InstructionTape.jump_left program
        else
          InstructionTape.move_right program
      in
      Running (new_tape, new_program, next_io)

end

let pprint (tape, program) =
  let data_str = DataTape.print tape in
  Printf.printf "Data Tape: %s\n" data_str;

  let instr_str = InstructionTape.print_span program 10 in
  Printf.printf "Instruction Tape: %s\n" instr_str;

module TestInterpreter = MakeBrainfuckInterpreter(TestIO)

let run initial =
  let rec go n state =
    Printf.printf "Step %d:\n" n;
    match TestInterpreter.step state with
    | Halted (tape, program, io_state) ->
       pprint (tape, program);
       (tape, program, io_state)
    | Running (tape, program, io_state) as s ->
       pprint (tape,program);
       go (n + 1) s
  in
    go 0 initial


module Examples = struct
  let move = ",>>[-]<<[->>+<<]"
  let max =
    let init = "+>>>>,>," in
    let count_smallest = "[-<[-<<]<<[>>]>+>>]" in
    let count_largest = "<[-<+>]" in
    let print = "<." in
    String.concat "" [init; count_smallest; count_largest; print]
  let max2 =
    "+>>,>,[-<[-<]<[>]>>>+<]<[->>+<<]>>."
  let max3 =
    "+>>,>,[-<[-<]<?[+>]>>?]<[-<<+>>]<<-."
  let max4 =
    ",[->+>+<<]?,[->+>-<<]?>[->>+<-[<]]?"
(* >>,>,[-<[-<]<+>[>]?>>]<[-<<+>>]<<. *)
end

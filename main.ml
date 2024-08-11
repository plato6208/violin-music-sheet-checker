type note_modifier = Sharp | Minor | Neutral

type note = {
  pitch: string;
  modifier: note_modifier option;
  note_type: string;
}

type 'a result = Ok of 'a | Error of string

type instrument = string
type dynamic_marking = string

let parse_note_type = function
  | "quarter" -> 1
  | "half" -> 2
  | "whole" -> 4
  | _ -> failwith "Invalid note type"

let parse_note_modifier = function
  | "#" -> Some Sharp
  | "n" -> Some Neutral
  | "-" -> Some Minor
  | _ -> None

let parse_note (pitch, modifier, note_type) =
  {
    pitch;
    modifier = parse_note_modifier modifier;
    note_type;
  }

let is_valid_violin_note note =
  let valid_notes = ["A"; "B"; "C"; "D"; "E"; "F"; "G"] in
  List.mem note.pitch valid_notes

let bar time_signature notes =
  let rec validate_notes remaining_beats = function
    | [] -> if remaining_beats = 0 then Ok () else Error "Notes don't match time signature"
    | note :: rest ->
        let note_duration = parse_note_type note.note_type in
        let remaining_beats_after_note = remaining_beats - note_duration in
        if remaining_beats_after_note < 0 || not (is_valid_violin_note note) then
          Error "Notes don't match time signature or are not valid for the violin"
        else
          validate_notes remaining_beats_after_note rest
  in

  let time_signature_beats = fst time_signature in
  let time_signature_type = snd time_signature in

  if time_signature_type = 4 (* Assuming 4/4 time signature for simplicity *) then
    validate_notes time_signature_beats (List.map parse_note notes)
  else
    Error "Invalid time signature"

let line instrument dynamic bars =
  let rec validate_bars = function
    | [] -> Printf.printf "Line is valid!\n"
    | (time_signature, notes) :: rest ->
        match bar time_signature notes with
        | Ok () -> validate_bars rest
        | Error message ->
            Printf.printf "Invalid bar details: %s, time signature: %s, instrument: %s, dynamic: %s\n"
              message
              (string_of_int (fst time_signature) ^ "/" ^ string_of_int (snd time_signature))
              instrument
              dynamic
            ;
            Printf.printf "Invalid line: %s\n" message
  in

  validate_bars bars

(* Example usage with an invalid line for a violin with dynamic "ff" *)
let () =
  line "Violin" "ff" [
    ((4, 4), [("A", "#", "quarter"); ("D", "-", "quarter"); ("E", "n", "half"); ("G", "#", "quarter")]); 
    ((4, 4), [("C", "#", "quarter"); ("D", "-", "half"); ("E", "n", "half"); ("G", "#", "quarter")]); 
  ];;

(* Example usage with a valid line for a violin with dynamic "p" *)
let () =
  line "Violin" "p" [
    ((4, 4), [("A", "#", "quarter"); ("D", "-", "quarter"); ("E", "n", "half");]); 
    ((4, 4), [("D", "-", "half"); ("E", "n", "half");]); 
  ];;

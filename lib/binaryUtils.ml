exception Invalid_input

let crc32 (input: bytes) =
    let divisor = 0xEDB88320 in
    let rec process_bytes (crc: int) (input: bytes) (index: int): int= match index with
        | _ when index = (Bytes.length input) -> crc lxor 0xFFFFFFFF
        | _ -> let byte = int_of_char (Bytes.get input index) in
            let byte_crc = crc lxor byte in
            let rec process_bits bits_crc remaining_bits = match remaining_bits with
                    | _ when remaining_bits = 0 -> bits_crc 
                    | _ -> process_bits (if (bits_crc land 1) <> 0 then (bits_crc lsr 1) lxor divisor else bits_crc lsr 1) (remaining_bits-1) in
            process_bytes (process_bits byte_crc 8) input (index+1) in

    process_bytes (0xFFFFFFFF) (input) 0

(* Big Endian *)
let bytes_of_int ~(value:int) (number_bytes:int) = 
    let rec loop (num:int) (i:int) (n_bytes:int) (bytes:Bytes.t) = match i with
        | _ when i = n_bytes -> bytes
        | _ -> let byte = ((num lsr ((n_bytes-i-1) * 8)) land 0xFF) in
            Bytes.set bytes i (Char.chr byte);
            loop num (i+1) n_bytes bytes 
        in

    (* Check can be represented *)
    if ((float_of_int 2) ** (float_of_int (number_bytes * 8))) <= (float_of_int value) then raise Invalid_input;
    (* Sense check for correctness of inputs*)
    if (number_bytes > 1000000) then raise Invalid_input;

    loop value 0 number_bytes (Bytes.make number_bytes (Char.chr 0))

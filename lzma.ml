open Core
open Ctypes
open PosixTypes
open Foreign

(* Most of the helper functions below are part of lib/util.ml *)

let ba_create size =
    Bigarray.Array1.create Bigarray.char Bigarray.c_layout size

let ba_create_zeroed size =
    let ba = ba_create size in
    Bigarray.Array1.fill ba '\x00';
    ba

(* ----------------------------------------------------------------------------- *)
let unwrap_int opt =
    match opt with
    | Some v -> v
    | None -> 0

(* TODO: More efficient primitives *)
let int64_BE_of buf int_of_pos =
    let rec loop buf i n =
        if i = 8 then n
        else
            let b = Int64.of_int (int_of_pos buf i) in
            let n' = Caml.Int64.logor (Int64.shift_left n 8) b in
            loop buf (i + 1) n'
    in
    loop buf 0 Int64.zero

let int64_of_bigarray_BE ba =
    let int_of_pos ba offset =
        Caml.Char.code (Bigarray.Array1.get ba offset)
    in
    int64_BE_of ba int_of_pos

let int64_of_bytes_BE buf =
    let int_of_pos buf offset =
        Caml.Char.code (Bytes.get buf offset)
    in
    int64_BE_of buf int_of_pos

let int64_of_string_BE buf =
    let int_of_pos buf offset =
        Caml.Char.code (String.get buf offset)
    in
    int64_BE_of buf int_of_pos

(* ----------------------------------------------------------------------------- *)

let int64_LE_of buf int_of_pos =
    let rec loop buf i n =
        if i = 0 then n
        else
            let b = Int64.of_int (int_of_pos buf (i - 1)) in
            let n' = Caml.Int64.logor (Int64.shift_left n 8) b in
            loop buf (i - 1) n'
    in
    loop buf 8 Int64.zero

let int64_of_bigarray_LE ba =
    let int_of_pos ba offset =
        Caml.Char.code (Bigarray.Array1.get ba offset)
    in
    int64_LE_of ba int_of_pos

let int64_of_bytes_LE buf =
    let int_of_pos buf offset =
        Caml.Char.code (Bytes.get buf offset)
    in
    int64_LE_of buf int_of_pos

let int64_of_string_LE buf =
    let int_of_pos buf offset =
        Caml.Char.code (String.get buf offset)
    in
    int64_LE_of buf int_of_pos

(* ----------------------------------------------------------------------------- *)

let int32_BE_of buf int_of_pos =
    let rec loop buf i n =
        if i = 4 then n
        else
            let b = Caml.Int32.of_int (int_of_pos buf i) in
            let n' = Caml.Int32.logor (Int32.shift_left n 8) b in
            loop buf (i + 1) n'
    in
    loop buf 0 Int32.zero

let int32_of_bigarray_BE ba =
    let int_of_pos ba offset =
        Caml.Char.code (Bigarray.Array1.get ba offset)
    in
    int32_BE_of ba int_of_pos

let int32_of_bytes_BE buf =
    let int_of_pos buf offset =
        Caml.Char.code (Bytes.get buf offset)
    in
    int32_BE_of buf int_of_pos

let int32_of_string_BE buf =
    let int_of_pos buf offset =
        Caml.Char.code (String.get buf offset)
    in
    int32_BE_of buf int_of_pos

(* ----------------------------------------------------------------------------- *)

let int32_LE_of buf int_of_pos =
    let rec loop buf i n =
        if i = 0 then n
        else
            let b = Caml.Int32.of_int (int_of_pos buf (i - 1)) in
            let n' = Caml.Int32.logor (Int32.shift_left n 8) b in
            loop buf (i - 1) n'
    in
    loop buf 4 Int32.zero

let int32_of_bigarray_LE ba =
    let int_of_pos ba offset =
        Caml.Char.code (Bigarray.Array1.get ba offset)
    in
    int32_LE_of ba int_of_pos

let int32_of_bytes_LE buf =
    let int_of_pos buf offset =
        Caml.Char.code (Bytes.get buf offset)
    in
    int32_LE_of buf int_of_pos

let int32_of_string_LE buf =
    let int_of_pos buf offset =
        Caml.Char.code (String.get buf offset)
    in
    int32_LE_of buf int_of_pos

(* ----------------------------------------------------------------------------- *)
(* TODO: Wait for the fix in Ctypes library
 * see https://github.com/ocamllabs/ocaml-ctypes/issues/476 *)

let add_gc_link ~from ~to_=
    let r = ref (Some (Obj.repr to_)) in
    let finaliser _ = r := None in
    Caml.Gc.finalise finaliser from

let safe_setf s f v =
    add_gc_link ~from:s ~to_:v;
    Ctypes.setf s f v

let safe_getf = Ctypes.getf

let safe_coerce t1 t2 ptr =
    let newptr = Ctypes.coerce t1 t2 ptr in
    add_gc_link ~from:newptr ~to_:ptr;
    newptr

(* TODO: Upstream this to Ctypes library
 * see https://github.com/ocamllabs/ocaml-ctypes/pull/573 *)

let carray_to_string a =
    let len = CArray.length a in
    let bytes = Bytes.create len in
    for i = 0 to len - 1 do
        Bytes.set bytes i (char_of_int
            (Unsigned.UInt8.to_int (CArray.unsafe_get a i)))
    done;
    Bytes.unsafe_to_string bytes

(* -------------------------------------------------------------------------------- *)
type data = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* LZMA types *)

(* ALLOC_FUNC *)
(* typedef void * ISzAlloc_Alloc(
    void *p, // ISzAlloc structure in fact, wtf
    size_t size);
 *)
let allocfunc_t = ptr void @-> size_t @-> returning (ptr void)

(* FREE_FUNC *)
(* typedef void ISzAlloc_Free(
    void *p, // IszAlloc structure in fact, wtf
    void *address);
 *)
let freefunc_t = ptr void @-> ptr void @-> returning void

type iszalloc
let iszalloc : iszalloc structure typ = structure "ISzAlloc"

let allocator_alloc = field iszalloc "Alloc" (funptr allocfunc_t)
let allocator_free = field iszalloc "Free" (funptr freefunc_t)

let () = seal iszalloc

type clzma_props
let clzma_props : clzma_props structure typ = structure "CLzmaProps"

let props_lc = field clzma_props "lc" uint8_t
let props_lp = field clzma_props "lp" uint8_t
let props_pb = field clzma_props "pb" uint8_t
let props_pad = field clzma_props "_pad_" uint8_t
let props_dicsize = field clzma_props "dicSize" uint32_t

let () = seal clzma_props

type clzma_prob = Unsigned.uint16 (* sometimes uint32_t *)

type clzma_dec
let clzma_dec : clzma_dec structure typ = structure "CLzmaDec"

let dec_prop = field clzma_dec "prop" clzma_props
let dec_probs = field clzma_dec "probs" (ptr uint16_t) (* clzma_prob *)
let dec_probs_1664 = field clzma_dec "probs_1664" (ptr uint16_t) (* clzma_prob *)
let dec_dic = field clzma_dec "dic" (ptr uint8_t)
let dec_dicbufsize = field clzma_dec "dicBufSize" size_t
let dec_dicpos = field clzma_dec "dicPos" size_t
let dec_buf = field clzma_dec "buf" (ptr void)
let dec_range = field clzma_dec "range" uint32_t
let dec_code = field clzma_dec "code" uint32_t
let dec_processsedpos = field clzma_dec "processedPos" uint32_t
let dec_checkdicsize = field clzma_dec "checkDicSize" uint32_t
let dec_reps = field clzma_dec "reps" (array 4 uint32_t)
let dec_state = field clzma_dec "state" uint32_t
let dec_remainlen = field clzma_dec "remainLen" uint32_t
let dec_numprobs = field clzma_dec "numProbs" uint32_t
(* See LzmaDec.h - who the hell uses unspecified size in such a structure!? *)
let dec_tempBufsize = field clzma_dec "tempBufSize" uint32_t (* is it int32 or int64? *)
let dec_tempbuf = field clzma_dec "tempBuf" (array 20 uint8_t)

let () = seal clzma_dec

type elzma_finish_mode =
    | LZMA_FINISH_ANY
    | LZMA_FINISH_END
    [@@deriving enum]

type elzma_status =
    | LZMA_STATUS_NOT_SPECIFIED
    | LZMA_STATUS_FINISHED_WITH_MARK
    | LZMA_STATUS_NOT_FINISHED
    | LZMA_STATUS_NEEDS_MORE_INPUT
    | LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
    [@@deriving enum]

type lzma_sres =
    | SZ_OK
    | SZ_ERROR_DATA
    | SZ_ERROR_MEM
    | SZ_ERROR_CRC
    | SZ_ERROR_UNSUPPORTED
    | SZ_ERROR_PARAM
    | SZ_ERROR_INPUT_EOF
    | SZ_ERROR_OUTPUT_EOF
    | SZ_ERROR_READ
    | SZ_ERROR_WRITE
    | SZ_ERROR_PROGRESS
    | SZ_ERROR_FAIL
    | SZ_ERROR_THREAD
    | SZ_ERROR_ARCHIVE [@value 16]
    | SZ_ERROR_NO_ARCHIVE [@value 17]
    [@@deriving enum]

(* Some constants from headers *)
let lzma_props_size = 5
let lzma_uncompressed_length_size = 8
let lzma_uncompressed_length_size_legacy = 4
let in_buf_size = 1 lsl 16
let out_buf_size = 1 lsl 16

module Internal_LZMA_Header = struct
    (* LZMA compression header (modern format) *)
    [%%cstruct
    type lzma_header = {
        properties: uint8_t;
        dict_size: uint32_t;
        outlen: uint64_t;
    } [@@bi_endian]
    ]
end

module Internal_LZMA_Header_Legacy = struct
    (* LZMA compression header (legacy format) *)
    [%%cstruct
    type lzma_header_legacy = {
        properties: uint8_t;
        dict_size: uint32_t;
        outlen: uint32_t;
    } [@@bi_endian]
    ]
end

type headers = {
    properties: int;
    dict_size: int32;
    outlen: int64;
}

(* -------------------------------------------------------------------------------- *)
(*
 * void LzmaDec_Init(CLzmaDec *p)
 *
 *)
let lzmadec_init =
    foreign "LzmaDec_Init" (ptr clzma_dec @-> (returning void))

(*
 * void LzmaDec_Allocate(CLzmaDec *p, const Byte *props, unsigned propsSize,
 *                      ISzAllocPtr alloc)
 *)
let lzmadec_allocate =
    foreign "LzmaDec_Allocate" (ptr clzma_dec @-> ptr char @-> uint32_t @->
                                ptr iszalloc @-> (returning void))

(*
 * void LzmaDec_Free(CLzmaDec *p, ISzAllocPtr alloc)
 *
 *)
let lzmadec_free =
    foreign "LzmaDec_Free" (ptr clzma_dec @-> ptr iszalloc @-> (returning void))

(*
 * SRes (int) LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, size_t *destLen,
 *                      const Byte *src, size_t *srcLen,
 *                      ELzmaFinishMode finishMode, ELzmaStatus *status)
 *)
let lzmadec_decode2buf =
    foreign "LzmaDec_DecodeToBuf" (ptr clzma_dec @-> ptr uint8_t @-> ptr size_t @->
                                ptr uint8_t @-> ptr size_t @->
                                uint32_t @-> ptr uint32_t @-> (returning int))


(* g_Alloc structure from Alloc.h *)
let lzmadec_galloc =
    foreign_value "g_Alloc" iszalloc

(* -------------------------------------------------------------------------------- *)
let alloc_clzmadec () =
    let clzmadecp = allocate_n clzma_dec ~count:1 in
    if not (is_null clzmadecp) then Some clzmadecp
    else None

let lzma_init header =
    match alloc_clzmadec () with
    | Some clzmadecp ->
        let clzmadec = !@ clzmadecp in
        (* #define LzmaDec_Construct(p) { (p)->dic = NULL; (p)->probs = NULL; } *)
        setf clzmadec dec_dic (from_voidp uint8_t null);
        setf clzmadec dec_probs (from_voidp uint16_t null); (* clzma_prob *)
        let props_size = Unsigned.UInt32.of_int lzma_props_size in
        let hdr = Ctypes.bigarray_start array1 header in
        lzmadec_allocate clzmadecp hdr props_size lzmadec_galloc;
        Ok clzmadecp
    | None ->
        Error 0

let lzma_deinit lzma_state =
    lzmadec_free lzma_state lzmadec_galloc

(* TODO: Are you sure what endian to use here? *)
let lzma_parse_headers header =
    let open Internal_LZMA_Header.LE in (* just using little endian seems enough *)
    let header_cs = Cstruct.of_bigarray header in
    {
        properties = get_lzma_header_properties header_cs;
        dict_size = get_lzma_header_dict_size header_cs;
        outlen = get_lzma_header_outlen header_cs;
    }

let dump_headers headers =
    Lwt_io.printf "headers {\n";
    Lwt_io.printf "  properties = 0x%x\n" headers.properties;
    Lwt_io.printf "  dict_size = 0x%lx\n" headers.dict_size;
    Lwt_io.printf "  outlen = 0x%Lx\n" headers.outlen;
    Lwt_io.printf "}\n"

(* Fix up the headers if legacy format is met *)
(* All modern implementations store size as 8 byte integer (uint64)
 * Some legacy ones store size as 4 byte integer (uint32)
 * *)
let lzma_read_headers (ba:data) legacy =
    if legacy then begin
        let uif x = unwrap_int (Int32.to_int x) in
        (* Read header in legacy format and convert into the new one *)
        let open Internal_LZMA_Header_Legacy.LE in
        let legacy_header = Bigarray.Array1.sub ba 0 sizeof_lzma_header_legacy in
        let legacy_header_cs = Cstruct.of_bigarray legacy_header in
        let properties = get_lzma_header_legacy_properties legacy_header_cs in
        let dict_size = get_lzma_header_legacy_dict_size legacy_header_cs in
        let outlen = uif (get_lzma_header_legacy_outlen legacy_header_cs) in
        (* Header should be in the little endian format *)
        let lzma_header = ba_create_zeroed 13 in (* byte + int + long long *)
        let lzma_header_cs = Cstruct.of_bigarray lzma_header in
        (* Form the header *)
        let open Internal_LZMA_Header.LE in (* just using little endian seems enough *)
        set_lzma_header_properties lzma_header_cs properties;
        set_lzma_header_dict_size lzma_header_cs dict_size;
        set_lzma_header_outlen lzma_header_cs (Int64.of_int outlen);
        let lzma_header' = Cstruct.to_bigarray lzma_header_cs in
        lzma_header'
    end else begin
        let open Internal_LZMA_Header.LE in
        Bigarray.Array1.sub ba 0 sizeof_lzma_header
    end

(* Skip the header according if legacy or not *)
let lzma_read_data (ba:data) legacy =
    let ba_size = Bigarray.Array1.dim ba in
    if legacy then
        let open Internal_LZMA_Header_Legacy.LE in
        Bigarray.Array1.sub ba sizeof_lzma_header_legacy
            (ba_size - sizeof_lzma_header_legacy)
    else
        let open Internal_LZMA_Header.LE in
        Bigarray.Array1.sub ba sizeof_lzma_header
            (ba_size - sizeof_lzma_header)


(* TODO: Add also bytes and string functions *)
(* It is utter crap, refactor this! *)
let lzma_decompress_auto_ba (ba:data) ?legacy:(legacy=false) =
    let ba_size = Bigarray.Array1.dim ba in
    (* Prepare the header first *)
    let header = lzma_read_headers ba legacy in
    let headers = lzma_parse_headers header in
    dump_headers headers;
    (* Read the uncompressed data size *)
    let data = lzma_read_data ba legacy in
    let data_size = Bigarray.Array1.dim data in
    let maybe_state = lzma_init header in
    match maybe_state with
    | Ok statep ->
        lzmadec_init statep;
        (* Loop over the buf, extract block by block *)
        let outbuf = CArray.make uint8_t out_buf_size in
        let outbufptr = CArray.start outbuf in
        let outbufsz = Unsigned.Size_t.of_int out_buf_size in
        let outdata = Buffer.create 16 in
        Lwt_io.printf "first block [%d|%d]\n" in_buf_size data_size;
        let firstblock =
            if data_size >= in_buf_size
            then Bigarray.Array1.sub data 0 in_buf_size
            else Bigarray.Array1.sub data 0 data_size
        in
        Lwt_io.printf "first block size %d\n" (Bigarray.Array1.dim firstblock);
        (* Stupid copy of LzmaUtil.c Decode2 *)
        let rec walk inpos insize outpos block =
            Lwt_io.printf "inpos = 0x%x insize = 0x%x datasize = 0x%x outpos = 0x%x\n"
                inpos insize data_size outpos;
            if inpos < (data_size - 8) then begin
                (* Read next block if current one was processed *)
                let peekdata data =
                    if inpos = insize then begin
                        (* insize can be less than in_buf_size at the end *)
                        let tailsize = data_size - inpos in
                        let readsize =
                            if tailsize < insize then tailsize
                            else in_buf_size
                        in
                        Lwt_io.printf "Bigarray..sub %d %d\n" inpos readsize;
                        let data = Bigarray.Array1.sub data inpos readsize in
                        let inpos' = 0 in
                        data, inpos', readsize
                    end else begin
                        let data = block in
                        let inpos' = inpos in
                        data, inpos', insize
                    end
                in
                (* Read next block if current one was processed *)
                let (indata, inpos', insize') = peekdata data in
                let insize'' = insize' - inpos' in
                (* Check if size is not -1 and processed data is bigger *)
                (* NOTE: The real data size can be less than data_size or bigger! *)
                let (finishmode, outprocessed) =
                    let outproc = Int64.of_int (out_buf_size - outpos) in
                    if not (headers.outlen = -1L) && outproc > headers.outlen then
                    LZMA_FINISH_END, headers.outlen
                    else LZMA_FINISH_ANY, Int64.of_int (out_buf_size - outpos)
                in
                let finishmode' = Unsigned.UInt32.of_int
                    (elzma_finish_mode_to_enum finishmode) in
                let insz = Unsigned.Size_t.of_int insize'' in
                let inszptr = allocate size_t insz in
                let outprocessedsz = Unsigned.Size_t.of_int64 outprocessed in
                let outszptr = allocate size_t outprocessedsz in
                let inp = Ctypes.bigarray_start array1 indata in
                let inptr = coerce (ptr char) (ptr uint8_t) inp in
                let statusptr = allocate uint32_t Unsigned.UInt32.zero in
                (* Then call LzmaDec_DecodeToBuf *)
                let res = lzma_sres_of_enum (lzmadec_decode2buf statep
                                            outbufptr outszptr (* dest *)
                                            inptr inszptr (* src *)
                                            finishmode' statusptr) in
                let outprocessedsz = !@ outszptr in
                let outprocessed = Unsigned.Size_t.to_int outprocessedsz in
                let inprocessedsz = !@ inszptr in
                let inprocessed = Unsigned.Size_t.to_int inprocessedsz in
                let status = !@ statusptr in
                let nextinpos = inpos' + inprocessed in
                let nextoutpos = outpos + outprocessed in
                Lwt_io.printf "nextin %d ; nextsz %d ; nexout %d\n"
                    nextinpos insize'' nextoutpos;
                (* TODO: Check for LZMA_STATUS_FINISHED_WITH_MARK! *)
                match res with
                | Some SZ_OK -> (
                    Lwt_io.printf "LZMA_DEC: uncompressed 0x%x bytes -> 0x%x bytes\n"
                        inprocessed outprocessed;
                    (* Save the outstream *)
                    if outprocessed > 0 then begin
                        let realout = CArray.sub outbuf 0 outprocessed in
                        let outstr = carray_to_string realout in
                        Buffer.add_substring outdata outstr outpos outprocessed;
                        (* Continue unpacking *)
                        match (walk nextinpos insize'' nextoutpos indata) with
                        | Ok somedata -> Ok somedata
                        | Error e -> Error e
                    end else
                        Ok (Buffer.contents outdata)
                )
                | Some SZ_ERROR_DATA ->
                    (* Here we still can have some output *)
                        Lwt_io.printf "LZMA_DEC [corrupted]: uncompressed 0x%x bytes -> 0x%x bytes\n"
                            inprocessed outprocessed;
                    (* Save the outstream *)
                    let realout = CArray.sub outbuf 0 outprocessed in
                    let outstr = carray_to_string realout in
                    Buffer.add_substring outdata outstr outpos outprocessed;
                    (* Now return from the loop *)
                    Ok (Buffer.contents outdata)
                | Some SZ_ERROR_MEM ->
                    Or_error.error_string "Decompression: memory error"
                | Some SZ_ERROR_CRC ->
                    Or_error.error_string "Decompression: wrong CRC"
                | Some SZ_ERROR_UNSUPPORTED ->
                    Or_error.error_string "Decompression: unsupported compression"
                | Some SZ_ERROR_PARAM ->
                    Or_error.error_string "Decompression: param error"
                | Some SZ_ERROR_INPUT_EOF ->
                    Or_error.error_string "Decompression: premature input end"
                | Some SZ_ERROR_OUTPUT_EOF ->
                    Or_error.error_string "Decompression: premature output end"
                | Some SZ_ERROR_READ ->
                    Or_error.error_string "Decompression: read error"
                | Some SZ_ERROR_WRITE ->
                    Or_error.error_string "Decompression: write error"
                | Some SZ_ERROR_PROGRESS ->
                    Or_error.error_string "Decompression: progress wtf?"
                | Some SZ_ERROR_FAIL ->
                    Or_error.error_string "Decompression: fail"
                | Some SZ_ERROR_THREAD ->
                    Or_error.error_string "Decompression: thread error"
                | Some SZ_ERROR_ARCHIVE ->
                    Or_error.error_string "Decompression: archive error"
                | Some SZ_ERROR_NO_ARCHIVE ->
                    Or_error.error_string "Decompression: no archive error"
                | _ ->
                    Or_error.error_string "Decompression: unknown error"

            end else
                (* Unpacked data? *)
                Ok (Buffer.contents outdata)
        in
        walk 0 in_buf_size 0 firstblock
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA state"


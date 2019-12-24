open Core
open Ctypes
open PosixTypes
open Foreign

(* Logging setup *)
(* ----------------------------------------------------------------------------- *)
let src = Logs.Src.create "lzma"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)
(* ----------------------------------------------------------------------------- *)

let unwrap_int opt =
    match opt with
    | Some v -> v
    | None -> 0

(* TODO: Upstream this to Ctypes library
 * see https://github.com/ocamllabs/ocaml-ctypes/pull/573 *)

let carray_to_string a =
    let len = CArray.length a in
    let bytes = Bytes.create len in
    for i = 0 to len - 1 do
        Bytes.set bytes i (char_of_int
            (Unsigned.UInt8.to_int (CArray.unsafe_get a i)))
    done;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes

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

type lzma_finish_mode =
    | LZMA_FINISH_ANY
    | LZMA_FINISH_END
    [@@deriving enum]

type lzma_status =
    | LZMA_STATUS_NOT_SPECIFIED
    | LZMA_STATUS_FINISHED_WITH_MARK
    | LZMA_STATUS_NOT_FINISHED
    | LZMA_STATUS_NEEDS_MORE_INPUT
    | LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
    [@@deriving enum]

type lzma_sres =
    | SZ_OK [@value 0]
    | SZ_ERROR_DATA [@value 1]
    | SZ_ERROR_MEM [@value 2]
    | SZ_ERROR_CRC [@value 3]
    | SZ_ERROR_UNSUPPORTED [@value 4]
    | SZ_ERROR_PARAM [@value 5]
    | SZ_ERROR_INPUT_EOF [@value 6]
    | SZ_ERROR_OUTPUT_EOF [@value 7]
    | SZ_ERROR_READ [@value 8]
    | SZ_ERROR_WRITE [@value 9]
    | SZ_ERROR_PROGRESS [@value 10]
    | SZ_ERROR_FAIL [@value 11]
    | SZ_ERROR_THREAD [@value 12]
    | SZ_ERROR_ARCHIVE [@value 16]
    | SZ_ERROR_NO_ARCHIVE [@value 17]
    [@@deriving enum]

let lzma_error_to_string = function
    | SZ_ERROR_DATA ->
        "Decompression: data error"
    | SZ_ERROR_MEM ->
        "Decompression: memory error"
    | SZ_ERROR_CRC ->
        "Decompression: wrong CRC"
    | SZ_ERROR_UNSUPPORTED ->
        "Decompression: unsupported compression"
    | SZ_ERROR_PARAM ->
        "Decompression: param error"
    | SZ_ERROR_INPUT_EOF ->
        "Decompression: premature input end"
    | SZ_ERROR_OUTPUT_EOF ->
        "Decompression: premature output end"
    | SZ_ERROR_READ ->
        "Decompression: read error"
    | SZ_ERROR_WRITE ->
        "Decompression: write error"
    | SZ_ERROR_PROGRESS ->
        "Decompression: progress wtf?"
    | SZ_ERROR_FAIL ->
        "Decompression: fail"
    | SZ_ERROR_THREAD ->
        "Decompression: thread error"
    | SZ_ERROR_ARCHIVE ->
        "Decompression: archive error"
    | SZ_ERROR_NO_ARCHIVE ->
        "Decompression: no archive error"
    | _ ->
        "Decompression: unknown error"

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
                                uint32_t @-> ptr int32_t @-> (returning int))


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
    Log.debug (fun mf -> mf "headers {") |> ignore;
    Log.debug (fun mf -> mf "  properties = 0x%x" headers.properties) |> ignore;
    Log.debug (fun mf -> mf "  dict_size = 0x%lx" headers.dict_size) |> ignore;
    Log.debug (fun mf -> mf "  outlen = 0x%Lx" headers.outlen) |> ignore;
    Log.debug (fun mf -> mf "}") |> ignore

(* Fix up the headers if legacy format is met *)
(* All modern implementations store size as 8 byte integer (uint64)
 * Some legacy ones store size as 4 byte integer (uint32)
 * *)
let lzma_read_headers (ba:data) legacy =
    if legacy then begin
        let uif x = unwrap_int (Int32.to_int x) in
        (* Read header in legacy format and convert into the new one *)
        let open Internal_LZMA_Header_Legacy.LE in
        let legacy_header = Cstruct.of_bigarray ~off:0 ~len:sizeof_lzma_header_legacy ba in
        let properties = get_lzma_header_legacy_properties legacy_header in
        let dict_size = get_lzma_header_legacy_dict_size legacy_header in
        let outlen = uif (get_lzma_header_legacy_outlen legacy_header) in
        (* Header should be in the little endian format *)
        let lzma_header = Bigarray.Array1.create
            Bigarray.char Bigarray.c_layout 13 (* byte + int + long long *)
        in
        Bigarray.Array1.fill lzma_header '\x00'; (* Fill it with zeroes *)
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

(* Saves the state of the input data *)
type inbuf_state = {
    data_size: int;
    inpos: int;
    insize: int;
}

(* Saves the state of the output data *)
type outbuf_state = {
    (* These for the C calls *)
    outbuf: Unsigned.uint8 carray;
    outbufptr: Unsigned.uint8 ptr;
    outbufsize: int;
    outpos: int;
    (* And this one is the OCaml level buffer *)
    outdata: Buffer.t;
}

type decompress_block_state = {
    internal_state: clzma_dec structure ptr;
    blocknum: int;
    inbuf_state: inbuf_state;
    outbuf_state: outbuf_state;
}

(* Returns tuple (data, inpos, insize) *)
let peekdata state data =
    let datasize = Bigarray.Array1.dim data in
    (* insize can be less than in_buf_size at the end *)
    let tailsize = state.data_size - state.inpos in
    let readsize =
        if tailsize < state.insize then tailsize
        else in_buf_size
    in
    Log.debug (fun mf -> mf "PEEK: [0x%x] @@ 0x%x (tail is 0x%x, total is 0x%x)"
        readsize state.inpos tailsize datasize) |> ignore;
    let peek = Bigarray.Array1.sub data state.inpos readsize in
    peek, state.inpos, readsize

type lzma_decompression_state = FINISH_OK | MORE_DATA_OK | DATA_ERROR

type lzma_internal_result = {
    state: lzma_decompression_state;
    data: string;
    consumed: int;
}

let rec decompress_block headers state data consumed =
    Log.debug (fun mf -> mf "--------------------------------------------------------") |> ignore;
    Log.debug (fun mf -> mf " -- [%d] block: IN[...0x%x...][0x%x] -> OUT[...0x%x...]"
        state.blocknum state.inbuf_state.inpos state.inbuf_state.insize
        state.outbuf_state.outpos) |> ignore;
    if state.inbuf_state.inpos < (state.inbuf_state.data_size - 8) then begin
        (* Read next block if current one was processed *)
        let (indata, inpos, insize) = peekdata state.inbuf_state data in
        (* Check if size is not -1 and processed data is bigger *)
        (* NOTE: The real data size can be less than data_size or bigger! *)
        let (finishmode, outprocessed) =
            (* If outproc is 0 then restart? *)
            let outproc = Int64.of_int state.outbuf_state.outbufsize in
            if (not Int64.(headers.outlen = -1L)) && Int64.(outproc > headers.outlen) then
                LZMA_FINISH_END, headers.outlen
            else
                LZMA_FINISH_ANY, outproc
        in
        let finishmode' = Unsigned.UInt32.of_int
            (lzma_finish_mode_to_enum finishmode) in
        let insz = Unsigned.Size_t.of_int insize in
        let inszptr = allocate size_t insz in
        let outprocessedsz = Unsigned.Size_t.of_int64 outprocessed in
        let outszptr = allocate size_t outprocessedsz in
        let inp = Ctypes.bigarray_start array1 indata in
        let inptr = coerce (ptr char) (ptr uint8_t) inp in
        let statusptr = allocate int32_t Int32.zero in
        Log.debug (fun mf -> mf "outprocessed = 0x%Lx" outprocessed) |> ignore;

        (* Then call LzmaDec_DecodeToBuf *)
        let rawres = lzmadec_decode2buf state.internal_state
                                    state.outbuf_state.outbufptr outszptr (* dest *)
                                    inptr inszptr (* src *)
                                    finishmode' statusptr in
        let res = lzma_sres_of_enum rawres in
        let outprocessedsz = !@ outszptr in
        let outprocessed = Unsigned.Size_t.to_int outprocessedsz in
        let inprocessedsz = !@ inszptr in
        let inprocessed = Unsigned.Size_t.to_int inprocessedsz in
        Log.debug (fun mf -> mf "inprocessed = 0x%x, outprocessed = 0x%x"
            inprocessed outprocessed) |> ignore;
        let status = lzma_status_of_enum (unwrap_int (Int32.to_int (!@ statusptr))) in
        match status with
        | Some LZMA_STATUS_NEEDS_MORE_INPUT | Some LZMA_STATUS_NOT_FINISHED -> (
            let nextinpos = inpos + inprocessed in
            let nextoutpos = state.outbuf_state.outpos + outprocessed in
            (* Reset the outbuf pointer if the block was unpacked Successfully *)
            Log.debug (fun mf -> mf "More input - nextin 0x%x ; nextsz 0x%x ; nextout 0x%x"
                nextinpos insize nextoutpos) |> ignore;
            match res with
            | Some SZ_OK -> (
                Log.debug (fun mf -> mf "LZMA_DEC: uncompressed 0x%x bytes -> 0x%x bytes"
                    inprocessed outprocessed) |> ignore;
                (* Save the outstream *)
                if outprocessed > 0 then begin
                    let consumed' = consumed + inprocessed in
                    let realout = CArray.sub state.outbuf_state.outbuf ~pos:0 ~length:outprocessed in
                    let outstr = carray_to_string realout in
                    Buffer.add_substring state.outbuf_state.outdata outstr ~pos:0 ~len:outprocessed;
                    (* Continue unpacking *)
                    let newstate = {state with
                        blocknum = state.blocknum + 1;
                        inbuf_state = {state.inbuf_state with
                            inpos = nextinpos;
                            insize;
                        };
                        outbuf_state = {state.outbuf_state with
                            outpos = nextoutpos;
                        };
                    } in
                    match (decompress_block headers newstate data consumed') with
                    | Ok someresult -> Ok someresult
                    | Error e -> Error e
                end else
                    Ok ({
                        state = MORE_DATA_OK;
                        data = Buffer.contents state.outbuf_state.outdata;
                        consumed;
                    })
            )
            | Some SZ_ERROR_DATA ->
                (* Here we still can have some output *)
                Log.debug (fun mf -> mf "LZMA_DEC [corrupted]: uncompressed 0x%x bytes -> 0x%x bytes"
                        inprocessed outprocessed) |> ignore;
                (* Save the outstream only if processed something *)
                if inprocessed > 0 then begin
                    let consumed' = consumed + inprocessed in
                    let realout = CArray.sub state.outbuf_state.outbuf ~pos:0 ~length:outprocessed in
                    let outstr = carray_to_string realout in
                    Buffer.add_substring state.outbuf_state.outdata outstr ~pos:0 ~len:outprocessed;
                    (* Now return from the loop *)
                    Ok ({
                        state = DATA_ERROR;
                        data = Buffer.contents state.outbuf_state.outdata;
                        consumed = consumed';
                    })
                end else
                    Or_error.error_string "Decompression: corrupted data - processed nothing"
            | Some lzma_error ->
                Or_error.error_string (lzma_error_to_string lzma_error)
            | _ ->
                Or_error.error_string "Decompression: unknown error"
        )
        | Some LZMA_STATUS_FINISHED_WITH_MARK
        | Some LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK -> (
            (match status with
            | Some LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK ->
                Log.debug (fun mf -> mf "LZMA_DEC: Stream finished without the mark") |> ignore
            | _ ->
                Log.debug (fun mf -> mf "LZMA_DEC: Stream finished with the end mark") |> ignore);
            match res with
            | Some SZ_OK -> (
                Log.debug (fun mf -> mf "LZMA_DEC (FINISH): uncompressed 0x%x bytes -> 0x%x bytes"
                    inprocessed outprocessed) |> ignore;
                (* Save the outstream *)
                if outprocessed > 0 then begin
                    let consumed' = consumed + inprocessed in
                    let realout = CArray.sub state.outbuf_state.outbuf ~pos:0 ~length:outprocessed in
                    let outstr = carray_to_string realout in
                    Buffer.add_substring state.outbuf_state.outdata outstr ~pos:0 ~len:outprocessed;
                    Ok ({
                        state = FINISH_OK;
                        data = Buffer.contents state.outbuf_state.outdata;
                        consumed = consumed';
                    })
                end else
                    Ok ({
                        state = FINISH_OK;
                        data = Buffer.contents state.outbuf_state.outdata;
                        consumed;
                    })
            )
            | Some SZ_ERROR_DATA ->
                (* Here we still can have some output *)
                Log.debug (fun mf -> mf "LZMA_DEC [corrupted]: uncompressed 0x%x bytes -> 0x%x bytes"
                        inprocessed outprocessed) |> ignore;
                (* Save the outstream only if processed something *)
                if inprocessed > 0 then begin
                    let consumed' = consumed + inprocessed in
                    let realout = CArray.sub state.outbuf_state.outbuf ~pos:0 ~length:outprocessed in
                    let outstr = carray_to_string realout in
                    Buffer.add_substring state.outbuf_state.outdata outstr ~pos:0 ~len:outprocessed;
                    (* Now return from the loop *)
                    Ok ({
                        state = DATA_ERROR;
                        data = Buffer.contents state.outbuf_state.outdata;
                        consumed = consumed';
                    })
                end else
                    Or_error.error_string "Decompression: corrupted data - processed nothing"
            | Some lzma_error ->
                Or_error.error_string (lzma_error_to_string lzma_error)
            | _ ->
                Or_error.error_string "Decompression: unknown error"
        )
        | _ -> (
            (match res with
            | Some SZ_OK ->
                Log.debug (fun mf -> mf "LZMA UNSPECIFIED STATUS, SZ_OK") |> ignore
            | Some lzma_error ->
                let lzma_error_str = lzma_error_to_string lzma_error in
                Log.debug (fun mf -> mf "LZMA UNSPECIFIED STATUS, Error (%d): %s"
                   rawres lzma_error_str) |> ignore
            | None -> ());
            Or_error.error_string "Decompression: unknown status"
        )

    end else
        (* Unpacked data? *)
        Ok ({
            state = FINISH_OK;
            data = Buffer.contents state.outbuf_state.outdata;
            consumed = consumed;
        })

type lzma_result = {
    data: string;
    consumed: int;
}

let rec decompress_blocks headers state data consumed =
    match decompress_block headers state data consumed with
    | Ok result -> (
        (* Continue to unpack *)
        let newstate = {state with
            blocknum = state.blocknum + 1;
            inbuf_state = {state.inbuf_state with
                inpos = state.inbuf_state.inpos + result.consumed;
            };
            outbuf_state = state.outbuf_state;
        } in
        let consumed = consumed + result.consumed in
        match result.state with
        | FINISH_OK | MORE_DATA_OK ->
            if newstate.inbuf_state.inpos < (newstate.inbuf_state.data_size - 8) then begin
               Log.debug (fun mf -> mf "continuing to unpack @ 0x%x" newstate.inbuf_state.inpos) |> ignore;
               decompress_blocks headers newstate data consumed
            end else
                Ok ({
                    data = result.data;
                    consumed = result.consumed;
                })
        | DATA_ERROR ->
            Ok ({
                data = result.data;
                consumed = result.consumed;
            })
    )
    | Error e -> Error e


(* TODO: Add also bytes and string functions *)
(* Returns the lzma_result - buffer and bytes consumed *)
let lzma_decompress_auto_ba ?legacy:(legacy=false) (ba:data) =
    (* Prepare the header first *)
    let header = lzma_read_headers ba legacy in
    let headers = lzma_parse_headers header in
    dump_headers headers;
    (* Read the uncompressed data size *)
    let data = lzma_read_data ba legacy in
    let data_size = Bigarray.Array1.dim data in
    Log.debug (fun mf -> mf "LZMA: input data size 0x%x" data_size) |> ignore;
    let maybe_state = lzma_init header in
    match maybe_state with
    | Ok statep ->
        lzmadec_init statep;
        (* Loop over the buf, extract block by block *)
        let outbufsize = out_buf_size * 4 in
        let outbuf = CArray.make uint8_t outbufsize in
        let outbufptr = CArray.start outbuf in
        let outdata = Buffer.create 16 in
        (* The initial state of the block-level decompression *)
        let initial_state = {
            internal_state = statep;
            blocknum = 0;
            inbuf_state = {
                data_size;
                inpos = 0;
                insize = in_buf_size;
            };
            outbuf_state = {
                outbuf;
                outbufptr;
                outbufsize;
                outpos = 0;
                outdata;
            }
        } in
        let res = decompress_blocks headers initial_state data 0 in
        lzma_deinit statep;
        res
    | _ -> Or_error.error_string "Decompression: cannot initialize LZMA state"


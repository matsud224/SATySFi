
exception FontFormatBroken of Otfm.error
exception NoGlyph          of Uchar.t

type file_path = string

type 'a resource =
  | Data           of 'a
  | EmbeddedStream of int

type predefined_encoding =
  | StandardEncoding
  | MacRomanEncoding
  | WinAnsiEncoding

type differences = (string * int) list

type encoding =
  | ImplicitEncoding
  | PredefinedEncoding of predefined_encoding
  | CustomEncoding     of predefined_encoding * differences


type cmap_data = (string resource) ref  (* temporary;*)

type cmap =
  | PredefinedCMap of string
  | CMapFile       of cmap_data

type cid_system_info = {
    registry   : string;
    ordering   : string;
    supplement : int;
  }

type bbox = int * int * int * int

type matrix = float * float * float * float

type font_descriptor = {
    font_name    : string;
    font_family  : string;
    italic_angle : float;
    ascent    : float;
    descent   : float;
    stemv     : float;
    font_data : (Otfm.decoder resource) ref;
    (* temporary; should contain more fields *)
  }

module CIDFontType0
= struct
    type font = {
        system_info     : cid_system_info;
        base_font       : string;
        font_descriptor : font_descriptor;
        dw              : int option;
        dw2             : (int * int) option;
        (* temporary; should contain more fields; /W, /W2 *)
      }
  end

type cid_to_gid_map =
  | CIDToGIDIdentity
  | CIDToGIDStream   of (string resource) ref  (* temporary *)

module CIDFontType2
= struct
    type font = {
        system_info : cid_system_info;
        base_font   : string;
        font_descriptor : font_descriptor;
        dw              : int option;
        dw2             : (int * int) option;
        cid_to_gid_map  : cid_to_gid_map;
        (* temporary; should contain more fields; /W, /W2 *)
      }
  end

type cid_font =
  | CIDFontType0 of CIDFontType0.font
  | CIDFontType2 of CIDFontType2.font


let to_base85_pdf_bytes (dcdr : Otfm.decoder) : Pdfio.bytes =
  match Otfm.decoder_src dcdr with
  | `String(s) ->
      let s85 = Base85.encode s in
        Pdfio.bytes_of_string s85


let add_stream_of_decoder (pdf : Pdf.t) (dcdr : Otfm.decoder) (subtypeopt : string option) () : int =
  let bt85 = to_base85_pdf_bytes dcdr in
  let len = Pdfio.bytes_size bt85 in
  let contents = [
      ("/Length", Pdf.Integer(len));
      ("/Filter", Pdf.Name("/ASCII85Decode"));
    ]
  in
  let dict =
    match subtypeopt with
    | None          -> contents
    | Some(subtype) -> ("/Subtype", Pdf.Name("/" ^ subtype)) :: contents
  in
  let objstream =
    Pdf.Stream(ref (Pdf.Dictionary(dict), Pdf.Got(bt85)))
  in
  let irstream = Pdf.addobj pdf objstream in
    irstream


let get_glyph_id (dcdr : Otfm.decoder) (uch : Uchar.t) : Otfm.glyph_id =
  let cp = Uchar.to_int uch in
  let cmapres =
    Otfm.cmap dcdr (fun accopt mapkd (u0, u1) gid ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if u0 <= cp && cp <= u1 then Some(gid + (cp - u0)) else None
    ) None
  in
    match cmapres with
    | Error(e)                   -> raise (FontFormatBroken(e))
    | Ok(((_, _, _), None))      -> raise (NoGlyph(uch))
    | Ok(((_, _, _), Some(gid))) -> gid


let get_uchar_raw_contour_list_and_bounding_box (dcdr : Otfm.decoder) (uch : Uchar.t)
    : ((bool * int * int) list) list * (int * int * int * int) =
  let gid = get_glyph_id dcdr uch in
  let gloc =
    match Otfm.loca dcdr gid with
    | Error(e)    -> raise (FontFormatBroken(e))
    | Ok(None)    -> raise (NoGlyph(uch))
    | Ok(Some(l)) -> l
  in
    match Otfm.glyf dcdr gloc with
    | Error(e)                        -> raise (FontFormatBroken(e))
    | Ok((`Composite(_), _))          -> raise (NoGlyph(uch))  (* temporary; does not deal with composite glyphs *)
    | Ok((`Simple(precntrlst), bbox)) -> (precntrlst, bbox)


let get_uchar_height_and_depth (dcdr : Otfm.decoder) (uch : Uchar.t) : int * int =
  try  (* temporary; for font formats that do not contain the `loca` table *)
    let (_, (_, ymin, _, ymax)) = get_uchar_raw_contour_list_and_bounding_box dcdr uch in
      (ymax, ymin)
  with FontFormatBroken(_) -> (500, -100)  (* temporary; for font formats that do not contain the `loca` table *)


let get_uchar_horz_metrics (dcdr : Otfm.decoder) (uch : Uchar.t) =
  let gidkey = get_glyph_id dcdr uch in
  let hmtxres =
    None |> Otfm.hmtx dcdr (fun accopt gid adv lsb ->
      match accopt with
      | Some(_) -> accopt
      | None    -> if gid = gidkey then Some((adv, lsb)) else None
    )
  in
    match hmtxres with
    | Error(e)             -> raise (FontFormatBroken(e))
    | Ok(None)             -> assert false
    | Ok(Some((adv, lsb))) -> (adv, lsb)


let get_uchar_advance_width (dcdr : Otfm.decoder) (uch : Uchar.t) : int =
  try
    let (adv, _) = get_uchar_horz_metrics dcdr uch in adv
  with
  | NoGlyph(_) -> 0


let get_uchar_metrics (dcdr : Otfm.decoder) (uch : Uchar.t) : int * int * int =
  let wid = get_uchar_advance_width dcdr uch in
  let (hgt, dpt) = get_uchar_height_and_depth dcdr uch in
    (wid, hgt, dpt)


let get_truetype_widths_list (dcdr : Otfm.decoder) (firstchar : int) (lastchar : int) : int list =
  let rec range acc m n =
    if m > n then List.rev acc else
      range (m :: acc) (m + 1) n
  in
    (range [] firstchar lastchar) |> List.map (fun charcode ->
      get_uchar_advance_width dcdr (Uchar.of_int charcode)
    )


let font_descriptor_of_decoder dcdr font_name =
  {
    font_name    = font_name;
    font_family  = "";  (* temporary *)
    italic_angle = 0.;  (* temporary *)
    ascent       = 0.;  (* temporary *)
    descent      = 0.;  (* temporary *)
    stemv        = 0.;  (* temporary *)
    font_data    = ref (Data(dcdr));
    (* temporary; should contain more fields *)
  }


module Type1Scheme_
= struct
    type font = {
        name            : string option;
          (* -- obsolete field; required in PDF 1.0
                but optional in all other versions -- *)
        base_font       : string;
        first_char      : int;
        last_char       : int;
        widths          : int list;
        font_descriptor : font_descriptor;
        encoding        : encoding;
        to_unicode      : cmap_data option;
      }
end

module Type1
= struct
    include Type1Scheme_
  end

module TrueType
= struct
    include Type1Scheme_


    let of_decoder dcdr fc lc =
      let base_font =
        match Otfm.postscript_name dcdr with
        | Error(e)    -> raise (FontFormatBroken(e))
        | Ok(None)    -> assert false
        | Ok(Some(x)) -> x
      in
        {
          name            = None;
          base_font       = base_font;
          first_char      = fc;
          last_char       = lc;
          widths          = get_truetype_widths_list dcdr fc lc;
          font_descriptor = font_descriptor_of_decoder dcdr base_font;
          encoding        = PredefinedEncoding(StandardEncoding);
          to_unicode      = None;
        }


    let to_pdf_dictionary pdf trtyfont dcdr =
      let font_name  = trtyfont.base_font in
      let first_char = trtyfont.first_char in
      let last_char  = trtyfont.last_char in
      let widths     = trtyfont.widths in
      let irstream = add_stream_of_decoder pdf dcdr None () in
        (* -- add to the PDF the stream in which the font file is embedded -- *)
      let objdescr =
        Pdf.Dictionary[
          ("/Type"       , Pdf.Name("/FontDescriptor"));
          ("/FontName"   , Pdf.Name("/" ^ font_name));
          ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
          ("/FontBBox"   , Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
          ("/ItalicAngle", Pdf.Integer(0));  (* temporary; should be variable *)
          ("/Ascent"     , Pdf.Integer(0)); (* temporary; should be variable *)
          ("/Descent"    , Pdf.Integer(0)); (* temporary; should be variable *)
          ("/StemV"      , Pdf.Integer(0));  (* temporary; should be variable *)
          ("/FontFile2"  , Pdf.Indirect(irstream));
        ]
      in
      let irdescr = Pdf.addobj pdf objdescr in
        Pdf.Dictionary[
          ("/Type"     , Pdf.Name("/Font"));
          ("/Subtype"  , Pdf.Name("/TrueType"));
          ("/BaseFont" , Pdf.Name("/" ^ font_name));
          ("/FirstChar", Pdf.Integer(first_char));
          ("/LastChar" , Pdf.Integer(last_char));
          ("/Widths"   , Pdf.Array(List.map (fun x -> Pdf.Integer(x)) widths));
          ("/FontDescriptor", Pdf.Indirect(irdescr));
        ]
  end

module Type3
= struct
    type font = {
        name            : string;
        font_bbox       : bbox;
        font_matrix     : matrix;
        encoding        : encoding;
        first_char      : int;
        last_char       : int;
        widths          : int list;
        font_descriptor : font_descriptor;
        to_unicode      : cmap_data option;
      }
  end


let pdfobject_of_cmap pdf cmap =
  match cmap with
  | PredefinedCMap(cmapname) -> Pdf.Name("/" ^ cmapname)
  | CMapFile(res)            -> failwith "cmap file for Type 0 fonts; remains to be implemented."


module Type0
= struct
    type font = {
        base_font        : string;
        encoding         : cmap;
        descendant_fonts : cid_font;  (* -- represented as a singleton list in PDF -- *)
        to_unicode       : cmap_data option;
      }


    let add_font_descriptor pdf fontdescr base_font =
      let dcdr =
        match !(fontdescr.font_data) with
        | Data(d) -> d
        | _       -> assert false
      in
      let irstream = add_stream_of_decoder pdf dcdr (Some("OpenType")) () in
        (* -- add to the PDF the stream in which the font file is embedded -- *)
      let objdescr =
        Pdf.Dictionary[
          ("/Type"       , Pdf.Name("/FontDescriptor"));
          ("/FontName"   , Pdf.Name("/" ^ base_font));
          ("/Flags"      , Pdf.Integer(4));  (* temporary; should be variable *)
          ("/FontBBox"   , Pdf.Array[Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0); Pdf.Integer(0)]);  (* temporary; should be variable *)
          ("/ItalicAngle", Pdf.Real(fontdescr.italic_angle));
          ("/Ascent"     , Pdf.Real(fontdescr.ascent));
          ("/Descent"    , Pdf.Real(fontdescr.descent));
          ("/StemV"      , Pdf.Real(fontdescr.stemv));
          ("/FontFile3"  , Pdf.Indirect(irstream));
        ]
      in
      let irdescr = Pdf.addobj pdf objdescr in
        irdescr


    let pdf_dictionary_of_cid_system_info cidsysinfo =
      Pdf.Dictionary[
        ("/Registry", Pdf.String(cidsysinfo.registry));
        ("/Ordering", Pdf.String(cidsysinfo.ordering));
        ("/Supplement", Pdf.Integer(cidsysinfo.supplement));
      ]


    let add_cid_type_0 pdf cidty0font =
      let cidsysinfo = cidty0font.CIDFontType0.system_info in
      let base_font  = cidty0font.CIDFontType0.base_font in
      let fontdescr  = cidty0font.CIDFontType0.font_descriptor in
      let irdescr = add_font_descriptor pdf fontdescr base_font in
      let objdescend =
        Pdf.Dictionary[
          ("/Type"          , Pdf.Name("/Font"));
          ("/Subtype"       , Pdf.Name("/CIDFontType0"));
          ("/BaseFont"      , Pdf.Name("/" ^ base_font));
          ("/CIDSystemInfo" , pdf_dictionary_of_cid_system_info cidsysinfo);
          ("/FontDescriptor", Pdf.Indirect(irdescr));
            (* should add more; /DW, /W, /DW2, /W2 *)
        ]
      in
      let irdescend = Pdf.addobj pdf objdescend in
        irdescend


    let add_cid_type_2 pdf cidty2font =
      let cidsysinfo = cidty2font.CIDFontType2.system_info in
      let base_font  = cidty2font.CIDFontType2.base_font in
      let fontdescr  = cidty2font.CIDFontType2.font_descriptor in
      let irdescr = add_font_descriptor pdf fontdescr base_font in
      let objdescend =
        Pdf.Dictionary[
          ("/Type"          , Pdf.Name("/Font"));
          ("/Subtype"       , Pdf.Name("/CIDFontType2"));
          ("/BaseFont"      , Pdf.Name("/" ^ base_font));
          ("/CIDSystemInfo" , pdf_dictionary_of_cid_system_info cidsysinfo);
          ("/FontDescriptor", Pdf.Indirect(irdescr));
            (* should add more; /DW, /W, /DW2, /W2, /CIDToGIDMap *)
        ]
      in
      let irdescend = Pdf.addobj pdf objdescend in
        irdescend


    let to_pdf_dictionary pdf ty0font dcdr =
      let cidfont       = ty0font.descendant_fonts in
      let base_font_ty0 = ty0font.base_font in
      let cmap          = ty0font.encoding in
      let irdescend =
        match cidfont with
        | CIDFontType0(cidty0font) -> add_cid_type_0 pdf cidty0font
        | CIDFontType2(cidty2font) -> add_cid_type_2 pdf cidty2font
      in
        Pdf.Dictionary[
          ("/Type"           , Pdf.Name("/Font"));
          ("/Subtype"        , Pdf.Name("/Type0"));
          ("/Encoding"       , pdfobject_of_cmap pdf cmap);
          ("/BaseFont"       , Pdf.Name("/" ^ base_font_ty0));  (* -- can be arbitrary name -- *)
          ("/DescendantFonts", Pdf.Array[Pdf.Indirect(irdescend)]);
        ]

  end

type font =
  | Type0    of Type0.font
  | Type1    of Type1.font
(*  | Type1C *)
(*  | MMType1 *)
(*  | Type3 *)
  | TrueType of TrueType.font

module Length
: sig
    type t  [@@deriving show]
    val zero : t
    val add : t -> t -> t
    val subtr : t -> t -> t
    val mult : t -> float -> t
    val div : t -> t -> float
    val max : t -> t -> t
    val min : t -> t -> t
    val negate : t -> t
    val abs : t -> t
    val less_than : t -> t -> bool
    val leq : t -> t -> bool
    val is_nearly_zero : t -> bool
    val of_pdf_point : float -> t
    val to_pdf_point : t -> float
    val of_centimeter : float -> t
    val of_millimeter : float -> t
    val of_inch : float -> t
    val show : t -> string
  end
= struct

    type t = float  [@@deriving show]
    let zero = 0.
    let add = ( +. )
    let subtr = ( -. )
    let mult = ( *. )
    let div = ( /. )
    let max = max
    let min = min
    let negate x = 0. -. x
    let abs x = if x < 0. then -.x else x
    let less_than = ( < )
    let leq = ( <= )
    let is_nearly_zero sl = (sl < 0.01)

    let of_pdf_point pt = pt
    let to_pdf_point len = len

    let convert pdfunit flt =
      let dpi = 72. in  (* temporary; dpi *)
        Pdfunits.convert dpi pdfunit Pdfunits.PdfPoint flt      

    let of_centimeter = convert Pdfunits.Centimetre
    let of_millimeter = convert Pdfunits.Millimetre
    let of_inch       = convert Pdfunits.Inch

    let show = string_of_float
  end

let ( +% ) = Length.add
let ( -% ) = Length.subtr
let ( *% ) = Length.mult
let ( *%! ) l n = l *% (float_of_int n)
let ( /% ) = Length.div
let ( <% ) = Length.less_than
let ( <=% ) = Length.leq


type length = Length.t  [@@deriving show]

type point = length * length

type stretchable =
  | FiniteStretch of length
  | Fils          of int

let add_stretchable strc1 strc2 =
  match (strc1, strc2) with
  | (FiniteStretch(w1), FiniteStretch(w2)) -> FiniteStretch(w1 +% w2)
  | (Fils(i1), Fils(i2))                   -> Fils(i1 + i2)
  | (Fils(i1), _)                          -> Fils(i1)
  | (_, Fils(i2))                          -> Fils(i2)
  
type length_info =
  {
    natural     : length;
    shrinkable  : length;
    stretchable : stretchable;
  }

type pure_badness = int
[@@deriving show]

type ratios =
  | TooShort
  | PermissiblyShort of float
  | PermissiblyLong  of float
  | TooLong

type font_abbrev = string
[@@deriving show]

type math_font_abbrev = string
[@@deriving show]

type file_path = string
(*
type encoding_in_pdf =
  | Latin1
  | UTF16BE
  | IdentityH
*)
type font_with_size = font_abbrev * Length.t
[@@deriving show]

type font_with_ratio = font_abbrev * float * float
[@@deriving show]

type page_size =
  | A4Paper
  | UserDefinedPaper of length * length
[@@deriving show]

type page_scheme =
  {
    page_size        : page_size;
    left_page_margin : length;
    top_page_margin  : length;
    area_width       : length;
    area_height      : length;
  }
[@@deriving show]

type paddings =
  {
    paddingL : length;
    paddingR : length;
    paddingT : length;
    paddingB : length;
  }
[@@deriving show]

(* -- representation about graphics based on PDF 1.7 specification -- *)

type color =
  | DeviceGray of float
  | DeviceRGB  of float * float * float
  | DeviceCMYK of float * float * float * float
[@@deriving show]

type 'a path_element =
  | LineTo              of 'a
  | CubicBezierTo       of point * point * 'a

type path =
  | GeneralPath of point * (point path_element) list * (unit path_element) option
  | Rectangle   of point * point

type line_dash =
  | SolidLine
  | DashedLine of length * length * length

type line_join =
  | MiterJoin
  | RoundJoin
  | BevelJoin

type line_cap =
  | ButtCap
  | RoundCap
  | ProjectingSquareCap

(* will be deprecated *)
type graphics_state =
  {
    line_width   : length;
    line_dash    : line_dash;
    line_join    : line_join;
    line_cap     : line_cap;
    miter_limit  : length;
    fill_color   : color;
    stroke_color : color;
  }

(* will be deprecated *)
type graphics_command =
  | DrawStroke
  | DrawFillByNonzero
  | DrawFillByEvenOdd
  | DrawBothByNonzero
  | DrawBothByEvenOdd

type horz_string_info =
  {
    font_abbrev    : font_abbrev;
    text_font_size : length;
    text_color     : color;
    rising         : length;
  }

let pp_horz_string_info fmt info =
  Format.fprintf fmt "(HSinfo)"

type math_string_info =
  {
    math_font_abbrev : math_font_abbrev;
    math_font_size   : length;
    math_color       : color;
  }

(* -- internal representation of boxes -- *)

type decoration = point -> length -> length -> length -> Pdfops.t list
[@@deriving show]


module ScriptSchemeMap = Map.Make
  (struct
    type t = CharBasis.script
    let compare = Pervasives.compare
  end)


type math_kind =
  | MathOrdinary
  | MathBinary
  | MathRelation
  | MathOperator
  | MathPunct
  | MathOpen
  | MathClose
  | MathPrefix    (* -- mainly for differantial operator 'd', '\partial', etc. -- *)
  | MathInner
  | MathEnd
[@@deriving show]

type math_char_class =
  | MathItalic
  | MathBoldItalic
  | MathRoman
  | MathBoldRoman
[@@deriving show]
(* TEMPORARY; should add more *)


module MathVariantCharMap = Map.Make
  (struct
    type t = string * math_char_class
    let compare = Pervasives.compare
  end)


type input_context = {
  font_size        : length;
  font_scheme      : font_with_ratio ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  langsys_scheme   : CharBasis.language_system ScriptSchemeMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  math_font        : math_font_abbrev;
  dominant_script  : CharBasis.script;
  space_natural    : float;
  space_shrink     : float;
  space_stretch    : float;
  adjacent_stretch : float;
  paragraph_width  : length;
  paragraph_top    : length;
  paragraph_bottom : length;
  leading          : length;
  min_gap_of_lines : length;
  text_color       : color;
  manual_rising    : length;
  page_scheme      : page_scheme;
  badness_space    : pure_badness;
  math_variant_char_map : math_variant_value MathVariantCharMap.t;
    [@printer (fun fmt _ -> Format.fprintf fmt "<map>")]
  math_char_class  : math_char_class;
}

(* -- 'pure_horz_box': core part of the definition of horizontal boxes -- *)
and pure_horz_box =
(* -- spaces inserted before text processing -- *)
  | PHSOuterEmpty     of length * length * length
  | PHSOuterFil
  | PHSFixedEmpty     of length
(* -- texts -- *)
  | PHCInnerString    of input_context * Uchar.t list
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedString(...)@]")]
  | PHCInnerMathGlyph of math_string_info * length * length * length * FontFormat.glyph_id
      [@printer (fun fmt _ -> Format.fprintf fmt "@[FixedMathGlyph(...)@]")]
(* -- groups -- *)
  | PHGRising         of length * horz_box list
  | PHGFixedFrame     of paddings * length * decoration * horz_box list
  | PHGInnerFrame     of paddings * decoration * horz_box list
  | PHGOuterFrame     of paddings * decoration * horz_box list
  | PHGEmbeddedVert   of length * length * length * evaled_vert_box list
  | PHGFixedGraphics  of length * length * length * (point -> Pdfops.t list)

and horz_box =
  | HorzPure           of pure_horz_box
  | HorzDiscretionary  of pure_badness * horz_box list * horz_box list * horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "HorzDiscretionary(...)")]
  | HorzFrameBreakable of paddings * length * length * decoration * decoration * decoration * decoration * horz_box list

and evaled_horz_box_main =
  | EvHorzString of horz_string_info * length * length * OutputText.t
      (* --
         (1) string information for writing string to PDF
         (2) content height
         (3) content depth
         (4) content string
         -- *)

  | EvHorzMathGlyph      of math_string_info * length * length * FontFormat.glyph_id
      [@printer (fun fmt _ -> Format.fprintf fmt "EvHorzMathGlyph(...)")]
  | EvHorzRising         of length * length * length * evaled_horz_box list
  | EvHorzEmpty
  | EvHorzFrame          of length * length * decoration * evaled_horz_box list
  | EvHorzEmbeddedVert   of length * length * evaled_vert_box list
  | EvHorzInlineGraphics of length * length * (point -> Pdfops.t list)

and evaled_horz_box =
  | EvHorz of length * evaled_horz_box_main

and intermediate_vert_box =
  | ImVertLine              of length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "Line")]
  | ImVertFixedBreakable    of length
      [@printer (fun fmt _ -> Format.fprintf fmt "Breakable")]
  | ImVertTopMargin         of bool * length
      [@printer (fun fmt _ -> Format.fprintf fmt "Top")]
  | ImVertBottomMargin      of bool * length
      [@printer (fun fmt _ -> Format.fprintf fmt "Bottom")]
  | ImVertFrame             of paddings * decoration * decoration * decoration * decoration * length * intermediate_vert_box list
(*      [@printer (fun fmt (_, _, _, _, _, imvblst) -> Format.fprintf fmt "%a" (pp_list pp_intermediate_vert_box) imvblst)] *)
and evaled_vert_box =
  | EvVertLine       of length * length * evaled_horz_box list
      [@printer (fun fmt _ -> Format.fprintf fmt "EvLine")]
  | EvVertFixedEmpty of length
      [@printer (fun fmt _ -> Format.fprintf fmt "EvEmpty")]
  | EvVertFrame      of paddings * decoration * length * evaled_vert_box list

and math_char_kern_func = length -> length -> length
  (* -- takes the actual font size and the y-position, and returns a kerning value -- *)

and math_element_main =
  | MathChar         of Uchar.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-char>")]
  | MathCharWithKern of Uchar.t * math_char_kern_func * math_char_kern_func
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-char'>")]
  | MathEmbeddedText of (input_context -> horz_box list)

and math_element =
  | MathElement           of math_kind * math_element_main
  | MathVariantChar       of string
  | MathVariantCharDirect of math_kind * Uchar.t * Uchar.t * Uchar.t * Uchar.t  (* TEMPORARY; should extend more *)
      [@printer (fun fmt _ -> Format.fprintf fmt "<math-variant-char-direct>")]

and math_kern_func = length -> length
  (* -- takes the y-position and then returns a kerning value -- *)

and math_variant_value = math_kind * math_variant_value_main

and math_variant_value_main =
  | MathVariantToChar         of Uchar.t
      [@printer (fun fmt _ -> Format.fprintf fmt "<to-char>")]
  | MathVariantToCharWithKern of Uchar.t * math_char_kern_func * math_char_kern_func
      [@printer (fun fmt _ -> Format.fprintf fmt "<to-char'>")]

and paren = length -> length -> length -> length -> color -> horz_box list * math_kern_func
  (* --
     'paren':
       the type for adjustable parentheses.
       An adjustable parenthesis takes as arguments
       (1-2) the height and the depth of the inner contents,
       (3)   the axis height,
       (4)   the font size, and
       (5)   the color for glyphs,
       and then returns its inline box representation and the function for kerning.
     -- *)

and radical = length -> length -> length -> length -> color -> horz_box list
  (* --
     'radical':
       the type for adjustable radicals.
       An adjustable radical takes as arguments
       (1-2) the height and the thickness of the bar required by the math font,
       (3)   the depth of the inner contents,
       (4)   the font size, and
       (5)   the color for glyphs,
       and then returns the inline box representation.
     -- *)

and math_context_change =
  | MathChangeColor         of color
  | MathChangeMathCharClass of math_char_class

and math =
  | MathPure              of math_element
  | MathChangeContext     of math_context_change * math list
  | MathGroup             of math_kind * math_kind * math list
  | MathSubscript         of math list * math list
  | MathSuperscript       of math list * math list
  | MathFraction          of math list * math list
  | MathRadicalWithDegree of math list * math list
  | MathRadical           of radical * math list
  | MathParen             of paren * paren * math list
  | MathUpperLimit        of math list * math list
  | MathLowerLimit        of math list * math list
[@@deriving show]


module MathContext
: sig
    type t
    val make : input_context -> t
    val context_for_text : t -> input_context
    val convert_math_variant_char : t -> string -> math_variant_value
    val color : t -> color
    val set_color : color -> t -> t
    val enter_script : t -> t
    val math_char_class : t -> math_char_class
    val set_math_char_class : math_char_class -> t -> t
    val is_in_base_level : t -> bool
    val actual_font_size : t -> (math_font_abbrev -> FontFormat.math_decoder) -> length
    val base_font_size : t -> length
    val math_font_abbrev : t -> math_font_abbrev
  end
= struct
    type level =
      | BaseLevel
      | ScriptLevel
      | ScriptScriptLevel

    type t =
      {
        mc_font_abbrev    : math_font_abbrev;
        mc_base_font_size : length;
        mc_level_int      : int;
        mc_level          : level;
        context_for_text  : input_context;
      }

    let make (ctx : input_context) =
      {
        mc_font_abbrev    = ctx.math_font;
        mc_base_font_size = ctx.font_size;
        mc_level_int      = 0;
        mc_level          = BaseLevel;
        context_for_text  = ctx;
      }

    let convert_math_variant_char mctx s =
      let ctx = mctx.context_for_text in
      let mcclsmap = ctx.math_variant_char_map in
      let mccls = ctx.math_char_class in
        match mcclsmap |> MathVariantCharMap.find_opt (s, mccls) with
        | Some(mvvalue) ->
            Format.printf "HorzBox> convert_math_variant_char: found\n";  (* for debug *)
            mvvalue

        | None ->
            Format.printf "HorzBox> convert_math_variant_char: NOT found\n";  (* for debug *)
            begin
              match InternalText.to_uchar_list (InternalText.of_utf8 s) with
              | []       -> (MathOrdinary, MathVariantToChar(Uchar.of_int 0))  (* needs reconsideration *)
              | uch :: _ -> (MathOrdinary, MathVariantToChar(uch))
            end

    let context_for_text mctx =
      mctx.context_for_text
        (* temporary; maybe should update font size *)

    let color mctx =
      mctx.context_for_text.text_color

    let set_color color mctx =
      let ctx = mctx.context_for_text in
      let ctxnew = { ctx with text_color = color; } in
        { mctx with context_for_text = ctxnew; }

    let math_char_class mctx =
      mctx.context_for_text.math_char_class

    let set_math_char_class mccls mctx =
      let ctx = mctx.context_for_text in
      let ctxnew = { ctx with math_char_class = mccls } in
        { mctx with context_for_text = ctxnew }

    let enter_script mctx =
      let levnew = mctx.mc_level_int + 1 in
      match mctx.mc_level with
      | BaseLevel         -> { mctx with mc_level = ScriptLevel;       mc_level_int = levnew; }
      | ScriptLevel       -> { mctx with mc_level = ScriptScriptLevel; mc_level_int = levnew; }
      | ScriptScriptLevel -> { mctx with                               mc_level_int = levnew; }

    let is_in_base_level mctx =
      match mctx.mc_level with
      | BaseLevel -> true
      | _         -> false

    let actual_font_size mctx (mdf : math_font_abbrev -> FontFormat.math_decoder) =
      let bfsize = mctx.mc_base_font_size in
      let md = mdf mctx.mc_font_abbrev in
      let mc = FontFormat.get_math_constants md in
      match mctx.mc_level with
      | BaseLevel         -> bfsize
      | ScriptLevel       -> bfsize *% mc.FontFormat.script_scale_down
      | ScriptScriptLevel -> bfsize *% mc.FontFormat.script_script_scale_down

    let base_font_size mctx =
      mctx.mc_base_font_size

    let math_font_abbrev mctx =
      mctx.mc_font_abbrev

  end


type math_context = MathContext.t


let default_font_with_ratio =
  ("Arno", 1., 0.)  (* TEMPORARY *)


let get_font_with_ratio ctx script_raw =
  let script =
    match script_raw with
    | ( CharBasis.Common | CharBasis.Unknown | CharBasis.Inherited ) -> ctx.dominant_script
    | _                                                              -> script_raw
  in
    match ctx.font_scheme |> ScriptSchemeMap.find_opt script with
    | None          -> default_font_with_ratio
    | Some(fontsch) -> fontsch


let get_language_system ctx script_raw =
  let script =
    match script_raw with
    | ( CharBasis.Common | CharBasis.Unknown | CharBasis.Inherited ) -> ctx.dominant_script
    | _                                                              -> script_raw
  in
  match ctx.langsys_scheme |> ScriptSchemeMap.find_opt script with
  | None          -> CharBasis.NoLanguageSystem
  | Some(langsys) -> langsys


let get_string_info ctx script_raw =
  let (font_abbrev, ratio, rising_ratio) = get_font_with_ratio ctx script_raw in
    {
      font_abbrev    = font_abbrev;
      text_font_size = ctx.font_size *% ratio;
      text_color     = ctx.text_color;
      rising         = ctx.manual_rising +% ctx.font_size *% rising_ratio;
    }

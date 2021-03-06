
open MyUtil
open LengthInterface
open HorzBox
open CharBasis
open LineBreakBox


type chunk_info = context_main * script * line_break_class


let to_chunk_main_list ctx uchlst alwlast : break_opportunity * line_break_chunk_main list =
  let (alwfirst, trilst) = LineBreakDataMap.append_break_opportunity uchlst alwlast in
  let scrlst = ScriptDataMap.divide_by_script ctx trilst in
  (alwfirst, scrlst)


let to_chunks ctx uchlst alwlast : break_opportunity * line_break_chunk list =
  let (alwfirst, scrlstsp) = to_chunk_main_list ctx uchlst alwlast in
  let chunklst =
    scrlstsp |> List.map (fun chunkmain -> (ctx, chunkmain))
  in
    (alwfirst, chunklst)


let half_kern (hsinfo : horz_string_info) : lb_pure_box =
  LBAtom((natural (hsinfo.text_font_size *% -0.5), Length.zero, Length.zero), EvHorzEmpty)


let pure_space_between_scripts ctx1 ctx2 size (script1 : script) (lbc1 : line_break_class) (script2 : script) (lbc2 : line_break_class) =
  if is_open_punctuation lbc1 || is_close_punctuation lbc2 then
    None
  else
    match
      (ctx1.script_space_map |> ScriptSpaceMap.find_opt (script1, script2),
       ctx2.script_space_map |> ScriptSpaceMap.find_opt (script1, script2))
    with
    | (None, None) ->
        None

    | (Some(tuple), None)
    | (None, Some(tuple)) ->
        let (r0, r1, r2) = tuple in
          Some(LBAtom((natural (size *% r0), size *% r1, size *% r2), EvHorzEmpty))

    | (Some(tuple1), Some(tuple2)) ->
        let (r10, r11, r12) = tuple1 in
        let (r20, r21, r22) = tuple2 in
        let (r0, r1, r2) = (max r10 r20, max r11 r21, max r12 r22) in
          Some(LBAtom((natural (size *% r0), size *% r1, size *% r2), EvHorzEmpty))
(*
    match (script1, script2) with
    | (HanIdeographic    , Latin             )
    | (Latin             , HanIdeographic    )
    | (HiraganaOrKatakana, Latin             )
    | (Latin             , HiraganaOrKatakana)
      ->
        Some(LBAtom((natural (size *% 0.24), size *% 0.08, size *% 0.16), EvHorzEmpty))
          (* temporary; shold refer to the context for spacing information between two scripts *)
    | _ -> None
*)

let space_width_info ctx : length_info =
  let size = ctx.font_size in
    (* -- uses font size directly, not multiplied by the ratio of the dominant script -- *)
  let widnatural = size *% ctx.space_natural in
  let widshrink  = size *% ctx.space_shrink in
  let widstretch = size *% ctx.space_stretch in
    make_width_info widnatural widshrink widstretch


let pure_space ctx : lb_pure_box =
  let widinfo = space_width_info ctx in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


let get_corrected_font_size ctx script =
  let (_, font_ratio, _) = get_font_with_ratio ctx script in
    ctx.font_size *% font_ratio


(* -- 'pure_halfwidth_space_soft': inserts a shrinkable CJK halfwidth space -- *)
let pure_halfwidth_space_soft fontsize : lb_pure_box =
  let widinfo = make_width_info (fontsize *% 0.5) (fontsize *% 0.25) (fontsize *% 0.25) in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


(* -- 'pure_halfwidth_space_hard': inserts a non-shrinkable CJK halfwidth space -- *)
let pure_halfwidth_space_hard fontsize : lb_pure_box =
  let widinfo = make_width_info (fontsize *% 0.5) Length.zero (fontsize *% 0.25) in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


(* -- 'pure_fullwidth_space': inserts a shrinkable CJK fullwidth space -- *)
let pure_fullwidth_space fontsize : lb_pure_box =
  let widinfo = make_width_info fontsize (fontsize *% 0.5) (fontsize *% 0.5) in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


(*  -- 'adjacent_space': inserts glue between directly adjacent CJK characters -- *)
let adjacent_space ctx1 ctx2 =
  let fontsize = Length.max ctx1.font_size ctx2.font_size in
  let ratio = max ctx1.adjacent_stretch ctx2.adjacent_stretch in
  let widstretch = fontsize *% ratio in
  let widinfo = make_width_info Length.zero Length.zero widstretch in
    LBAtom((widinfo, Length.zero, Length.zero), EvHorzEmpty)


(*  -- 'halfwidth_kern': inserts a solid backward halfwidth kern for CJK characters -- *)
let halfwidth_kern ctx script : lb_box =
  let size = get_corrected_font_size ctx script in
    LBPure(LBAtom((natural (Length.negate (size *% 0.5)), Length.zero, Length.zero), EvHorzEmpty))


(*  -- 'quarterwidth_kern': inserts a solid backward quaterwidth kern for CJK characters -- *)
let quarterwidth_kern ctx script : lb_box =
  let size = get_corrected_font_size ctx script in
    LBPure(LBAtom((natural (Length.negate (size *% 0.25)), Length.zero, Length.zero), EvHorzEmpty))


let breakable_space lphbf ctx () : lb_box =
  let dscrid = DiscretionaryID.fresh () in
  let lphb1 = lphbf ctx.before_word_break in
  let lphb2 = lphbf ctx.after_word_break in
    LBDiscretionary(ctx.space_badness, dscrid, [pure_space ctx], lphb1, lphb2)


let unbreakable_space ctx : lb_box =
    LBPure(pure_space ctx)


let inner_string_pure (ctx : context_main) (script : script) (uchseglst : uchar_segment list) : lb_pure_box =
  let hsinfo = get_string_info ctx script in
  let (otxt, wid, hgt, dpt) = FontInfo.get_metrics_of_word hsinfo uchseglst in
    LBAtom((natural wid, hgt, dpt), EvHorzString(hsinfo, hgt, dpt, otxt))


let generate_separation_list (uchseglstlst : (uchar_segment list) list) : (uchar_segment list * uchar_segment list) list =
  let rec aux acc revprefix suffix =
    match suffix with
    | [] ->
        Alist.to_list acc

    | uchseglst :: [] ->
        Alist.to_list acc

    | uchseglst :: suffixnew ->
        let revprefixnew = Alist.append revprefix uchseglst in
        let accnew = Alist.extend acc (Alist.to_list revprefixnew, List.concat suffixnew) in
          aux accnew revprefixnew suffixnew
  in
    aux Alist.empty Alist.empty uchseglstlst


let make_string_atom (hsinfo : horz_string_info) (uchseglst : uchar_segment list) : lb_pure_box =
  let (otxt, wid, hgt, dpt) = FontInfo.get_metrics_of_word hsinfo uchseglst in
    LBAtom((natural wid, hgt, dpt), EvHorzString(hsinfo, hgt, dpt, otxt))


(* -- 'inner_string': makes an alphabetic word or a CJK character -- *)
let inner_string (ctx : context_main) (script : script) (uchseglst : uchar_segment list) : lb_box list =
  let hsinfo = get_string_info ctx script in
    match LoadHyph.lookup ctx.left_hyphen_min ctx.right_hyphen_min ctx.hyphen_dictionary uchseglst with
    | LoadHyph.Single(uchseglst) ->
        [LBPure(make_string_atom hsinfo uchseglst)]

    | LoadHyph.Fractions(uchseglstlst) ->
        let uchseglst0 = List.concat uchseglstlst in
        let lphb0 = make_string_atom hsinfo uchseglst0 in
        let lphb_hyphen = inner_string_pure ctx script [(Uchar.of_char '-', [])] in  (* temporary; should be variable *)
        let seplst = generate_separation_list uchseglstlst in
        let dscrlst =
          seplst |> List.fold_left (fun dscracc (uchseglstP, uchseglstS) ->
            let lphbP = make_string_atom hsinfo uchseglstP in
            let lphbS = make_string_atom hsinfo uchseglstS in
            let dscrid = DiscretionaryID.fresh () in
              Alist.extend dscracc (dscrid, [lphbP; lphb_hyphen], [lphbS])
          ) Alist.empty |> Alist.to_list
        in
        [LBDiscretionaryList(ctx.hyphen_badness, [lphb0], dscrlst)]


let discretionary_if_breakable alw badns lphb () =
  match alw with
  | AllowBreak ->
      let dscrid = DiscretionaryID.fresh () in
        LBDiscretionary(badns, dscrid, [lphb], [], [])

  | PreventBreak ->
      LBPure(lphb)


(* temporary; should refer to the context for spacing between two scripts *)
let pure_space_between_classes (ctx1, script1, lbc1) (ctx2, script2, lbc2) =
  let size1 = get_corrected_font_size ctx1 script1 in
  let size2 = get_corrected_font_size ctx2 script2 in
  let sizeM = Length.max size1 size2 in
  let hwhard1 = (pure_halfwidth_space_hard size1) in
  let hwsoft1 = (pure_halfwidth_space_soft size1) in
  let hwsoft2 = (pure_halfwidth_space_soft size2) in
  let hwsoftM = (pure_halfwidth_space_soft sizeM) in
  let hwhardM = (pure_halfwidth_space_hard sizeM) in
  match (lbc1, lbc2) with
  | (JLCP, JLOP) -> Some(hwsoftM)
  | (JLCM, JLOP) -> Some(hwsoftM)
  | (JLFS, JLOP) -> Some(hwhardM)
  | (_   , JLOP) -> Some(hwsoft2)
  | (JLCP, JLCM) -> None
  | (JLCP, JLFS) -> None
  | (JLCP, _   ) -> Some(hwsoft1)
  | (JLCM, _   ) -> Some(hwsoft1)
  | (JLFS, _   ) -> Some(hwhard1)
      (* TEMPORARY; SHOULD WRITE MORE based on JLreq *)
(*
  | (JLNonstarter(_, _), PreWord(_, _, _)) -> full_space
*)
  | _ -> None


let space_between_chunks info1 alw info2 : lb_box list =
  let (ctx1, script1, lbc1) = info1 in
  let (ctx2, script2, lbc2) = info2 in
  let badns = max ctx1.space_badness ctx2.space_badness in
  if not (script_equal script1 script2) then
    let size = Length.max ctx1.font_size ctx2.font_size in
      match pure_space_between_scripts ctx1 ctx2 size script1 lbc1 script2 lbc2 with
      | Some(lphb) ->
          [discretionary_if_breakable alw badns lphb ()]

      | None ->
        (* -- if there is no space between scripts -- *)
          begin
            match pure_space_between_classes info1 info2 with
            | None       -> [discretionary_if_breakable alw badns (adjacent_space ctx1 ctx2) ()]
            | Some(lphb) -> [discretionary_if_breakable alw badns lphb ()]
          end
  else
  (* -- if scripts are the same -- *)
    match pure_space_between_classes info1 info2 with
    | None       -> [discretionary_if_breakable alw badns (adjacent_space ctx1 ctx2) ()]
    | Some(lphb) -> [discretionary_if_breakable alw badns lphb ()]


let space_between_chunks_pure info1 info2 : lb_pure_box list =
  let (ctx1, script1, lbc1) = info1 in
  let (ctx2, script2, lbc2) = info2 in
  if not (script_equal script1 script2) then
    let size = Length.max ctx1.font_size ctx2.font_size in
      match pure_space_between_scripts ctx1 ctx2 size script1 lbc1 script2 lbc2 with
      | Some(lphb) ->
          [lphb]

      | None ->
          begin
            match pure_space_between_classes info1 info2 with
            | None       -> [adjacent_space ctx1 ctx2]
            | Some(lphb) -> [lphb]
          end
  else
  (* -- if scripts are the same -- *)
    match pure_space_between_classes info1 info2 with
    | None       -> [adjacent_space ctx1 ctx2]
    | Some(lphb) -> [lphb]


(* -- 'ideographic_single': converts single CJK character, not depending on adjacent characters -- *)
let ideographic_single ctx script lbc uchlst =
  let lphbraw = LBPure(inner_string_pure ctx script uchlst) in
  let hwkern = halfwidth_kern ctx script in
  let qwkern = quarterwidth_kern ctx script in
    match lbc with
    | JLCP  (* -- JLreq cl-02; fullwidth close punctuation -- *)
    | JLFS  (* -- JLreq cl-06; kuten (fullwidth full stops) -- *)
    | JLCM  (* -- JLreq cl-07; touten (fullwidth commas) -- *)
      -> [lphbraw; hwkern]

    | JLOP  (* -- JLreq cl-01; fullwidth open punctuation -- *)
      -> [hwkern; lphbraw]

    | JLMD  (* -- JLreq cl-05; nakaten (fullwidth middle dot, fullwidth semicolon, etc.) -- *)
      -> [qwkern; lphbraw; qwkern]

    | _ -> [lphbraw]


type chunk_accumulator =
  | AccInitial
  | AccNone
  | AccSome    of (context_main * script * line_break_class) * break_opportunity


let chunks_to_boxes (lphbf : horz_box list -> lb_pure_box list) (script_before : script) (chunklst : line_break_chunk list) (script_after : script) : lb_box list =
  let rec aux lhbacc optprev chunklst =
    match chunklst with
    | [] ->
        begin
          match optprev with
          | AccInitial ->
              []
                (* temporary; it may be better to insert spaces
                   using 'script_before' and 'script_after',
                   but we do not have any input context for it *)

          | AccNone ->
              Alist.to_list lhbacc

          | AccSome(infoprev, alw) ->
              let (ctxprev, _, _) = infoprev in
              let info_after = (ctxprev, script_after, XX) in
              let autospace = space_between_chunks infoprev alw info_after in
              Alist.to_list (Alist.append lhbacc autospace)
        end

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lhblstmain) =
          match chunkmain with
          | Space ->
              (AccNone, [breakable_space lphbf ctx ()])

          | UnbreakableSpace ->
              (AccNone, [unbreakable_space ctx])

          | AlphabeticChunk(script, lbcfirst, lbclast, uchseglst, alwnext) ->
              let opt = AccSome(((ctx, script, lbclast), alwnext)) in
              let lhblststr = inner_string ctx script uchseglst in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks info_before PreventBreak (ctx, script, lbcfirst) in
                    (opt, List.append autospace lhblststr)

                | AccNone ->
                    (opt, lhblststr)

                | AccSome(infoprev, alw) ->
                    let autospace = space_between_chunks infoprev alw (ctx, script, lbcfirst) in
                    (opt, List.append autospace lhblststr)
              end

          | IdeographicChunk(script, lbc, uchseg, alwnext) ->
              let opt = AccSome((ctx, script, lbc), alwnext) in
              let lhblststr = ideographic_single ctx script lbc [uchseg] in
              begin
                match optprev with
                | AccNone ->
                    (opt, lhblststr)

                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks info_before PreventBreak (ctx, script, lbc) in
                    (opt, List.append autospace lhblststr)

                | AccSome((infoprev, alw)) ->
                    let autospace = space_between_chunks infoprev alw (ctx, script, lbc) in
                    (opt, List.append autospace lhblststr)
              end
        in
        aux (Alist.append lhbacc lhblstmain) opt chunktail
  in
  aux Alist.empty AccInitial chunklst


let chunks_to_boxes_pure (script_before : script) (chunklst : line_break_chunk list) (script_after : script) : lb_pure_box list =
  let rec aux lphbacc optprev chunklst =
    match chunklst with
    | [] ->
        begin
          match optprev with
          | AccInitial ->
              []

          | AccNone ->
              Alist.to_list lphbacc

          | AccSome(infoprev, alw) ->
              let (ctx, _, _) = infoprev in
              let info_after = (ctx, script_after, XX) in
              let autospace = space_between_chunks_pure infoprev info_after in
              Alist.to_list (Alist.append lphbacc autospace)
        end

    | chunk :: chunktail ->
        let (ctx, chunkmain) = chunk in
        let (opt, lphblstmain) =
          match chunkmain with
          | Space ->
              (AccNone, [pure_space ctx])

          | UnbreakableSpace ->
              (AccNone, [pure_space ctx])

          | AlphabeticChunk(script, lbcfirst, lbclast, uchlst, alw) ->
              let opt = AccSome(((ctx, script, lbclast), alw)) in
              let lphblstmain = [inner_string_pure ctx script uchlst] in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks_pure info_before (ctx, script, lbcfirst) in
                    (opt, List.append autospace lphblstmain)

                | AccNone ->
                    (opt, lphblstmain)

                | AccSome((infoprev, alw)) ->
                    let autospace = space_between_chunks_pure infoprev (ctx, script, lbcfirst) in
                    (opt, List.append autospace lphblstmain)
              end

          | IdeographicChunk(script, lbc, uch, alw) ->
              let opt = AccSome(((ctx, script, lbc), alw)) in
              let lphblstmain = [inner_string_pure ctx script [uch]] in
              begin
                match optprev with
                | AccInitial ->
                    let info_before = (ctx, script_before, XX) in
                    let autospace = space_between_chunks_pure info_before (ctx, script, lbc) in
                    (opt, List.append autospace lphblstmain)

                | AccNone ->
                    (opt, lphblstmain)

                | AccSome((infoprev, alw)) ->
                    let autospace = space_between_chunks_pure infoprev (ctx, script, lbc) in
                    (opt, List.append autospace lphblstmain)
              end
        in
          aux (Alist.append lphbacc lphblstmain) opt chunktail
  in
  aux Alist.empty AccInitial chunklst


module Types = Types_
module Primitives = Primitives_
module Evaluator = Evaluator_
module Vm = Vm_
open Types
open Display


type file_path = string


exception NoLibraryRootDesignation
exception NoInputFileDesignation
exception CyclicFileDependency of file_path list
exception NotALibraryFile  of file_path * Typeenv.t * mono_type
exception NotADocumentFile of file_path * Typeenv.t * mono_type


type line =
  | NormalLine  of string
  | DisplayLine of string
  | NormalLineOption  of string option
  | DisplayLineOption of string option

type error_category =
  | Lexer
  | Parser
  | Typechecker
  | Evaluator
  | Interface
  | System


let show_error_category = function
  | Lexer       -> "Syntax Error at Lexer"
  | Parser      -> "Syntax Error at Parser"
  | Typechecker -> "Type Error"
  | Evaluator   -> "Error during Evaluation"
  | Interface   -> "Error"
  | System      -> "Error"


let report_error (cat : error_category) (lines : line list) =
  let rec aux lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail
    | NormalLineOption(Some(s)) :: tail
      -> begin print_endline ("    " ^ s) ; aux tail end
    | DisplayLine(s) :: tail
    | DisplayLineOption(Some(s)) :: tail
      -> begin print_endline ("      " ^ s); aux tail end
    | _ :: tail -> aux tail
  in
  let first lst =
    match lst with
    | []                     -> ()
    | NormalLine(s) :: tail
    | NormalLineOption(Some(s)) :: tail
      -> begin print_endline s; aux tail end
    | DisplayLine(s) :: tail
    | DisplayLineOption(Some(s)) :: tail
      -> begin print_endline ("\n      " ^ s); aux tail end
    | _ :: tail -> aux tail
  in
  begin
    print_string ("! [" ^ (show_error_category cat) ^ "] ");
    first lines;
    exit 1;
  end

let make_candidates_message (candidates : string list) =
  let add_quote s = "'" ^ s ^ "'" in
  let rec aux lst =
    match List.rev lst with
    | []        -> ""
    | s :: []   -> add_quote s
    | s :: rest  -> (String.concat ", " (List.map add_quote (List.rev rest))) ^ " or " ^ (add_quote s)
  in
  match candidates with
  | [] -> None
  | _  -> Some("Did you mean " ^ (aux candidates) ^ "?")


module FileDependencyGraph = DirectedGraph.Make
  (struct
    type t = file_path
    let compare = String.compare
    let show s = Filename.basename s
  end)


type file_info =
  | DocumentFile of untyped_abstract_tree
  | LibraryFile  of untyped_abstract_tree


let make_absolute_path curdir headerelem =
  match headerelem with
  | HeaderRequire(s) -> Config.resolve_dist_path (Filename.concat "dist/packages" s ^ ".satyh")
  | HeaderImport(s)  -> Filename.concat curdir (s ^ ".satyh")


let rec register_library_file (dg : file_info FileDependencyGraph.t) (file_path_in : file_path) : unit =
  begin
    Logging.begin_to_parse_file file_path_in;
    let curdir = Filename.dirname file_path_in in
    let file_in = open_in file_path_in in
    let (header, utast) = ParserInterface.process file_path_in (Lexing.from_channel file_in) in
    FileDependencyGraph.add_vertex dg file_path_in (LibraryFile(utast));
    header |> List.iter (fun headerelem ->
      let file_path_sub = make_absolute_path curdir headerelem in
      begin
        if FileDependencyGraph.mem_vertex file_path_sub dg then () else
          register_library_file dg file_path_sub
      end;
      FileDependencyGraph.add_edge dg file_path_in file_path_sub
    )
  end


let eval_library_file (tyenv : Typeenv.t) (env : environment) (file_name_in : file_path) (utast : untyped_abstract_tree) : Typeenv.t * environment =
  Logging.begin_to_read_file file_name_in;
  let (ty, tyenvnew, ast) = Typechecker.main tyenv utast in
  Logging.pass_type_check None;
  if OptionState.type_check_only () then (tyenvnew, env)
  else
  match ty with
  | (_, BaseType(EnvType)) ->
      let value =
        if OptionState.bytecomp_mode () then
          Bytecomp.compile_and_exec env ast
        else
          Evaluator.interpret env ast
      in
      begin
        match value with
        | EvaluatedEnvironment(envnew) -> (tyenvnew, envnew)
        | _                            -> failwith "not an 'EvaluatedEnvironment(...)'"
      end

  | _ -> raise (NotALibraryFile(file_name_in, tyenvnew, ty))


(* -- initialization that should be performed before every cross-reference-solving loop -- *)
let reset () =
  begin
    FontInfo.initialize ();
    ImageInfo.initialize ();
  end


(* -- initialization that should be performed before typechecking -- *)
let initialize (dump_file : file_path) =
  begin
    FreeID.initialize ();
    BoundID.initialize ();
    Typeenv.initialize_id ();
    EvalVarID.initialize ();
    StoreID.initialize ();
    let dump_file_exists = CrossRef.initialize dump_file in
    let (tyenv, env) = Primitives.make_environments () in
    begin
      if OptionState.bytecomp_mode () then
        Bytecomp.compile_environment env
      else
        ()
    end;
    (tyenv, env, dump_file_exists)
  end


let output_pdf pdfret =
  HandlePdf.write_to_file pdfret


module StoreIDMap = Map.Make(StoreID)


type frozen_environment = location EvalVarIDMap.t * (syntactic_value StoreIDHashTable.t) ref * syntactic_value StoreIDMap.t


let freeze_environment (env : environment) : frozen_environment =
  let open MyUtil in
  let (valenv, stenvref) = env in
  let stmap =
    StoreIDMap.empty @|> (!stenvref) @|> StoreIDHashTable.fold (fun stid value stmap ->
      stmap |> StoreIDMap.add stid value
    )
  in
  (valenv, stenvref, stmap)


let unfreeze_environment ((valenv, stenvref, stmap) : frozen_environment) : environment =
  let stenv = StoreIDHashTable.create 128 in
  stmap |> StoreIDMap.iter (fun stid value -> StoreIDHashTable.add stenv stid value);
  stenvref := stenv;
  (valenv, ref stenv)


let register_document_file (dg : file_info FileDependencyGraph.t) (file_path_in : file_path) : unit =
  begin
    Logging.begin_to_parse_file file_path_in;
    let file_in = open_in file_path_in in
    let curdir = Filename.dirname file_path_in in
    let (header, utast) = ParserInterface.process file_path_in (Lexing.from_channel file_in) in
    FileDependencyGraph.add_vertex dg file_path_in (DocumentFile(utast));
    header |> List.iter (fun headerelem ->
      let file_path_sub = make_absolute_path curdir headerelem in
      begin
        if FileDependencyGraph.mem_vertex file_path_sub dg then () else
          register_library_file dg file_path_sub;
      end;
      FileDependencyGraph.add_edge dg file_path_in file_path_sub
    )
  end


let eval_document_file (tyenv : Typeenv.t) (env : environment) (file_path_in : file_path) (utast : untyped_abstract_tree) (file_path_out : file_path) (file_path_dump : file_path) =
    Logging.begin_to_read_file file_path_in;
    let (ty, _, ast) = Typechecker.main tyenv utast in
    Logging.pass_type_check (Some(Display.string_of_mono_type tyenv ty));
    if OptionState.type_check_only () then ()
    else
    let env_freezed = freeze_environment env in
      match ty with
      | (_, BaseType(DocumentType)) ->
          let rec aux i =
            Logging.start_evaluation i;
            reset ();
            let env = unfreeze_environment env_freezed in
            let valuedoc =
              if OptionState.bytecomp_mode () then
                Bytecomp.compile_and_exec env ast
              else
                Evaluator.interpret env ast
            in
            Logging.end_evaluation ();
            begin
              match valuedoc with
              | DocumentValue(pagesize, pagecontf, pagepartsf, imvblst) ->
                  Logging.start_page_break ();
                  let pdf = PageBreak.main file_path_out pagesize pagecontf pagepartsf imvblst in
                  begin
                    match CrossRef.needs_another_trial file_path_dump with
                    | CrossRef.NeedsAnotherTrial ->
                        Logging.needs_another_trial ();
                        aux (i + 1);

                    | CrossRef.CountMax ->
                        Logging.achieve_count_max ();
                        output_pdf pdf;
                        Logging.end_output file_path_out;

                    | CrossRef.CanTerminate unresolved_crossrefs ->
                        Logging.achieve_fixpoint unresolved_crossrefs;
                        output_pdf pdf;
                        Logging.end_output file_path_out;
                  end

              | _ ->
                  Format.printf "valuedoc: %a\n" pp_syntactic_value valuedoc; failwith "main; not a DocumentValue(...)"
            end
          in
          aux 1

      | _ ->
          raise (NotADocumentFile(file_path_in, tyenv, ty))


let error_log_environment suspended =
  try
    suspended ()
  with
  | NoLibraryRootDesignation ->
      report_error Interface [
        NormalLine("cannot determine where the SATySFi library root is;");
        NormalLine("set appropriate environment variables.");
      ]

  | NoInputFileDesignation ->
      report_error Interface [
        NormalLine("no input file designation.");
      ]

  | CyclicFileDependency(cycle) ->
      report_error Interface (
        (NormalLine("cyclic dependency detected:")) ::
        (cycle |> List.map (fun s -> DisplayLine(s)))
      )

  | Config.DistFileNotFound(file_name, dirlst) ->
      report_error Interface (List.append [
        NormalLine("package file not found:");
        DisplayLine(file_name);
        NormalLine("candidate directories for the SATySFi library root:");
      ] (dirlst |> List.map (fun dir -> DisplayLine(dir))))

  | NotALibraryFile(file_name_in, tyenv, ty) ->
      report_error Typechecker [
        NormalLine("file '" ^ file_name_in ^ "' is not a header file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | NotADocumentFile(file_name_in, tyenv, ty) ->
      report_error Typechecker [
        NormalLine("file '" ^ file_name_in ^ "' is not a document file; it is of type");
        DisplayLine(string_of_mono_type tyenv ty);
      ]

  | CrossRef.InvalidYOJSON(dumpfile, msg) ->
      report_error Interface [
        NormalLine("dump file '" ^ dumpfile ^ "' is NOT a valid YOJSON file:");
        DisplayLine(msg);
      ]

  | CrossRef.DumpFileOtherThanAssoc(dumpfile) ->
      report_error Interface [
        NormalLine("in the dump file '" ^ dumpfile ^ "':");
        NormalLine("the content is NOT a dictionary.");
      ]

  | CrossRef.DumpFileValueOtherThanString(dumpfile, key, jsonstr) ->
      report_error Interface [
        NormalLine("in the dump file '" ^ dumpfile ^ "':");
        NormalLine("the value associated with the key '" ^ key ^ "' is NOT a string;");
        DisplayLine(jsonstr);
      ]

  | LoadFont.InvalidYOJSON(srcpath, msg) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the content is NOT a valid YOJSON format;");
        DisplayLine(msg);
      ]

  | LoadFont.FontHashOtherThanDictionary(srcpath) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the content is NOT a dictionary.");
      ]

  | LoadFont.FontHashElementOtherThanVariant(srcpath, abbrev, jsonstr) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with the font name '" ^ abbrev ^ "' is NOT a YOJSON variant;");
        DisplayLine(jsonstr);
      ]

  | LoadFont.MultipleDesignation(srcpath, abbrev, key) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with font name '" ^ abbrev ^ "' "
                     ^ "has multiple designations for '" ^ key ^ "'.");
      ]

  | LoadFont.UnexpectedYOJSONKey(srcpath, abbrev, key) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with font name '" ^ abbrev ^ "' "
                     ^ "has an unexpected designation key '" ^ key ^ "'.");
      ]

  | LoadFont.UnexpectedYOJSONValue(srcpath, abbrev, key, jsonstr) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with font name '" ^ abbrev ^ "' "
                     ^ "has an unexpected designation value for '" ^ key ^ "';");
        DisplayLine(jsonstr);
      ]

  | LoadFont.MissingRequiredYOJSONKey(srcpath, abbrev, key) ->
      report_error Interface [
        NormalLine("in the font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with font name '" ^ abbrev ^ "' "
                     ^ "does NOT have the required designation key '" ^ key ^ "'.");
      ]

  | SetDefaultFont.InvalidYOJSON(srcpath, msg) ->
      report_error Interface [
        NormalLine("the default font hash file '" ^ srcpath ^ "' is NOT a valid YOJSON file;");
        DisplayLine(msg);
      ]

  | SetDefaultFont.OtherThanDictionary(srcpath) ->
      report_error Interface [
        NormalLine("in the default font hash file '" ^ srcpath ^ "':");
        NormalLine("the content is NOT a dictionary.");
      ]

  | SetDefaultFont.MissingRequiredScriptKey(srcpath, key_script) ->
      report_error Interface [
        NormalLine("in the default font hash file '" ^ srcpath ^ "':");
        NormalLine("missing required script key '" ^ key_script ^ "'");
      ]

  | SetDefaultFont.MissingRequiredKey(srcpath, key_script, key) ->
      report_error Interface [
        NormalLine("in the default font hash file '" ^ srcpath ^ "':");
        NormalLine("missing required key '" ^ key ^ "' for the script '" ^ key_script ^ "'");
      ]

  | SetDefaultFont.ElementOtherThanDictionary(srcpath, key_script, jsonstr) ->
      report_error Interface [
        NormalLine("in the default font hash file '" ^ srcpath ^ "':");
        NormalLine("the value associated with the script key '" ^ key_script ^ "' is NOT a dictionary.");
        DisplayLine(jsonstr);
      ]

  | SetDefaultFont.InvalidDataTypeOfKey(srcpath, key_script, key) ->
      report_error Interface [
        NormalLine("in the default font hash file '" ^ srcpath ^ ":");
        NormalLine("the value associated with the key '" ^ key ^ "' "
                      ^ "for the script '" ^ key_script ^ "' is of invalid data type.");
      ]

  | FontFormat.FailToLoadFontOwingToSize(srcpath) ->
      report_error Interface [
        NormalLine("font file '" ^ srcpath ^ "' is too large to be loaded.");
      ]

  | FontFormat.FailToLoadFontOwingToSystem(srcpath, msg) ->
      report_error Interface [
        NormalLine("cannot load font file '" ^ srcpath ^ "';");
        DisplayLine(msg);
      ]

  | FontFormat.BrokenFont(srcpath, msg) ->
      report_error Interface [
        NormalLine("font file '" ^ srcpath ^ "' is broken;");
        DisplayLine(msg);
      ]

  | FontFormat.CannotFindUnicodeCmap(srcpath) ->
      report_error Interface [
        NormalLine("font file '" ^ srcpath ^ "' does not have 'cmap' subtable for Unicode code points.");
      ]

  | FontInfo.InvalidFontAbbrev(abbrev) ->
      report_error Interface [
        NormalLine ("cannot find a font named '" ^ abbrev ^ "'.");
      ]

  | FontInfo.InvalidMathFontAbbrev(mfabbrev) ->
      report_error Interface [
        NormalLine("cannot find a math font named '" ^ mfabbrev ^ "'.");
      ]

  | FontInfo.NotASingleFont(abbrev, srcpath) ->
      report_error Interface [
        NormalLine("the font file '" ^ srcpath ^ "',");
        NormalLine("which is associated with the font name '" ^ abbrev ^ "',");
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

  | FontInfo.NotASingleMathFont(mfabbrev, srcpath) ->
      report_error Interface [
        NormalLine("the font file '" ^ srcpath ^ "',");
        NormalLine("which is associated with the math font name '" ^ mfabbrev ^ "',");
        NormalLine("is not a single font file; it is a TrueType collection.");
      ]

  | Lexer.LexError(rng, s) ->
      report_error Lexer [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(s);
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
      ]

  | Parsing.Parse_error             -> report_error Parser [ NormalLine("something is wrong."); ]
  | ParseErrorDetail(s)             -> report_error Parser [ NormalLine(s); ]

  | IllegalArgumentLength(rng, len, lenexp) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("this declaration has" ^ (string_of_int len) ^ " argument pattern(s),");
        NormalLine("but is expected to have " ^ (string_of_int lenexp) ^ ".");
      ]

  | ParserInterface.Error(rng) ->
      report_error Parser [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
      ]

  | Typechecker.UndefinedVariable(rng, mdlnmlst, varnm, candidates) ->
      let s = String.concat "." (List.append mdlnmlst [varnm]) in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined variable '" ^ s ^ "'.");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typechecker.UndefinedConstructor(rng, constrnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined constructor '" ^ constrnm ^ "'.");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typechecker.TooManyArgument(rngcmdapp, tyenv, tycmd) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine(Range.to_source rngcmdapp);
        NormalLine(Range.to_underline_string rngcmdapp);
        NormalLine("too many argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ".")
      ]

  | Typechecker.NeedsMoreArgument(rngcmdapp, tyenv, tycmd, tyreq) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngcmdapp) ^ ":");
        NormalLine(Range.to_source rngcmdapp);
        NormalLine(Range.to_underline_string rngcmdapp);
        NormalLine("needs more mandatory argument(s);");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ",");
        NormalLine("and another argument of type");
        DisplayLine(Display.string_of_mono_type tyenv tyreq);
        NormalLine("is needed.");
      ]

  | Typechecker.InvalidOptionalCommandArgument(tyenv, tycmd, rngarg) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rngarg) ^ ":");
        NormalLine(Range.to_source rngarg);
        NormalLine(Range.to_underline_string rngarg);
        NormalLine("invalid application of an optional argument;");
        NormalLine("the command has type");
        DisplayLine((Display.string_of_mono_type tyenv tycmd) ^ ".");
      ]

  | Typechecker.UnknownUnitOfLength(rng, unitnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined unit of length '" ^ unitnm ^ "'.");
      ]

  | Typechecker.HorzCommandInMath(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("an inline command is used as a math command.");
      ]

  | Typechecker.MathCommandInHorz(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("a math command is used as an inline command.");
      ]

  | Typechecker.BreaksValueRestriction(rng) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("this expression breaks the value restriction;");
        NormalLine("it should be a syntactic function.");
      ]

  | Typechecker.MultiplePatternVariable(rng1, rng2, varnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng1));
        NormalLine(Range.to_source rng1);
        NormalLine(Range.to_underline_string rng1);
        NormalLine("and at " ^ (Range.to_string rng2) ^ ":");
        NormalLine(Range.to_source rng2);
        NormalLine(Range.to_underline_string rng2);
        NormalLine("pattern variable '" ^ varnm ^ "' is bound more than once.");
      ]

  | Typechecker.MultipleFieldInRecord(rng, fldnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("this record expression has more than one field for '" ^ fldnm ^ "'.");
      ]

  | Typechecker.ApplicationOfNonFunction(rng, tyenv, ty) ->
      let strty = string_of_mono_type tyenv ty in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("this expression has type");
        DisplayLine(strty);
        NormalLine("and thus it cannot be applied to arguments.");
      ]

  | Typeenv.IllegalNumberOfTypeArguments(rng, tynm, lenexp, lenerr) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("'" ^ tynm ^ "' is expected to have " ^ (string_of_int lenexp) ^ " type argument(s),");
        NormalLine("but it has " ^ (string_of_int lenerr) ^ " type argument(s) here.");
      ]

  | Typeenv.UndefinedTypeName(rng, mdlnmlst, tynm, candidates) ->
      let s = String.concat "." (List.append mdlnmlst [tynm]) in
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined type name '" ^ s ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.UndefinedModuleName(rng, mdlnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined module name '" ^ mdlnm ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.UndefinedTypeArgument(rng, tyargnm, candidates) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("undefined type argument '" ^ tyargnm ^ "'");
        NormalLineOption(make_candidates_message candidates);
      ]

  | Typeenv.CyclicTypeDefinition(reslist) ->
      report_error Typechecker (
        (NormalLine("cyclic synonym type definition:"))
        :: (List.map (fun (rng, strty) -> DisplayLine(strty ^ " (at " ^ (Range.to_string rng) ^ ")")) reslist)
      )

  | Typeenv.MultipleTypeDefinition(rng1, rng2, tynm) ->
      report_error Typechecker [
        NormalLine("parallel type definition by the same name:");
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng1) ^ ")");
        NormalLine(Range.to_source rng1);
        NormalLine(Range.to_underline_string rng1);
        DisplayLine(tynm ^ " (at " ^ (Range.to_string rng2) ^ ")");
        NormalLine(Range.to_source rng2);
        NormalLine(Range.to_underline_string rng2);
      ]

  | Typeenv.NotProvidingTypeImplementation(rng, tynm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("The implementation does not provide type '" ^ tynm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotProvidingValueImplementation(rng, varnm) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("The implementation does not provide value '" ^ varnm ^ "',");
        NormalLine("which is required by the signature.");
      ]

  | Typeenv.NotMatchingInterface(rng, varnm, tyenv1, pty1, tyenv2, pty2) ->
      report_error Typechecker [
        NormalLine("at " ^ (Range.to_string rng) ^ ":");
        NormalLine(Range.to_source rng);
        NormalLine(Range.to_underline_string rng);
        NormalLine("The implementation of value '" ^ varnm ^ "' has type");
        DisplayLine(Display.string_of_poly_type tyenv1 pty1);
        NormalLine("which is inconsistent with the type required by the signature");
        DisplayLine(Display.string_of_poly_type tyenv2 pty2);
      ]

  | Typechecker.ContradictionError(tyenv, ((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = string_of_mono_type tyenv ty1 in
      let strty2 = string_of_mono_type tyenv ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true)   ->
            ([NormalLine("(cannot report position; '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')")], strty1, strty2, [])
        | (true, false)  ->
            ([
              NormalLine("at " ^ strrng2 ^ ":");
              NormalLine(Range.to_source rng2);
              NormalLine(Range.to_underline_string rng2);
            ], strty2, strty1, [])
        | (false, true)  ->
            ([
              NormalLine("at " ^ strrng1 ^ ":");
              NormalLine(Range.to_source rng1);
              NormalLine(Range.to_underline_string rng1);
            ], strty1, strty2, [])
        | (false, false) ->
            ([
              NormalLine("at " ^ strrng1 ^ ":");
              NormalLine(Range.to_source rng1);
              NormalLine(Range.to_underline_string rng1);
            ], strty1, strty2, [
              NormalLine("This constraint is required by the expression");
              NormalLine("at " ^ strrng2 ^ ".");
              NormalLine(Range.to_source rng2);
              NormalLine(Range.to_underline_string rng2);
            ])
      in
        report_error Typechecker (posmsg @ [
          NormalLine("this expression has type");
          DisplayLine(strtyA ^ ",");
          NormalLine("but is expected of type");
          DisplayLine(strtyB ^ ".");
        ] @ additional)

  | Typechecker.InclusionError(tyenv, ((rng1, _) as ty1), ((rng2, _) as ty2)) ->
      let strty1 = string_of_mono_type tyenv ty1 in
      let strty2 = string_of_mono_type tyenv ty2 in
      let strrng1 = Range.to_string rng1 in
      let strrng2 = Range.to_string rng2 in
      let (posmsg, strtyA, strtyB, additional) =
        match (Range.is_dummy rng1, Range.is_dummy rng2) with
        | (true, true)   -> ("(cannot report position; '" ^ (Range.message rng1) ^ "', '" ^ (Range.message rng2) ^ "')", strty1, strty2, [])
        | (true, false)  -> ("at " ^ strrng2 ^ ":", strty2, strty1, [])
        | (false, true)  -> ("at " ^ strrng1 ^ ":", strty1, strty2, [])
        | (false, false) -> ("at " ^ strrng1 ^ ":", strty1, strty2, [ NormalLine("This constraint is required by the expression");
                                                                      NormalLine("at " ^ strrng2 ^ "."); ])
      in
        report_error Typechecker (List.append [
          NormalLine(posmsg);
          NormalLine("this expression has types");
          DisplayLine(strtyA);
          NormalLine("and");
          DisplayLine(strtyB);
          NormalLine("at the same time, but these are incompatible.");
        ] additional)

  | Evaluator.EvalError(s)
  | Vm.ExecError(s)
      -> report_error Evaluator [ NormalLine(s); ]

  | Sys_error(s) ->
      report_error System [ NormalLine(s); ]


let arg_version () =
  begin
    print_string (
        "  SATySFi version 0.0.2\n"
(*
      ^ "  (in the middle of the transition from Macrodown)\n"
      ^ "    ____   ____       ________     _____   ______\n"
      ^ "    \\   \\  \\   \\     /   _____|   /   __| /      \\\n"
      ^ "     \\   \\  \\   \\   /   /        /   /   /   /\\   \\\n"
      ^ "     /    \\  \\   \\  \\   \\       /   /   /   /  \\   \\\n"
      ^ "    /      \\  \\   \\  \\   \\     /   /   /   /    \\   \\\n"
      ^ "   /   /\\   \\  \\   \\  \\   \\   /   /   /   /      \\   \\\n"
      ^ "  /   /  \\   \\  \\   \\  \\___\\ /___/   /   /        \\   \\\n"
      ^ " /   /    \\   \\  \\   \\              /   /_________/   /\n"
      ^ "/___/      \\___\\  \\___\\            /_________________/\n"
*)
    );
    exit 0;
  end


let arg_output curdir s =
  let file_path =
    if Filename.is_relative s then Filename.concat curdir s else s
  in
  OptionState.set_output_file file_path


let handle_anonimous_arg (curdir : file_path) (s : file_path) =
  let file_path =
    if Filename.is_relative s then Filename.concat curdir s else s
  in
  OptionState.set_input_file file_path


let arg_spec_list curdir =
  [
    ("-o"                , Arg.String(arg_output curdir)             , " Specify output file"              );
    ("--output"          , Arg.String(arg_output curdir)             , " Specify output file"              );
    ("-v"                , Arg.Unit(arg_version)                     , " Prints version"                   );
    ("--version"         , Arg.Unit(arg_version)                     , " Prints version"                   );
    ("--full-path"       , Arg.Unit(OptionState.set_show_full_path)  , " Displays paths in full-path style");
    ("--debug-show-bbox" , Arg.Unit(OptionState.set_debug_show_bbox) , " Outputs bounding boxes for glyphs");
    ("--debug-show-space", Arg.Unit(OptionState.set_debug_show_space), " Outputs boxes for spaces"         );
    ("-t"                , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"        );
    ("--type-check-only" , Arg.Unit(OptionState.set_type_check_only) , " Stops after type checking"        );
    ("-b"                , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"            );
    ("--bytecomp"        , Arg.Unit(OptionState.set_bytecomp_mode)   , " Use bytecode compiler"            );
  ]


let setup_root_dirs () =
  let runtime_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "SATYSFI_RUNTIME" with
      | None    -> []
      | Some(s) -> [s]
    else
      ["/usr/local/share/satysfi"; "/usr/share/satysfi"]
  in
  let user_dirs =
    if Sys.os_type = "Win32" then
      match Sys.getenv_opt "userprofile" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
    else
      match Sys.getenv_opt "HOME" with
      | None    -> []
      | Some(s) -> [Filename.concat s ".satysfi"]
  in
  let ds = List.append user_dirs runtime_dirs in
    match ds with
    | []     -> raise NoLibraryRootDesignation
    | _ :: _ -> Config.initialize ds


let () =
  error_log_environment (fun () ->
    setup_root_dirs ();
    let curdir = Sys.getcwd () in
    Arg.parse (arg_spec_list curdir) (handle_anonimous_arg curdir) "";
    let input_file =
      match OptionState.input_file () with
      | None    -> raise NoInputFileDesignation
      | Some(v) -> v
    in
    let output_file =
      match OptionState.output_file () with
      | Some(v) ->
          v

      | None ->
          begin
            try (Filename.chop_extension input_file) ^ ".pdf" with
            | Invalid_argument(_) -> input_file ^ ".pdf"
          end
    in
    Logging.target_file output_file;
    let dump_file = (Filename.remove_extension output_file) ^ ".satysfi-aux" in
    let (tyenv, env, dump_file_exists) = initialize dump_file in
    Logging.dump_file dump_file_exists dump_file;

    let dg = FileDependencyGraph.create 32 in
    register_document_file dg input_file;
    match FileDependencyGraph.find_cycle dg with
    | Some(cycle) ->
        raise (CyclicFileDependency(cycle))

    | None ->
        FileDependencyGraph.backward_bfs_fold (fun (tyenv, env) file_path_in file_info ->
          match file_info with
          | DocumentFile(utast) ->
              eval_document_file tyenv env file_path_in utast output_file dump_file;
              (tyenv, env)

          | LibraryFile(utast) ->
              eval_library_file tyenv env file_path_in utast
        ) (tyenv, env) dg |> ignore
  )

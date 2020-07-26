
open MyUtil
open LengthInterface
open HorzBox

val solidify : vert_box list -> intermediate_vert_box list

val main : abs_path -> page_size -> column_hook_func -> page_content_scheme_func -> page_parts_scheme_func -> vert_box list -> HandlePdf.t

val main_two_column : abs_path -> page_size -> length -> column_hook_func -> page_content_scheme_func -> page_parts_scheme_func -> vert_box list -> HandlePdf.t

val adjust_to_first_line : intermediate_vert_box list -> length * length

val adjust_to_last_line : intermediate_vert_box list -> length * length

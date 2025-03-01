# namesettings

box::use(
  # app/logic/getEnumlist[getEnumlist],
  # app/logic/SimDat_analyser_result[SimDat_analyser_result],
  # app/logic/AnsysFunctions/SMCDela_Ekill_ENUM_just_write_the_numlist[SMCDela_Ekill_ENUM_just_write_the_numlist]
)


#' @export
Sequential_palette <- function() {
  return(c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys","Oranges", "OrRd", "PuBu", "PuBuGn",
           "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"))
}
#' @export
Qualitative_palette <- function() {
  return(c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Greys", "Set1", "Set2", "Set3"))
}
#' @export
Diverging_palette <- function() {
  return(c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
}
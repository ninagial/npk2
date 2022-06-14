#' Average loadings of a pair of items across two factor structures
#'
#' @param loadlist    list; a list of pattern matrices
#' @param itemlabels    character; a vector of item label strings
#'
#' @details rownames of pattern matrices must be the item labels
factor_count <- function(loadlist, itemlabels){
	cb = t(combn(itemlabels, 2))



}

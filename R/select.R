#'
#'
#' @param cname    column name, defaults to 'variable'
#' @param repl    replacement for cname, optional
#' @param del     logical; should the subs string be deleted
select_marker <- function(subs, data, del=T, cname, repl){
	if(missing(cname)) cname='variable'
	if(missing(repl)) repl='variable'
	out = data[grepl(subs, data[,cname]),]
	if (del) out[,cname] = gsub(subs, '', out[,cname])
	colnames(out) = gsub(cname, repl, colnames(out))
	out
}



bolden_val <- Vectorize(function(x, cut=.35){
		    if (abs(x)>cut){
			    return(sprintf("\\textbf{%.2f}",x))
		    } else { return(sprintf("%.2f",x))}
		})


texify_single_f <- function(x, fname='FactorXYZ'){
	x2 = apply(x[,-1],2,bolden_val)
	x = cbind(x[,1], x2)
	list(
	     paste(collapse = '\\\\\n', 
		   c(
		     paste(c(fname, rep('', ncol(x)-1)), collapse=' & '),
		     apply(x,1, paste, collapse=' & '))
	     )

	     )

}

texify_flist <- function(fit_obj, itemdf, fnames=NULL){
    assign = assign_items(fit_obj, itemdf=itemdf)
    flist = assign$tables
    if (is.null(fnames)) fnames <- sprintf('f%d', 1:length(flist))
    cat(
    sprintf('\n\\begin{tabular}{%s}\n', paste(collapse='', c('l', rep('c', ncol(assign$loadings))))),
    paste(collapse = '\\\\\n', 
	  unlist(Map(function(x,y) texify_single_f(x=x,fname=y),flist, sprintf("\\textbf{%s}", fnames)))),
    '\\\\\n\\end{tabular}\n'

)
}

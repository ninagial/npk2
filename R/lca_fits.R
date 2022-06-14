#' Find the Optimal Number of Classes for LCA
#'
#' @param items    character Vector of item codes
#' @example
#' lca_fits(gsmItems$item, data=gsm)
lca_fits <- function(items, data, ns=1:10){
	form = as.formula(sprintf('cbind(%s)~1', paste(collapse=", ", gsmItems$item)))
	lca_all = lapply(ns, function(ncl) poLCA::poLCA(form, data=lca_data, ncl=ncl, maxiter=9999))
	best_ncl = which.min(sapply(lca_all, function(x) x$aic))
	cat("Based on AIC, ", best_ncl, " classes are suggested.\n")
	ggplot2::ggplot(data.frame(ncl=ns, aic=sapply(lca_all, function(x) x$aic)), aes(ncl, aic))+geom_line() + stat_identity() + theme(panel.background=element_blank())+scale_x_discrete() + labs(x='Number of Classes', y='AIC')
	list(fits = lca_all, k=best_ncl)
}



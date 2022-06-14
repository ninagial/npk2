summarize_bia = function(bia_result, dependent, predictors) {
	   with(bia_result,{
	   pvals = 2 * stats::pt(abs(ests/var^0.5), df = df, lower.tail = FALSE)
	   outdf = data.frame(cbind(ests, var, ci[,1], ci[,2]), pvals )
	   colnames(outdf) = c("Estimate", "Std. error", "95% CI lower", "95% CI upper", "p")
	   rownames(outdf) = predictors
	   stargazer(outdf, summary=F, digits=2, type='html', title=dependent)
	   })
}


#' Summarize Boot-Impute-Analyze Results
#'
#' @param bia_result    result from \link{\code{impute}}
#' @param dependent     caption for the dependent variable
#' @param predictors    caption for the predictors
#' @param ix            indices for slicing results (for longer tables)
summarize_bia2 = function(bia_result, dependent, predictors, ix) {
	if(missing(ix)) ix = 1:length(bia_result$ests)
	with(bia_result,{
	   pvals = 2 * stats::pt(abs(ests[ix]/var[ix]^0.5), df = df[ix], lower.tail = FALSE)
	   outdf = data.frame(cbind(ests[ix], var[ix], ci[ix,1], ci[ix,2]), pvals )
	   colnames(outdf) = c("Estimate", "Std. error", "95% CI lower", "95% CI upper", "p")
	   rownames(outdf) = predictors
	   outdf
	   })
}

summarize_bia3 = function(bia_result, dependent, predictors) {
	with(bia_result,{
	   pvals = 2 * stats::pt(abs(ests/var^0.5), df = df, lower.tail = FALSE)
	   outdf = data.frame(cbind(ests, var, ci[,1], ci[,2]), pvals )
	   colnames(outdf) = c("Estimate", "Std. error", "95% CI lower", "95% CI upper", "p")
	   rownames(outdf) = predictors
	   outdf
	   })
}

#' See a participants response to an item
#'
#' This now only works for the `knowl` dataset
pcp_response  = function(pcp, item, data, items=items){
	this = data[pcp, item]
	this
}

#' See a question cue with its response alternatives
#'
seequest = function(it){
	cat(paste(s[s$item==it,], collapse="\n  "), '\n')
	barplot(table(factor(sapply(1:nrow(d), pcp_response, item=it))))
}

#' Alternative Response Distribution
#'
#' See a quick plot of the distribution of responses
seeresp = function(i){
	barplot(table(factor(sapply(1:nrow(d), pcp_response, item=i))))
}


#' Helps reading through multiple choice items
#'
inspect_item <- function(code, short=F, all=F){

	item = substr(code, start=1, stop=4)
	resp = sprintf('resp%s', substr(code, 5, 5))
	out_vector = resp 
		if (!short){
		out_vector = c('question', resp)
		}
	if (all){
		out_vector <- c('question', sprintf('resp%s',1:4))
	}
        knowlItems[knowlItems$item==item,out_vector]
}

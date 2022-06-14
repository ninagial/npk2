#' Create a dummy variable model frame 
#'
#' @param data: a data frame of factors with numerical levels
dummify <- function(data, bind=NULL){
	out <- as.data.frame(model.matrix(~.-1, factrz(data)))
	if (!is.null(bind)){
		if (!grepl('data.frame', class(bind))){
			bind = as.data.frame(bind)
		}
		out <- cbind(bind, out)
	}
	out
}

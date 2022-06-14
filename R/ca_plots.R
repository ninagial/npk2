ca_plot <- function(ca_obj){
	rcoord = matrix(ca_obj$row$coord,ncol=5)
	ccoord = matrix(ca_obj$col$coord,ncol=5)
	ro = data.frame(rcoord[,1:2])
	co = data.frame(ccoord[,1:2])
	colnames(ro) <- c('x','y')
	colnames(co) <- c('x','y')
	ggplot(ro, aes(x,y)) + 
		geom_text(label=rownames(ro)) +
		scale_x_continuous(limits=c(-1,1)) + 
		scale_y_continuous(limits=c(-1,1)) + 
	        geom_hline(yintercept=0) + geom_vline(xintercept=0) +
		geom_text(data=co, aes(x,y,label=rownames(co))) +
                theme(text=element_text(size=14), 
	              panel.background=element_rect(fill='white'))
}

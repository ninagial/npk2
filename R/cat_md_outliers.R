cat_md_outliers <- function(obj=NULL, cut=1.5){
	require(FactoMineR)
	if (is.null(obj)){
		obj = factrz(gsm[gsmItems$item])
	}
	     mca_df = MCA(graph=F, obj)$ind$coord

	     indices = which(apply(abs(mca_df[,1:2]),1, function(x) all(x < cut)))
# The following data points are excluded, using the criterion explained above:
	     which(!(1:nrow(obj) %in% indices))
}




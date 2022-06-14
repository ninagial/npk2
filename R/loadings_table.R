#' Sort items according to loading to specific Factor
#'
sort_best <- function(lmb, j, xnames, itemdf, cutoff=.4){
		x = lmb[,j]
		names(x) <- xnames
		loadings_sorted <- sort(x,decreasing=T)
		include = abs(loadings_sorted) >= cutoff
		labs = names(loadings_sorted[include])
		labs = sapply(labs, function(lab) itemdf[itemdf$item==lab, 'label'])
		data.frame(Factor=j, Label=labs, Loading=loadings_sorted[include])
	}

assign_items <- function(obj=gpcm_fit, itemdf=gidItems, cutoff=0.4){
	require(psych)
	require(lavaan)
	require(mirt)

	rownames(itemdf) <- itemdf$item

	if ('SingleGroupClass' %in% class(obj)){
		ld = obj@Fit$F
	}

	if (any(grepl('psych' ,class(obj)))){
		ld = data.frame(unclass(obj$loadings))
	}

	if (any(grepl('lavaan', class(obj)))){
		ld = inspect(obj, what='std')$lambda
	}

	assigned_to <- structure(names=rownames(ld), 
		  sapply(rownames(ld),  function(it){
		      out =  which.max(abs(ld[it,]))
		      names(out)<-NULL
		      	out}))
	out1 = lapply(split(names(assigned_to), assigned_to), 
	       function(x){
		      out = ld[x,]
	              if(is.null(dim(out))){
		          out = matrix(out, ncol=ncol(ld))
		      rownames(out) <- x
		      colnames(out) <- colnames(ld)
		      }
		          out= data.frame(out)
	              out
	       })
	out2 = lapply(1:length(out1), function(i){
		       out = out1[[i]][order(decreasing=T, abs(out1[[i]][,i])),]
		       out = data.frame(out)
		       out = cbind(Item=itemdf[rownames(out), "label"], out)
		       out
	       })
	list(loadings = ld, assign = assigned_to, tables=out2)
}


#' Format a Factor or Components Loadings Table
#' 
        loadings_table <- function(x, items=NULL, cols=NULL, bolden=F, tex=F, drop=T, cutoff=.35){
	ld = data.frame(unclass(x$loadings))
	ld = ld[sort(colnames(ld))]
	if (!is.null(items)){
	       	rownames(ld) <- items
		}
	ld = ld[do.call(order,-abs(ld)),]
	if (bolden){
		ld_ch = lapply(ld, function(loadings_col){
				       out = sapply(loadings_col, bolden_val, cut=cutoff)
				       out
					})
		out_alignment = c("p{9cm}", rep('r', ncol(ld)))
		out = data.frame(ld_ch)
	} else {
		out = ld 
	}
		rownames(out) <- rownames(ld)
		if (!is.null(cols)){
			colnames(out) <- cols} else {
				colnames(out) <- toupper(latin_nums[1:ncol(ld)])
		}
		if (tex){
			out = xtable(out, align=out_alignment )
		}
		out
		}

loadings_table2 <- function(obj=NULL, item_labels=NULL, cut=0.4){
	require(psych)

	if (is.null(obj)){
		obj = principal(nfact=3, numrz2(gsm[gsmItems$item]))
	}

	if (is.null(item_labels)){
		item_labels = gsmItems$label
	}

	if ('SingleGroupClass' %in% class(obj)){
		ld = obj@Fit$F
	}

	if ('psych' %in% class(obj)){
		ld = data.frame(unclass(obj$loadings))
	}

	if ('lavaan' %in% class(obj)){
		ld = inspect(obj, what='std')$lambda
	}
	Reduce(rbind, structure(lapply(1:ncol(ld), sort_best,lmb=ld, xnames=rownames(ld)), names=sprintf('Factor %d', 1:ncol(ld))))
}

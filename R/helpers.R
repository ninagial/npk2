numrz <- function(d){
	out=data.frame(t(apply(as.matrix(d),1,as.integer)))
	colnames(out) <- colnames(d)
	out
}

numrz2 <- function(d){
	out = lapply(d, as.numeric)
	out = data.frame(out)
	colnames(out) <- colnames(d)
	out
}

ordrz <- function(d){
	out=data.frame(lapply(d, ordered))
	colnames(out) <- colnames(d)
	out
}

factrz <- function(d){
	out=data.frame(lapply(d, factor))
	colnames(out) <- colnames(d)
	out
}

fitz = function(modeli){
	round(fitmeasures(modeli)[c('gfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'aic', 'bic')],2)
}


compare_fit <- function(fitlist, select = c('rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'tli','cfi')){
	out1 = lapply(fitlist, fitmeasures)
	out1 = Reduce(cbind, out1)
        out1 = round(digits=3, out1)
	out1 = data.frame(out1)[select,]
	out1
}

mgroup <- function(data = gsm, group='gender', itemdf=gsmItems, select = c('lng3','lng4','tra1', 'tra2'), group_levels = c('Male', 'Female'), item_type='gpcm'){
	require(mirt)

	analysis_data = numrz2(data[select])
	grouping_factor = gsm[,group]
	ixl_include = grouping_factor %in% group_levels
	analysis_data = analysis_data[ixl_include,]
	grouping_factor = grouping_factor[ixl_include]

	list(
	      multipleGroup(analysis_data, 1, group=grouping_factor, itemtype=item_type, method='MHRM'),
	      multipleGroup(analysis_data, 2, group=grouping_factor, itemtype=item_type, method='MHRM'),
	      multipleGroup(analysis_data, 1, group=grouping_factor, itemtype=item_type, method='MHRM', invariance=c('slopes')),
	      multipleGroup(analysis_data, 1, group=grouping_factor, itemtype=item_type, method = 'MHRM', invariance=c('slopes', 'intercepts'))
	      )

}

mgroup_gsm <- function(){
	out = list()
	out$gender <- mgroup(group='gender', group_levels=c('Male', 'Female'))
        out$country <- mgroup(group='native',group_levels=c('United Kingdom','Poland','Greece'))
	out$gender_bic <- sapply(out$gender, function(x) extract.mirt(x, 'BIC'))
	out$country_bic <- sapply(out$country, function(x) extract.mirt(x, 'BIC'))
	out$gender_best <- which.min(out$gender_bic)
	out$country_best <- which.min(out$country_bic)
	out$gender_lm <- lm(fscores(out$gender[[out$gender_best]])~out$gender[[out$gender_best]]@Data$group)
	out$country_lm <- lm(fscores(out$country[[out$country_best]])~out$country[[out$country_best]]@Data$group)
	out
}

mgroup_summary <- function(mg_obj){

	bic <- sapply(mg_obj, function(x) extract.mirt(x, 'BIC'))
	best <- which.min(bic)
	lm_data <- data.frame(outcome=fscores(mg_obj[[best]])[,1], grouping= mg_obj[[best]]@Data$group)
	fit <- lm(outcome~grouping,data=lm_data)
	list(bic, best, lm_data, fit)
}

mgroup_regress <- function(){
	out = new.env()
	with(out, {
	cnames <- c('lng3','lng4','tra1','tra2', 'txc1','txc2','csp1','csp2','sci1','sci2','lgb1','cis1','tph1','tph2')
	prednames <- c('gender2','native2')
	analysis_data = numrz2(gsm[cnames])
	model_orthogonal = 'f1=1-4\nf2=5-6,11\nf3=7-10'
	model_oblique = 'f1=1-4\nf2=5-6,11\nf3=7-10\nCOV=f1*f2*f3'
	model_oblique4 = 'f1=1-4\nf2=5-6,11\nf3=7,8\nf4=9,10\nf5=11,12,13\nCOV=f1*f2*f3*f4*f5'
	predictors <- gsm[prednames]
	analysis_data=na.omit(cbind(analysis_data, predictors))
	fit_orth<-mirt(analysis_data[cnames],mirt.model(model_orthogonal), itemtype='gpcm', method='MHRM')
	fit_obl<-mirt(analysis_data[cnames],mirt.model(model_oblique), itemtype='gpcm', method='MHRM')
	fit_obl4<-mirt(analysis_data[cnames],mirt.model(model_oblique4), itemtype='gpcm', method='MHRM')
	mg_obl4 <- multipleGroup(analysis_data[cnames], model_oblique4, group=analysis_data$gender2, method='MHRM')
	# mg_obl4.2 <- multipleGroup(analysis_data[cnames], model_oblique4, group=analysis_data$native2, method='MHRM')
	# outliers_orth <- gCD(analysis_data[cnames], mirt.model(model_orthogonal), method='MHRM')
	# outliers_obl <- gCD(analysis_data[cnames], mirt.model(model_oblique), method='MHRM')
	# list(fit_orth, fit_obl, analysis_data, outliers_orth, outliers_obl)
	      })
	out
}

regrsum <- function(fit, analysis_data){
	pv = mirt::fscores(fit, plausible.draws=10)
	pvmods <- lapply(pv, function(x, covdata) {
				 x=data.frame(x)
				 colnames(x) <- c('f1','f2','f3')
				 outcome = x$f1
				 predata = cbind(covdata, f2=x$f2, f3=x$f3)
				 predictors = model.matrix(~gender2*native2+f2+f3, data=predata)
				 
				 lm(outcome~predictors)
	      }, covdata=analysis_data[c('gender2','native2')])
	so <- lapply(pvmods, summary)
so
	par <- lapply(so, function(x) x$coefficients[, 'Estimate'])
	SEpar <- lapply(so, function(x) x$coefficients[, 'Std. Error'])
	mi= averageMI(par, SEpar)
	list(fit=fit,pv=pv, pvmods=pvmods, so=so, par=par, SEpar=SEpar, mi=mi)
	# list(fit, analysis_data)
}

#' For the Oblique 4 Solution
#' 
regrsum2 <- function(fit, analysis_data){
	pv = mirt::fscores(fit, plausible.draws=10, QMC=T)
	pvmods <- lapply(pv, function(x, covdata) {
				 x=data.frame(x)
				 colnames(x) <- c('f1','f2','f3', 'f4', 'f5')
				 outcome = x$f1
				 predata = cbind(covdata, f2=x$f2, f3=x$f3, f4=x$f4, f5=x$f5)
				 predictors = model.matrix(~gender2*native2+f2+f3+f4+f5, data=predata)
				 
				 lm(outcome~predictors)
	      }, covdata=analysis_data[c('gender2','native2')])
	so <- lapply(pvmods, summary)
so
	par <- lapply(so, function(x) x$coefficients[, 'Estimate'])
	SEpar <- lapply(so, function(x) x$coefficients[, 'Std. Error'])
	mi= averageMI(par, SEpar)
	list(fit=fit,pv=pv, pvmods=pvmods, so=so, par=par, SEpar=SEpar, mi=mi)
	# list(fit, analysis_data)
}

#' For the TXC etc Solution
#' 
regrsum3 <- function(fit, analysis_data, outcome='tra'){
	pv = mirt::fscores(fit, plausible.draws=50, QMC=T)
	pvmods <- lapply(pv, function(x, covdata) {
				 x=data.frame(x)
				 colnames(x) <- c('TRA','COM','CSP','SCI','TXC')
				 # outcome = x$f1
				 predata = cbind(covdata, x)
				 # predictors = model.matrix(~gender2*native2+f2+f3+f4+f5, data=predata)
				 
				 switch(outcome, 
					tra = lm(TRA~.-COM, data=predata), 
					com = lm(COM~.-TRA, data=predata))
	      }, covdata=analysis_data[c('gender2','native2')])
	so <- lapply(pvmods, summary)
so
	par <- lapply(so, function(x) x$coefficients[, 'Estimate'])
	SEpar <- lapply(so, function(x) x$coefficients[, 'Std. Error'])
	mi= averageMI(par, SEpar)
	list(fit=fit,pv=pv, pvmods=pvmods, so=so, par=par, SEpar=SEpar, mi=mi)
	# list(fit, analysis_data)
}
bazz <- function(summ){
	summ <- summ$mi
	rownames(summ) <- gsub('predictors|[ aeiouy]','', rownames(summ))
	summ[order(summ[,'p'], decreasing=T),]
}

mg_example22 <- function(){
	out <- new.env()

	with(out, {
	cur <- mgroup_regress()
	factor_summary <- summary(cur$fit_obl4)

	regression_summary <- with(cur, bazz(regrsum3(fit_obl4, analysis_data)))
	      })
	out
}


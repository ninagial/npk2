
#' @rdname extract
#' @export
#'
#' @title
#' Extract a named element from all elements of a nested list object.
#'
#' @description
#' When you have a nested list object (2 levels) and you want to extract the same named element from each list, you can use \code{extract}.
#'
#' @details
#' This may come in handy with the result of other functions. For example when we perform
#' model fitting on a number of scales, and we use various methods on the same subset of variables.
#' \code{extract} allows obtaining the desired statistics from each of the fits, for each scale. See \code{func2matx} for exploiting this function to produce matrices and tables.
#'
#' @author
#' Nikolaos Giallousis \email{ngiallousis@psyget.gr}
#' @seealso \code{\link{func2matx}}
#'
#' @param llistObj A nested list object with two levels.
#' @param element The name of an element to extract from each sub-list (Unquoted).
#' @return A list of the extracted elements.
#'
#' @examples
#' nested_list = list(list(a=1, b=2), list(a=3,b=4))
#' extract(nested_list, a)
#' nested_list = list(list(a=c('a','b','c'), b=1:3), list(a=c('d','e','f'),b=4:6))
#'
#' # the function uses this escape hatch for non-standard evaluation
#' extract_q(nested_list, 'a')
#' extract(nested_list, a)
#' extract(nested_list, 'a')
#'
#' # functionality within other functions
#' # ... non-standard evaluation
#' foo = function(llistObj) extract(llistObj, a)
#'
#' # ... standard evaluation
#' foo_q = function(llistObj) extract(llistObj, 'a')
#'
#' # result
#' foo(nested_list)
#' foo_q(nested_list)
#'
#' # combine with rbind (see func2matx documentation)
#' func2matx(rbind, nested_list, extract, element=a)
extract = function(llistObj, element){
  extract_q(llistObj, as.character(substitute(element)))
}

#' @rdname extract_q
#' @export
#'
#' @title
#' Extract a named element from all elements of a nested list object (Standard evaluation escape hatch) .
#'
#' @description
#' Standard evaluation escape hatch for \link{extract}.
#'
extract_q = function(llistObj, element){
  eval(lapply(llistObj, getElement, element), parent.frame())
}

#' @rdname func2matx
#' @export
#'
#' @title
#' Call a function on a nested list objects and bind the result into a matrix, row- or column-wise.
#'
#' @author
#' Nikolaos Giallousis \email{ngiallousis@psyget.gr}
#'
#' @details
#' \code{func2matx} calls rather than applies the function on the nested list. If you want to use the apply family you can use, for instance, \code{\link{lapply}} a closure right away in the \code{func} argument.
#' @param  bind The binding function, i.e. \code{\link{cbind}} or \code{\link{rbind}}.
#' @param llistObj A nested list object with two levels.
#' @param func The function to \emph{call} (not \link{apply}) on llistObj.
#' @param ... Other arguments to be passed to \code{func}.
#' @return A matrix of the return value of func called on llistObj.
#'
#' @examples
#' nested_list = list(list(a=c('a','b','c'), b=1:3), list(a=c('d','e','f'),b=4:6))
#' func2matx(rbind, nested_list, extract, element=a)
#'
#' func2matx(rbind, nested_list, paste, sep='-', collaspe='\n')
func2matx <- function(bind, llistObj, func, ...){
	eval(substitute(Reduce(bind, func(llistObj, ...))))
}

#' @rdname decorate_corr
#' @export
#'
#' @title
#' Add stars to correlation matrix
#'
#' @param values  matrix the r matrix from rcorr
#' @param signi  matrix the P matrix from rcorr
#'
#' @return a character matrix
decorate_corr = function(values, signi){
	vals = apply(values, 2, format, digits=2)
	vals[signi] <- sprintf("%s*", vals[signi], digits=2)
	vals
	}


#' @rdname fitmeasure_select
#' @export
#'
#' @title
#' Select Fit Measures from lavaan output
#'
#' @param fitObj an object fitted with lavaan
#' @param selection character the fit indices to select
#'
#' @return a numeric matrix
fitmeasure_select <- function(fitObj, selection = c('chisq', 'df', 'pvalue', 'gfi', 'agfi', 'rmsea', 'tli','cfi','srmr') ){
	fi <- fitmeasures(fitObj)
	fi_select <- fi[selection]
	fi_select$chisq_over_df <- fi_select['chisq']/fi_select['df']
	fi_select
}

#' @rdname modif
#' @export
#'
#' @title
#'  Return most important modification indices from lavaan object
#'
#' @param fitObj an object fitted with lavaan
#' @param many integer how many indices to use
modif <- function(fitObj, many=10){
	mi <- modificationindices(fitObj)
	head(mi[order(-mi$mi),], many)
}

#' @rdname form_prop_table
#' @export
#'
#' @title
#' Format a Proportion's Table
#'
#' @param cat1 factor A categorical variable
#' @param cat2 factor A categorical variable
#' @param output character Desired output, either \code{pander}, \code{xtable}, or \code{default}
#' @return A formated table
form_prop_table <- function(cat1, cat2){
	tb <- addmargins(table(cat1, cat2))
	ptb <- addmargins(prop.table(table(cat1,cat2))*100)
	dimen <- dim(tb)
	mx <- matrix(NA, dimen[1], dimen[2])
	for (i in 1:dimen[1]){
		for (j in 1:dimen[2]){
			newVal = sprintf('%d (%.2f%%)', tb[i,j], ptb[i,j])
			mx[i,j] <- newVal
		}
	}
	rownames(mx) = rownames(tb)
	colnames(mx) = colnames(tb)
	mx
}


form_prop_table2 <- function(x){
	tb <- addmargins(table(x))
	ptb <- addmargins(prop.table(table(x))*100)
	len = length(tb)
	vc <- rep(NA, len)
	for (i in 1:len){
		newVal = sprintf('%d (%.2f%%)', tb[i], ptb[i])
			vc[i] <- newVal

	}
	names(vc) = names(tb)
	vc
}

#' @rdname llistify
#' @export
#'
#' @title
#' Turn a flat list to a llistObj
#'
#' @param global list a factor_facet list
#' @param flat list a list, most plausibly a list of analysis by facet
#'
#'
#' @details
#' Useful when there are global dimensions, i.e. hierarchy in scales
llistify = function(global, flat){
	lapply(global, function(global_i){
			   flat[global_i]
})}

#' @rdname myExclude
#' @export
#'
#' @title
#' Global Item Exclusion
#'
#' @details
#' Nested list objects, as usual, structured by Subscale
#' Uses \code{\link{Map}} to exclude
#' TODO deal with numeric vs. colnames item definition
#' .. if this is an option, it should read the colnames properly
#' TODO deal with flat list vs nested list (facets vs global>facets)
#' TODO check lengths so that not too many items are deleted
#' QUEST is there order in the POC lists?
myExclude = function(item_list, exclude_list, num=F, col_str='Q') {
	if (!num) item_list = sprintf('%s%s',col_str, item_list)
	# if there is nothing to exclude return original
	if (is.null(exclude_list)|!length(exclude_list)) return(item_list)
	item_list[-which(item_list %in% exclude_list)] # x2[..] propends Q x[..] returns num
}

#' @rdname mod2mod
#' @export
#'
#' @title
#' Construct expressions from modification indices
#'
mod2mod = function(fitted_model, n=10, global_str = 'GD_'){
	modifications = modif(fitted_model, many=n)
	ix = grepl(global_str, modifications$lhs)
	with(modifications[ix,],
		 paste(sprintf('%s%s%s', lhs, op, rhs), collapse=';'))
}

#' @rdname fit_bulk_models
#' @export
#'
#' @title
#' Iteratively Fit and Modify CFA models
#'
#' @examples
#'
#' bulk_fit0 = fit_bulk_models(d, 20, model0)
#' Reduce(rbind, bulk_fit0$fit_stats)
fit_bulk_models <- function(dataObj, n=12, start_model){
  require(lavaan)
	fit_stats = list()
	model_objects = list()
	current_model = start_model
	models_log = list(start_model)
	for (i in 1:n){
		print(i)
		current_fit = cfa(current_model, data=dataObj)
		current_model = sprintf('%s;%s', current_model, mod2mod(current_fit))
		fit_stats[[length(fit_stats)+1]] <- fitmeasure_select(current_fit)
		models_log[[length(models_log)+1]] <- current_model
		model_objects[[length(model_objects)+1]] <- current_fit
	}
	list(fit_stats=fit_stats, models_log=models_log, model_objects=model_objects)
}


#' @rdname analyze_items
#' @export
#'
#' @title
#' Quick CTT and Rasch Item Analysis
#'
#' @examples
#' data(ddf)
#' item_analysis0 = lapply(facet_items, analyze_items, d=ddf)
#' ctt0 = extract(item_analysis0, ctt)
#' alphas0 = func2matx(rbind, ctt0, extract, total)
#' item_stats0 = extract(ctt0, item.stats)
#'
#' rasch0 = extract(item_analysis0, rasch)
#' ifit0 = extract(item_analysis0, ifit)
#' pfit0 = extract(item_analysis0, pfit)
analyze_items <- function(faceti, d) {
            require(eRm)
            require(Hmisc)
					  cat(sprintf("Facet %s is processed\n", toupper(faceti)))
					   ddf = d[, faceti]
				   cat("Data frame OK \n")
				   ctt = try( psych::alpha(ddf, check.keys=T))
				   cat(ifelse(class(ctt)!='try-error', "Alpha OK \n", "Alpha failed \n"))
				   rasch = try(eRm::PCM(ddf, se=T))
				   cat(ifelse(class(rasch)!='try-error', "Rasch OK \n", "Rasch failed \n"))
				   ppar <- try(person.parameter(rasch))
				   ifit = try(itemfit(ppar))

				   cat(ifelse(class(ifit)!='try-error', "Ifit OK \n", "Ifit failed \n"))
				   pfit = try(personfit(ppar))
				   cat(ifelse(class(pfit)!='try-error', "Pfit OK \n", "Pfit failed \n"))
				   llist(ctt, rasch, ifit, pfit)
				   }


#' @rdname model_string
#' @export
#'
#' @title
#' Construct model strings from test ontology
#'
model_string <- function(facet_items, factor_facets, covar = T, regr = character(0)){
	#variable definition
	m1 = paste(lapply(names(facet_items), function(x){
			  sprintf('%s=~%s;\n', x, paste(facet_items[[x]], collapse='+'))
			  }), collapse='')

	# second order variable definition
	m2 = paste(lapply(names(factor_facets), function(x){
			   sprintf('%s=~%s;\n', x, paste(factor_facets[[x]], collapse='+'))		  }), collapse='')

	# covariances
	covs = if(covar) {
		paste(apply(combn(names(factor_facets), 2), 2, function(x) sprintf('%s~~%s;\n', x[1], x[2])), collapse='')} else NULL

	# regressions
	regs = if (length(regr)){ regr } else NULL
	meas_mod = paste(m1, m2, covs, collapse='')
	struct_mode = paste(m1, m2, covs, regs, collapse='')
	list(m1=m1, m2=m2, covs=covs, regs=regs, meas_mod=meas_mod, struct_mod=struct_mode)
}

# Perform Regression Diagnostics {{{

#' @rdname reg_diagnosis
#' @export
#'
#' @title
#' Perform Regression Diagnostics
#'
reg_diagnosis <- function(lmodel){

	## Full Diagnostics
	lmodel_diag <- data.frame(
		res = resid(lmodel),
		sres = rstandard(lmodel),
		stres = rstudent(lmodel),
		cook = cooks.distance(lmodel),
		dfb = dfbeta(lmodel),
		dff = dffits(lmodel),
		lev = hatvalues(lmodel),
		covra = covratio(lmodel))
	str(lmodel_diag)

	## Criteria

	### Residuals

	crit_res <- which( abs(lmodel_diag$sres) > 2 )

	### Cooks Distance

	crit_cook <-which( abs(lmodel_diag$cook) > 1 )

	### Leverage
	lmodel_kn <- dim(lmodel$model)
	lmodel_average_leverage <- lmodel_kn[2]/lmodel_kn[1]
	crit_lev <- which( lmodel_diag$lev > 2*lmodel_average_leverage )

	### Covariance Ratio

	crit_covra1 <- which( lmodel_diag$covra > 1 + 3*lmodel_average_leverage)
	crit_covra2 <- which( lmodel_diag$covra < 1 - 3*lmodel_average_leverage)

	## Participants in Cynicism Model

	lmodel_incl <- rownames(lmodel$model)

	crit_part <- lapply(list(crit_res, crit_cook, crit_lev, crit_covra1, crit_covra2), function(x) lmodel_incl %in% lmodel_incl[x])

	crit_part_df <- structure(data.frame(Reduce(cbind, crit_part)), row.names=lmodel_incl)

	participant_criteria_scores <- apply(crit_part_df, 1, sum)

	lmodel_excl <- lmodel_incl[which(participant_criteria_scores > 0)]

	## New Model
    ix <- !(rownames(lmodel$model) %in% lmodel_excl)
	form <- formula(lmodel)
	ddf <- lmodel$model[ix,]
	lmodel00 <- lm(form, data=ddf)

	# Value
    out1 <- list(original = lmodel, updated = lmodel00, criteria = list(residuals = crit_res, cook_dist = crit_cook, leverage = crit_lev),
		 cov_ratio = list(plus = crit_covra1, minus = crit_covra2), excluded =
			 lmodel_excl, average_leverage = lmodel_average_leverage, participant_scores =
			 participant_criteria_scores, score_data = crit_part_df, formula =
			 formula(lmodel), data=lmodel$model, xdata=ddf,diagnostics = lmodel_diag)
	return(out1)
}

#' @rdname reg_diagnosis_summary
#' @export
#'
#' @title
#' Summarize Regression Diagnostics
#'
reg_diagnosis_summary <- function(reg_d, title='Model'){
	cat('Excluded ', length(reg_d$excluded), ' data points\n')
	cat('\nHere are the differences in regression coefficients: \n\n')
	print(coef(reg_d$original) - coef(reg_d$updated))
	coef_sig <- list(original = names(which(summary(reg_d$original)$coefficients[,4]<0.1)),
				   updated=names(which(summary(reg_d$updated)$coefficients[,4]<0.1)))
	cat('\nPredictors becoming NON-Significant after refitting the model: \n\n')
	print(setdiff(coef_sig[[1]], coef_sig[[2]]))
	cat('\nPredictors becoming Significant after refitting the model: \n\n')
	print(setdiff(coef_sig[[2]], coef_sig[[1]]))
	par(mfrow=c(2,4))
	plot(reg_d$original, main = paste0(title, ' Original'))
	plot(reg_d$updated, main = paste0(title, ' Updated'))

}
#' cy_diag <- reg_diagnosis(cy_lm)
#' ex_diag <- reg_diagnosis(ex_lm)
#' pe_diag <- reg_diagnosis(pe_lm)
#'
#' par(mfrow=c(2,4))
#' reg_diagnosis_summary(cy_diag)
#' reg_diagnosis_summary(ex_diag)
#' reg_diagnosis_summary(pe_diag)
#'
#' summary(cy_diag$original)
#' summary(cy_diag$updated)
#' summary(ex_diag$original)
#' summary(ex_diag$updated)
#' summary(pe_diag$original)
#' summary(pe_diag$updated)
#'
#' cy_lm2 <- update(cy_diag$updated, . ~ . -practice -location -nyear -nempl,
#'                  data=cy_diag$xdata)
#' ex_lm2 <- update(ex_diag$updated, . ~ . -age -location -nempl,
#'                  data=ex_diag$xdata)
#' pe_lm2 <- update(pe_diag$updated, . ~ . -age -practice -location,
#'                  data=pe_diag$xdata)
#'
#' cy_fit16 <- fitted(cy_lm2)
#' ex_fit16 <- fitted(ex_lm2)
#' pe_fit16 <- fitted(pe_lm2)
#'
#'

#' @rdname pool_mlm
#' @export
#'
#' @title
#' (Tweak) Combine Multiple Imputation Results
#'
pool_mlm <- function (results, variances, call = sys.call(), df.complete = Inf,
    ...)
{
	library(Amelia)
	library(mi)
	library(mitools)
	library(magrittr)
	library(norm)
	library(mice)
	library(smcfcs)
	library(Zelig)

    m <- length(results)
    oldcall <- attr(results, "call")
	# if (missing(variances)) {
	#     variances <- suppressWarnings(lapply(results, vcov))
	#     results <- lapply(results, coef)
	# }
	objects <- lapply(results, function(obj){

					coe <- coef(obj)
					vco <- vcov(obj)
					results <- structure(c(coe[,1], coe[,2], coe[,3]), names=rownames(vco))

					list(results = results, variances=vco)})

    vbar <- objects[[1]]$variances
    cbar <- objects[[1]]$results
    for (i in 2:m) {
        cbar <- cbar + objects[[i]]$results
        vbar <- vbar + objects[[i]]$variances
    }
    cbar <- cbar/m
    vbar <- vbar/m
    evar <- var(do.call("rbind", lapply(objects, function(x) x$results)))
    r <- (1 + 1/m) * evar/vbar
    df <- (m - 1) * (1 + 1/r)^2
    if (is.matrix(df))
        df <- diag(df)
    if (is.finite(df.complete)) {
        dfobs <- ((df.complete + 1)/(df.complete + 3)) * df.complete *
            vbar/(vbar + evar)
        if (is.matrix(dfobs))
            dfobs <- diag(dfobs)
        df <- 1/(1/dfobs + 1/df)
    }
    if (is.matrix(r))
        r <- diag(r)
    rval <- list(coefficients = cbar, variance = vbar + evar *
        (m + 1)/m, call = c(oldcall, call), nimp = m, df = df,
        missinfo = (r + 2/(df + 3))/(r + 1))
    class(rval) <- "MIresult"
    rval
}
#}}}

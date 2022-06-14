# Several functions used in preliminary treatment of datasets
# First, univariate outliers are identified and the specific values are removed (ie converted to missing)
# Multivariate outliers are removed altogether.
# Box Cox Normalization transformations are employed
# Scales that undergo a change in directionality due to the transformation
# as indicated by the sign of the exponent
# are reversed back to their original direction
# Missing values are then imputed with Predictive Mean Matching
# And submitted to the analysis function
# This whole chain of operations typically results in confidence intervals for the requested parameters
# These, in turn, must be the return value of the "analysis function", ie the function run on each one of the imputed datasets.

#' Indices of Multivariate Outliers
#'
#' @return integer
mv_outliers <- function(data, alpha=0.001){
		     # multivariate (back on the original dataset
		     # md <- mahalanobis(data, center = colMeans(data), cov = cov(data))
		     md = psych::outlier(data)
		     cutoff <- (qchisq(p = 1 - alpha, df = ncol(data)))
		     which(md > cutoff)
	     }

#' Convert Outlying Values to Missing
#'
#' @details Quantitative variables only
#'
#' @return data.frame
outliers_unv <- function(data, indices=F){
		     # use only quantitative variables
	             # to convert outliers to NA
	if (!indices){
                     inject_na = function(x) {
			     bp = boxplot(x, plot=F)$stats
			     ixl = which(x > max(bp) | x < min(bp))
			     x[ixl] <- NA
			     x
		     }	
		     from_boxplot = lapply(data, inject_na)
		     return(data.frame(from_boxplot))
	} else {
                       outlier_ixs = function(x) {
			     bp = boxplot(x, plot=F)$stats
			     ixl = which(x > max(bp) | x < min(bp))
			     return(ixl)

		     }	
	return(lapply(data, outlier_ixs))
	}
	     }


#' Treat Univariate and Multivariate Outliers
#'
#' @details 
outliers <- function(data){
	out = outliers_unv(data)
	excl = mv_outliers(data)
	out[-excl,]
}

#' Apply Box-Cox Power Transformation and Clean Up
#'
#' @param scalp logical Should output data be scaled?
#' @param exps logical Should a list with both the transformed data and the exponents be returned instead
#' @return data.frame The transformed data
normalize <- function(data, scalp=T, exps=F){
	bc_pows = sapply(data, function(x) powerTransform(x)$lambda)
	neg_ix = which(bc_pows < 0)
	out = data.frame(Map(function(x,r) x^r, data, bc_pows))
	if (scalp) {
	    out = scale(out)
	} 
	out[, neg_ix] <- -out[,neg_ix]
	if (exps) {
		out = list(data=out, exponents=bc_pows)
	}
	out
}

impute = function(data, func, nb=2, ni=10, nc=4){
	bootmi_imps = bootImpute::bootMice(data, nBoot=nb, nImp=ni, print=F, seed=1983674653, nCores=nc)
 	boot_results = bootImpute::bootImputeAnalyse(bootmi_imps, func)
}


#' Robust Stepwise Regression on MICE Imputed Datasets
#'
#' @param ni integer Number of imputations
#' @details Stepwise has posed some issues with the approach above. This is an alternative solution for doing MI/Bootstrapped stepwise regression.
impute_step <- function(models, data, ni=10){
	
	foo_imps = mice::mice(data=data, m=ni, maxit=20, meth='pmm', seed=1983674653, print=F)
	foo_imps2 = lapply(1:ni, function(i) complete(foo_imps, i))
        step_fits = lapply(models, function(modi) {
				   lapply(foo_imps2, function(dt){
						  model = step(lm(modi, dt))
		      model})
})
	step_fits
	}


#' Get a List of Imputed Datasets
#
#' @param ni    integer; Number of imputations
impute_data <- function(data, ni=20){
	imps = mice::mice(data=data, m=ni, maxit=20, meth='pmm', seed=1983674653, print=F)
	lapply(1:ni, function(i) mice::complete(imps, i))
}


#' Mclust on Imputed Dataset (TEMP)
#'
#' @example impute_mclust(vars = c('panss1g.total', 'panss1p.total', 'panss1n.total', 'lifetime_freq', 'wmi', 'vci', 'psi', 'pri'), outliers_unv(premorbid))
impute_mclust <- function(vars, data, ncl=NULL, shp=NULL, ni=20){
	imps = impute_data(data, ni)
	lapply(1:ni, function(di) mclust::Mclust(imps[[di]],ncl,shp))
}

#' Number of Classes (Majority Rule)
#'
#' @param mcl    list; The output of \link{\code{impute_mclust}}
mclust_votes <- function(mcl){
	md_name = sapply(mcl, function(x) x$modelName)
	mdk = sapply(mcl, function(x) length(table(x$classification)))
	table(sprintf("(%s, %d)", md_name, mdk))
}





#' Apply Majority Rule for Stepwise Regression
#'
#' @param ml list The result of step_impute
#'
maj_rule = function(ml){
	get_names <- function(ll){
		     unique(unlist(lapply(ll, names)))
	     }

	get_sum_inlists <- function(x, ll){
		     sum(sapply(ll, function(elem) x %in% names(elem)))
	     }

        get_votes <- function(ll){
		     the_names = get_names(ll)
		     sapply(the_names, get_sum_inlists, ll=ll)
	     }

	count_votes = function(modi){
		mi_steps = lapply(ml[[modi]], function(fiti) coef(fiti))
		get_votes(mi_steps)

	}
        votes = lapply(1:length(ml), count_votes)
	votes
}

#' Extract Fit Statistics from Stepwise
#'
#' @param ml list The output of step_fits
#' @seealso step_impute
step_fitst = function(ml){
	lapply(ml, function(modi){
		 sumodel = summary(modi)
		      c(sumodel$fstatistic, sumodel$r.squared, AIC(modi), BIC(modi))
		})
		foo_fits = Reduce(rbind, this)
		colnames(foo_fits) <- c('R-squared', 'AIC', 'BIC')
		foo_fits
}

#' Standardized Canonical Correlation Loadings
#'
canonical <- function(set1, set2){
		     require(Hmisc)
		     require(CCA)
		     require(CCP)
		     canonical = CCA::cc(set1, set2)
		     cc_loadings = comput(set1, set2, canonical)
		     s1 <- diag( sqrt(diag(cov(set1))))
		     s2 <- diag( sqrt(diag(cov(set2))))
		     cc_std1 <- s1 %*% canonical$xcoef
		     cc_std2 <- s2 %*% canonical$ycoef

		     # statistical inference (test)
		     rho = canonical$cor
		     n = dim(set1)[1]
		     p <- dim(set1)[2]
		     q <- dim(set2)[2]

		     canonical_pvals = list(
					    p.asym(rho,n,p,q,tstat='Wilks'),
					    p.asym(rho,n,p,q,tstat='Hotelling'),
					    p.asym(rho,n,p,q,tstat='Pillai'),
					    p.asym(rho,n,p,q,tstat='Roy')
					    )
		     out = llist(canonical, cc_loadings, cc_std1, cc_std2, canonical_pvals)
		     out
}

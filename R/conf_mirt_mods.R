conf_mirt_mods <- function(){
	mods <- list()
	mods
}

model_descriptions <- function(type=c('d','m','l')){
     descriptions = new.env()
     chalmers = new.env()
     beaujean = new.env()
     with(descriptions, {
		  tra_index = 'Well behaving index as a starting point'
		  cis2_tra = 'Does cis2 load onto TRA index?'
		  lng1_2 = 'Is there a separate interpersonal factor?'
		  lng1_2_obl = 'If there interpersonal factor is it orthogonal?'
    		  one_csp = 'Is there an Epistemology Factor, separate from CTs?'
         	  cis1_csp = 'Does cis1 load to the conspiracy factor?'
    		  lgb1br_txc = 'Does tranformed lgb1 load onto TXC factor?'
	      # add more for final model
		  })
     with(chalmers, {
		  tra_index = 'TRA=1-4'
		  cis2_tra = 'TRA=1-4,5'
		  lng1_2 = 'TRA=1-4,5\nCOM=6-7'
		  lng1_2_obl = 'TRA=1-4,5\nCOM=6-7\nCOV=TRA*COM'
    		  one_csp = 'TRA=1-4,5\nCOM=6-7\nCSP=8-11\nCOV=TRA*COM'
		  csp_sci = 'TRA=1-4,5\nCOM=6-7\nCSP=8,9\nSCI=10,11\nCOV=TRA*COM'
		  csp_sci_cov = 'TRA=1-4,5\nCOM=6-7\nCSP=8,9\nSCI=10,11\nCOV=TRA*COM*CSP*SCI'
         	  cis1_csp = 'TRA=1-4,5\nCOM=6-7\nCSP=8,9,12\nSCI=10,11\nCOV=TRA*COM*CSP*SCI'
		  txc_index = 'TRA=1-4,5\nCOM=6-7\nCSP=8,9,12\nSCI=10,11\nTXC=13,14\nCOV=TRA*COM*CSP*SCI*TXC'
		  })
     with(beaujean, {
		  cis2_tra = 'Does cis2 load onto TRA index?'
		  lng1_2 = 'Is there a separate interpersonal factor?'
    		  one_csp = 'Is there an Epistemology Factor, separate from CTs?'
		  csp_sci = 'Is there one CT and one SCI factor?'
		  csp_sci_cov = 'If there two factors are they correlated?'
         	  cis1_csp = 'Does cis1 load to the conspiracy factor?'
    		  lgb2br_txc = 'Does tranformed lgb2 load onto TXC factor?'
		  })

     switch(type, d = descriptions, m = chalmers, l = beaujean)

}

conf_mirt <- function(){
	out = new.env()
	with(out, {
	cnames <- c('lng3','lng4','tra1','tra2', 
		    'cis2', 'lng1','lng2','csp1',
		    'csp2','sci1','sci2','cis1',
		    'txc1','txc2','lgb1br')
	prednames <- c('gender2','native2')
	analysis_data = numrz2(gsm[cnames])
	# instead of model strings use the model description func
	models = model_descriptions('m') # m for mirt
	# just an idea I had
	# lapply(ls(models), function(model_spec){
	# 	       mirt.model(get(model_spec, models))
                  # })

	predictors <- gsm[prednames]
	analysis_data=na.omit(cbind(analysis_data, predictors))
	# current_model = mirt.model(models$tra_index)
	# tra_index <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$cis2_tra)
	# cis2_tra <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$lng1_2)
	# lng1_2 <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$lng1_2_obl)
	# lng1_2_obl <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$one_csp)
	# one_csp <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$csp_sci)
	# csp_sci <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$csp_sci_cov)
	# csp_sci_cov <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	# current_model = mirt.model(models$cis1_csp)
	# cis1_csp <- mirt(analysis_data[cnames],current_model, itemtype='gpcm', method='MHRM')
	current_model = mirt.model(models$txc_index)
	txc_index <- mirt(analysis_data[cnames],current_model, itemtype=c(rep('gpcm',14),'2PL'), method='MHRM')
	
		      })
	out
}


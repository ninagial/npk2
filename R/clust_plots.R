inspect_bars2 <- function(data_cl, items, select_items=c('txc1','txc2'), set_y=130 ){
		data_cl$id = 1:nrow(data_cl)
		ldf0 = dcast(melt(data_cl, id='id'), variable+value~.)
		items = items[items$item%in%select_items,]
		ldf0 = ldf0[ldf0$variable%in%select_items,]
		ldf0$value = factor(ldf0$value, levels=5:1, labels=c('Totally Disagree', 'Moderately Disagree', 'Neutral', 'Modeately Agree', 'Totally Agree')[5:1])
		ldf0 = merge(ldf0, items, by.x='variable', by.y='item')
		labelling_df = ldf0[c('variable', 'statement')]
		labelling_df = labelling_df[!duplicated(labelling_df),]
		plo = ggplot(ldf0, aes(variable,., fill=value))
		plo = plo + geom_bar(stat='identity', width=0.3) + 
			coord_flip()+ 
			guides(fill=F) + 
			theme(
			      axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), 
			      axis.title.x=element_blank(), axis.title.y=element_blank(), 
			      axis.text.x=element_blank(), axis.text.y=element_blank(), 
			      panel.background=element_rect(fill='white'))  +
			scale_fill_manual(values=agreement_palette) +
			annotate(geom='text', x=.3+1:length(select_items), y=set_y, , size=12, label=stringr::str_wrap(labelling_df$statement,70))
		plo
	}


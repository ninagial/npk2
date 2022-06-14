
	  plot_lca = function(items=c('lng3','lng4','cis1','tph2','lgb2', 'csp1'), title='LCA w/ 5 Classes: Item Response Probabilities (Overview)', data) {
		  ggplot(data[data$item %in% items,], aes(item, prob, fill=response, facet=class)) + geom_bar(stat='identity', width=0.5)+facet_wrap(class~.) + coord_flip() + scale_fill_manual(values=h$agreement_palette) + theme(panel.background=element_blank(), text=element_text(size=15)) + labs(y='Probability of Response', x='Item Label', fill='Response\nAlternative') + ggtitle(title)
	  }

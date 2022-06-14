#' Class Compare 2
class_compare2 = function(all_item_probs){

	require(entropy)
	class_combos = combn(1:(dim(all_item_probs[[1]])[1]), 2)

	# best_info = sapply(all_item_probs, function(item_probs) which.max(apply(class_combos,2, function(x) mi.plugin(item_probs[x,]))))
	best_info = lapply(all_item_probs, function(item_probs) apply(class_combos,2, function(x) mi.plugin(item_probs[x,])))

	# choices = Reduce(rbind, lapply(best_info, function(cix){
	# 	       pair = class_combos[,cix]
	# 	       peas = all_item_probs[[cix]]
	# 	       c(
	# 		 which.max(peas[pair[1],]),
	# 		 which.max(peas[pair[2],])
	# 	       )
# }))
	# list(best_info, choices)
	out = t(Reduce(rbind, best_info))
	out = cbind(t(class_combos), out)
	colnames(out) <- c('C1','C2', names(all_item_probs))
	round(out,2)
}

	

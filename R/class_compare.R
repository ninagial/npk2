#' Which classes differ the most
#'
compare_classes = function(item, item_probs){
  class_combn = combn(1:5,2)
  # entropy = apply(class_combn, 2, function(ix) {
	  # d= item_probs[[item]]
	  # cross_entropy_loss(d[ix[1]], d[ix[2]])
  # })
  distances = apply(class_combn, 2, function(ix) {
	  d= item_probs[[item]]
	  compare_ps(d[ix[1]], d[ix[2]])
  })
  # modes = apply(class_combn, 2, function(ix) {
	  # d= item_probs[[item]]
	  # c(order(d[ix[1],], decreasing=T), order(d[ix[2],], decreasing=T))
  # })
  
  ratios = apply(class_combn, 2, function(ix) {
	  d= item_probs[[item]]
	  log(d[ix[1]]/d[ix[2]])
  })
  # chitest = apply(class_combn, 2, function(ix) {
	  # d= item_probs[[item]]
	  # chisq.test(d[ix[1],], p=d[ix[2],])$p.value
  # })
  out=data.frame(t(rbind(class_combn, round(distances,3),round(ratios,3))))
  # out=t(rbind(class_combn, modes, round(distances,3), round(chitest,3))[order(distances),])
  colnames(out) = c('C1','C2','L2','logRatio')
  out$item = item
  out
}

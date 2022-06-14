# an attempt at an Automatic Description
cross_entropy_loss = function(p,q){
# this is wrong p and q are more like p1 nd p2 here https://math.stackexchange.com/questions/3389976/what-is-the-motivation-for-using-cross-entropy-to-compare-two-probability-vector
  -sum(q*log(p))
}

l2_norm = function(x){
  sqrt( sum( x^2 ))
}

compare_ps = function(p,q){
  l2_norm(p-q)
  # l2_norm(p-q)^2/2
}

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

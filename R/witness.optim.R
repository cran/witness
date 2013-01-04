witness.optim <-
function(param.form, data.set, generate.new=TRUE,...){
	if (generate.new){
		random.params = witness.starting.params(param.form)
	} else { random.params = param.form }
	optimal = optim(par=unique(as.numeric(random.params)), fn=fitOpWit, param.form=random.params, dataset=data.set, ...)
	return(optimal)
}

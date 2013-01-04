witness.starting.params <-
function(parameter.form, values=NULL){
	parameter.start.values = matrix(nrow=nrow(parameter.form), ncol=5)
	s=1
	for (i in 1:5){
		num.starting.values = length(unique(parameter.form[,i]))
		if (i ==4){ top.num=.2 } else {top.num=1}
		if (!is.null(values)){
				random.value = values[s:(s+num.starting.values-1)]
				s = s + num.starting.values
			} else {
			random.value = runif(num.starting.values, 0, top.num)
			}
		un.ones = unique(parameter.form[,i])
		new = 1:nrow(parameter.form)
		for (j in 1:length(un.ones)){
			num = which(parameter.form[,i]==un.ones[j])
			parameter.start.values[num,i] = random.value[j]
		}					
	}
	return(parameter.start.values)
}

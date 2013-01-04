witness.genetic <-
function(parameter.form, data=NULL, 
				N=1000, generations=100, pcross=.8, 
				mutProb=.001, prop.random=.001, gradient=.01,...){
			
	###### generate starting parameters N times
	parent.array = array(dim=c(dim(parameter.form), N)) 
	fit.matrix = 1:N

	####### generate initial fits
	for (i in 1:N){
		parent.array[,,i] = witness.starting.params(parameter.form)
		fit.matrix[i] = witness.est(parent.array[,,i], data=data, optim=TRUE,...)
	}	

		
	######## record best ever fits
	bestEv = min(fit.matrix)
	bestEv.par = parent.array[,,which(fit.matrix==min(fit.matrix))]
	k=0
	
	######## optimize the best ever fits
	opfit = witness.optim(bestEv.par, data.set=data, ...)	
	bestEv = opfit$value
	bestEv.par = witness.starting.params(parameter.form, values=opfit$par)
	parent.array[,,which(fit.matrix==min(fit.matrix))] = bestEv.par
	
	
		print(paste("optimal fit:", round(min(fit.matrix), digits=4)))
		print(paste("average fit:", round(mean(fit.matrix), digits=4)))	
		print(paste("generation:", k))	
	
	######## start generations loop
	while(k<generations){
		k = k + 1

		##### come up with PDF based on Fits
		fit.matrix = 1/fit.matrix
		pdf = fit.matrix/sum(fit.matrix)
		
		##### sample based on PDF
		samp = sample(1:N, replace=TRUE, size=N, prob=pdf)
		
		##### copy first generation
		nxtgen = parent.array[,,samp]
		
		#### randomly select parents
		randDraw = runif(N)
		parents = parentsFun(N, pcross*length(which(randDraw<pcross))) #### randomly match parents
		
		##### randomly pair parameters
		alleles = array(round(runif(nrow(parents)*5*nrow(parameter.form),0,1)), dim=c(nrow(parameter.form), 5, nrow(parents)))
		nxtgen[,,1:nrow(parents)] = nxtgen[,,parents[,1]] * alleles + nxtgen[,,parents[,1]] * (1-alleles)
		
		###### randomly mutate 
		randDraw = runif(N)
		if (length(which(randDraw<mutProb))>0){
			rand.mut.amt = runif(length(which(randDraw<mutProb)), -.2, .2)
			nxtgen[,,randDraw<mutProb] = nxtgen[,,randDraw<mutProb] + rand.mut.amt
		}
		
		###### randomly replace one and optimize
		samp = sample(1:length(N), 1)
		nxtgen[,,samp] = witness.starting.params(parameter.form)
		opfit = witness.optim(nxtgen[,,samp], data.set=data, ...)
		nxtgen[,,samp] = witness.starting.params(parameter.form, values=opfit$par)	
		
		###### now refit
		for (i in 1:N){
			parent.array[,,i] = nxtgen[,,i]
			fit.matrix[i] = witness.est(parent.array[,,i], data=data, optim=TRUE, ...)
		}	
		fit.matrix[samp]
			

		###### see if new fit surpasses previous fit
		new.Best = min(fit.matrix)
		if (new.Best<bestEv){
			bestEv = new.Best
			bestEv.par = nxtgen[,,which(fit.matrix==min(fit.matrix))]
		}
		
		####### make this generation the new parents
		parent.array = nxtgen
		
		####### report best fit
		cat(paste("\n\noptimal fit:", round(bestEv, digits=4)))
		cat(paste("\naverage fit:", round(mean(fit.matrix), digits=4)))	
		cat(paste("\ngeneration:", k))			

	}
	list(fit=bestEv, params=bestEv.par)
	

}

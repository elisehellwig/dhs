
setwd('d:/gdrive/projects/DHS')
#setwd('c:/gdrive/projects/DHS')
setwd('/Users/echellwig/Drive/DHS/') #Elise

source('/Users/echellwig/Documents/Research/dhs/functions/aggregate_functions.R')
#source('R/functions/pre_process_functions.R')

load('/Users/echellwig/Documents/Research/dhs/data/cttc.RData')

load('/Users/echellwig/Drive/DHS/data/processed/KR_prepared.RData')
for (recode in c('KR'#, 'IR', 'HR'
)) {
	
	
	d <- try( getDHS(recode) )
	

	if (class(d) == 'try-error') next
	
	
	p = paste0('data/aggregated/', recode)
	n <- dhsAggregate(d, level='national')
	nn <- n[[1]]
	nc <- n[[2]]
	#write.csv(n, paste0(p, '_national.csv'), row.names=FALSE)
	save(nn, file=paste0(p, '_national_num.RData'))
	save(nc, file=paste0(p, '_national _cat.RData'))

	n <- dhsAggregate(d, level='national', sex=TRUE)
	nn <- n[[1]]
	nc <- n[[2]]
	#write.csv(n, paste0(p, '_national.csv'), row.names=FALSE)
	save(nn, file=paste0(p, '_sex_national_num.RData'))
	save(nc, file=paste0(p, '_sex_national _cat.RData'))

	print('cluster')
	cl <- dhsAggregate(d, level='cluster')
	cln <- cl[[1]]
	clc <- cl[[2]]
	#write.csv(cl, paste0(p, '_cluster.csv'), row.names=FALSE)
	save(cln, file=paste0(p, '_cluster_num.RData'))
	save(clc, file=paste0(p, '_cluster_cat.RData'))
	
	cl <- dhsAggregate(d, level='cluster', sex=TRUE)
	cln <- cl[[1]]
	clc <- cl[[2]]
	#write.csv(cl, paste0(p, '_cluster.csv'), row.names=FALSE)
	save(cln, file=paste0(p, '_sex_cluster_num.RData'))
	save(clc, file=paste0(p, '_sex_cluster_cat.RData'))

}



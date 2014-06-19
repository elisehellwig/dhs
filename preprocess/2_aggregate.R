
setwd('d:/gdrive/projects/DHS')
#setwd('c:/gdrive/projects/DHS')
setwd('/Users/echellwig/Google Drive/DHS/') #Elise

source('R/functions/aggregate_functions.R')
#source('R/functions/pre_process_functions.R')


for (recode in c('KR'#, 'IR', 'HR'
)) {
	
	d <- try( getDHS(recode) )
	
	
	if (class(d) == 'try-error') next
	
	
	p = paste0('data/processed/', recode)
	n <- dhsAggregate(d, level='national')
	write.csv(n, paste0(p, '_national.csv'), row.names=FALSE)
	save(n, file=paste0(p, '_national.RData'))
		
		
	n <- dhsAggregate(d, level='national', sex=TRUE)
	write.csv(n, paste0(p, '_sex_national.csv'), row.names=FALSE)
	save(n, file=paste0(p, '_sex_national.RData'))
		
		
	cl <- dhsAggregate(d, level='cluster')
	write.csv(cl, paste0(p, '_cluster.csv'), row.names=FALSE)
	save(cl, file=paste0(p, '_cluster.RData'))
	
	
	cl <- dhsAggregate(d, level='cluster', sex=TRUE)
	write.csv(cl, paste0(p, '_sex_cluster.csv'), row.names=FALSE)
	save(cl, file=paste0(p, '_sex_cluster.RData'))
	
}



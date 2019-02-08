# TODO: Add comment
# 
# Author: jfcollin
###############################################################################

size1=150
size2=100
size3=80

y_numeric=c(rnorm(size1,0),rnorm(size2,2),rnorm(size3,2))
y_logistic=c(round(runif(size1,0)),round(runif(size2,0)),round(runif(size3,0)))
y_poisson=c(round(rnorm(size1,3)),round(rnorm(size2,9)),round(rnorm(size3,5)))

# Add some missing values

y_logistic[sample(1:length(y_logistic),10)]=NA
y_numeric[sample(1:length(y_numeric),10)]=NA
y_poisson[sample(1:length(y_poisson),10)]=NA

GROUP=c(rep("A",size1),rep("B",size2),rep("C",size3))
VAR=c(sample(c(rep("Cat 1",10),rep("Cat 2",10)),size1,replace=T),sample(c(rep("Cat 1",10),rep("Cat 2",10)),size2,replace=T),
sample(c(rep("Cat 1",10),rep("Cat 2",10)),size3,replace=T))
TIMEPOINT=rep(c("D1","D2","D3","D4","D5"),length(GROUP)/5)
SUBJID=rep(paste0("Subj ", c(1:(length(GROUP)/5))),rep(5,length(paste0("Subj ", c(1:(length(GROUP)/5))))))
	



data=data.frame(y_numeric,y_logistic,y_poisson,VAR,GROUP,TIMEPOINT,SUBJID)
save(data, file = "C:\\Users\\jfcollin\\Google Drive\\Dev\\ClinReport\\data\\data.RData")
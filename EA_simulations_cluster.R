

#########################

# NOTES

# this R script runs the EA simulations on the tardis cluster 

# Tardis thereby runs many of this scrips in parallel and asigns a diferent 'clusterid' to each worker

# The simulation contains following steps:
# Load functions and packages
# Assign 'clusterid'
# Define important algorithm paramters (e.g. population size and generations)
# Run evolutionary algorithm
# Arrange results
# Saves everything for later use (e.g. making figures and diagnosis)

##########################


#Name of analyis
name="simple_p_new"


#Load important functions
source("EA_functions4.R")

#Load important packages
packages <- c('dplyr',"tidyr","foreach","parallel","doParallel")
lapply(packages, require, character.only=TRUE)



#Get ID assigned by cluster
clusterid <- as.integer(commandArgs(TRUE)[1])
if(is.na(clusterid)){clusterid<-1}



#Define paramters
pop_size = 1000 #Population size
number_of_generations=1000 #Number of Generations
gif=0 # Do you whant a gif? (Needs special packages; keep=0 if you whant to play save)
reps <- 8 #How many parallel populations 
do_parallel=1 #Yes please


#Define analysed paramter range
bound_initial_bias=c(-0.5,2)#c(0,0.001)#
bound_theta=c(0.1,12)
bound_delta=c(0,2)#c(3,3.001)
bound_bino=c(0,1)#c(3,3.001)

dummy=rbind(bound_initial_bias,bound_theta,bound_delta,bound_bino,bound_bino)
lb<-dummy[,1]
ub<-dummy[,2]

#Define features of the environment

#Group sizes
gs_vec=c(1,5,10,20,50) 
#Cost asymmetry
fnc_vec=c(1,2,4)# #symmetric is 1 (with one point for correct)
#Intial infromation
initial_variance_vec=0#seq(0,1.5,0.5)
initial_knowledge_vec=0#seq(0,0.5,0.5)
#Information gathered during process
average_pdrift_vec = 0.3#seq(0.1,0.3,0.1)
variance_pdrift=0
#Time cost
time_cost_vec = c(0.05)
#Cooperative=1 or competitive=0 
collective_vec=c(0,1)





#Define varables for current run:
final_data=expand.grid(Group_Size=gs_vec,FNC=fnc_vec,average_pdrift=average_pdrift_vec,initial_knowledge=initial_knowledge_vec,initial_variance=initial_variance_vec,time_cost=time_cost_vec,collective=collective_vec,m1=NA,sd1=NA,m2=NA,sd2=NA,m3=NA,sd3=NA,m4=NA,sd4=NA,m5=NA,sd5=NA,m6=NA,sd6=NA,m7=NA,sd7=NA)  
nrow(final_data)
final_data$FNC <- as.numeric(final_data$FNC)

false_costs=c(2/(1+final_data$FNC[clusterid]),(2*final_data$FNC[clusterid])/(1+final_data$FNC[clusterid]))
numAgents=final_data$Group_Size[clusterid]
initial_variance=final_data$initial_variance[clusterid]
initial_knowledge=final_data$initial_knowledge[clusterid]
average_pdrift =final_data$average_pdrift[clusterid]
time_cost=as.numeric(final_data$time_cost[clusterid])
collective = final_data$collective[clusterid]

    

#Make parallel a
cl<-makeCluster(reps) #change the 2 to your number of CPU cores
registerDoParallel(cl)
clusterExport(cl, ls()) #export workspace into cluster

#Run EA algorithm
xx=foreach(r=1:reps) %dopar% { #each population in parralel
  runEA(numAgents,pop_size,number_of_generations,gif,false_costs,time_cost,initial_knowledge,initial_variance,average_pdrift,variance_pdrift,collective)
}
stopCluster(cl)#stop cluster

#rearrange results
result=NULL
for(ii in 1:length(xx[[1]][1,])){
  result[[ii]]=matrix(NA,number_of_generations,reps)
  for (i in 1:reps){
    result[[ii]][,i]<-t(xx[[i]][,ii])
  }
}


#Get results of last 10 generations
final_data$m1=mean(apply(result[[1]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd1=mean(apply(result[[1]],1,sd)[(number_of_generations-10):number_of_generations])

final_data$m2=mean(apply(result[[2]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd2=mean(apply(result[[2]],1,sd)[(number_of_generations-10):number_of_generations])

final_data$m3=mean(apply(result[[3]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd3=mean(apply(result[[3]],1,sd)[(number_of_generations-10):number_of_generations])

final_data$m4=mean(apply(result[[4]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd4=mean(apply(result[[4]],1,sd)[(number_of_generations-10):number_of_generations])

final_data$m5=mean(apply(result[[5]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd5=mean(apply(result[[5]],1,sd)[(number_of_generations-10):number_of_generations])


final_data$m6=mean(apply(result[[6]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd6=mean(apply(result[[6]],1,sd)[(number_of_generations-10):number_of_generations])

final_data$m7=mean(apply(result[[7]],1,mean)[(number_of_generations-10):number_of_generations])
final_data$sd7=mean(apply(result[[7]],1,sd)[(number_of_generations-10):number_of_generations])

print(final_data[clusterid,])
dir.create(name, showWarnings = F)
#Save results
save.image(paste0(name,"/",clusterid,name,".RData"))


    
#monopol_analysis



result=(data.frame(GS=(final_data$Group_Size),FNC=final_data$FNC,threshold=final_data$m2,bias=final_data$m1,SI=final_data$m3)) %>% 
  filter(GS%in%c(1,5,50))
bias_vec<-seq(0,1.2,0.025)#c(0.2,seq(0.5,4,0.5))#
result=result[sort(rep(seq_len(nrow(result)), length(bias_vec))), ]
result$bias=rep(bias_vec,length(result$FNC)/length(bias_vec))


cl<-makeCluster(cores) #change the 2 to your number of CPU cores
registerDoParallel(cl)
clusterExport(cl, ls()) #export workspace into cluster


dummy_result=foreach::foreach(x1=1:dim(result)[1], .combine=rbind)%dopar% {
  
  false_costs=c(2/(1+    result$FNC[x1]),(2*result$FNC[x1])/(1+result$FNC[x1]))
  
  
  accuracy=performance=maj_size=predator=left=fast_perf=RT=rep(NA,reps)
  for(i in 1:reps){
    
    
    dummy1<-simulation(0,result$GS[x1],initial_variance,initial_knowledge,0,0,  result$bias[x1],
                       rep(result$threshold[x1],result$GS[x1]),
                       result$SI[x1],false_costs,time_cost)
    
    accuracy[i]=mean(dummy1$final_decision)
    performance[i]=mean(dummy1$fitness)
    maj_size[i]=abs(mean(dummy1$final_decision)-0.5)+0.5
    left[i]=(1-(mean(dummy1$d_time)*time_cost))
    predator[i]=dummy1$predator
    fast_perf[i]=mean(round(mean(dummy1$final_decision[which(dummy1$d_time==min(dummy1$d_time))])))
    RT[i]=mean(dummy1$d_time)
  }
  
  data.frame(accuracy=mean(accuracy),performance=mean(performance),maj_size=mean(maj_size),fast_perf=mean(fast_perf),RT=mean(RT),left=mean(left),TP=mean(accuracy[predator==1]),TN=mean(accuracy[predator==0]))
  
}
stopCluster(cl)#stop cluster

result$accuracy=dummy_result$accuracy
result$performance=dummy_result$performance
result$maj_size=dummy_result$maj_size
result$left=dummy_result$left
result$add=dummy_result$accuracy-(1-dummy_result$left)
result$TP=dummy_result$TP
result$RT=dummy_result$RT
result$accuracy=dummy_result$accuracy#-(0.5*(1-dummy_result$TP)*result$FNC)
result$fast_perf=dummy_result$fast_perf
result$exp_payoff=(result$accuracy)-(0.5*(1-dummy_result$TP)*(2*result$FNC)/(1+result$FNC))-(0.5*(1-dummy_result$TN)*2/(1+    result$FNC))


result[which(result$FNC==4),] 

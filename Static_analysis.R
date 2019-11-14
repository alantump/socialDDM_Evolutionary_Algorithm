# Static analysis



#########################

# NOTES

# this R script runs the analysis the further static anaysis of the EA results and makes the plots
##########################







packages <- c('dplyr',"tidyr","foreach","parallel","doParallel","viridis","cowplot")
lapply(packages, require, character.only=TRUE)



cores=30
reps=1000000


#Sumarise results in data frames:
#######
name="divstart_new"

#Static environment with evolving sharing and innovation
load(paste0("ServerResults/",name,"/",1,name,".RData"))
final_data_all<-expand.grid(Group_Size=gs_vec,FNC=fnc_vec,average_pdrift=average_pdrift_vec,initial_knowledge=initial_knowledge_vec,initial_variance=initial_variance_vec,time_cost=time_cost_vec,collective=collective_vec,m1=NA,sd1=NA,m2=NA,sd2=NA,m3=NA,sd3=NA,m4=NA,sd4=NA,m5=NA,sd5=NA,m6=NA,sd6=NA,m7=NA,sd7=NA)  
final_data_all[1,]<-final_data[1,]


for (loop_parameters in 1:nrow(final_data)){
  if( file.exists(paste0("ServerResults/",name,"/",loop_parameters,name,".RData")) ){
    load(paste0("ServerResults/",name,"/",loop_parameters,name,".RData"))
    final_data_all[loop_parameters,]<-final_data[loop_parameters,]
  } else {
    print("File missing")
  }
  
}
final_data=final_data_all %>% filter(collective==1)


saveplot=0
source("monopol_analysis.R")




dat= result %>% filter( FNC==1)  %>% 
  mutate(TN=accuracy*2-TP)  %>% mutate(TN=ifelse(TN<0,0,TN))  %>%  gather(Rates,value,c(TN,TP)) %>% 
  mutate(Rates=ifelse(Rates=="TP","Hit","Correct rejection")) 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

STD<-ggplot(data=dat)+
  geom_line(aes(bias,value,linetype=Rates),size=1.5) +
  facet_grid(~GS)+viridis::scale_fill_viridis() +
  labs(subtitle = "Group Size",y = "Rates", x = "Bias") +
  geom_line(data=result,aes(bias,(exp_payoff+1)/2,colour=as.factor(abs(FNC))),size=1.5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*2-1,name = "Expected Payoff"),limits = c(0,1))+
  scale_color_manual(name="Cost \nasymmetry",values=heat.colors(3))+
  scale_linetype_manual(values=c("longdash", "dotted"))+
  theme(axis.text.y.right = element_text(color = "red"),  axis.line.y.right = element_line(color = "red"),  axis.title.y.right = element_text(color = "red"))+
  scale_x_continuous(breaks=seq(0,2,by=0.5))
STD

name=paste("STD_analiysis.pdf")
ggsave(name,STD, width = 7,height = 3.5,dpi = 5000)





dat= result %>% filter( FNC==-1)  %>% 
  mutate(TN=accuracy*2-TP)  %>% mutate(TN=ifelse(TN<0,0,TN))  %>%  gather(Rates,value,c(TN,TP)) %>% 
  mutate(Rates=ifelse(Rates=="TP","Hit","Correct rejection")) 
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

STD2<-ggplot(data=dat)+
  geom_line(aes(bias,value,linetype=Rates),size=1.5) +
  facet_grid(~GS)+viridis::scale_fill_viridis() +
  labs(subtitle = "Group Size",y = "Rates", x = "Bias") +
  #geom_line(data=dat,aes(bias,(exp_payoff+1)/2,colour=as.factor(abs(FNC))),size=1.5)+ 
  scale_y_continuous(sec.axis = sec_axis(~.*2-1,name = "Expected Payoff"),limits = c(0,1))+
  scale_color_manual(name="Cost of miss",values=heat.colors(3))+
  scale_linetype_manual(values=c("longdash", "dotted"))+
  scale_x_continuous(breaks=c(0,1,2))


name=paste("STD_analiysis_miniconf2_0.jpg")
ggsave(name,STD2, width = 7,height = 3.5)







#Static environment with evolving sharing and innovation
load(paste0("ServerResults/",name,"/",1,name,".RData"))
final_data_all<-expand.grid(Group_Size=gs_vec,FNC=fnc_vec,average_pdrift=average_pdrift_vec,initial_knowledge=initial_knowledge_vec,initial_variance=initial_variance_vec,time_cost=time_cost_vec,collective=collective_vec,m1=NA,sd1=NA,m2=NA,sd2=NA,m3=NA,sd3=NA,m4=NA,sd4=NA,m5=NA,sd5=NA,m6=NA,sd6=NA,m7=NA,sd7=NA)  
final_data_all[1,]<-final_data[1,]


for (loop_parameters in 1:nrow(final_data)){
  if( file.exists(paste0("ServerResults/",name,"/",loop_parameters,name,".RData")) ){
    load(paste0("ServerResults/",name,"/",loop_parameters,name,".RData"))
    final_data_all[loop_parameters,]<-final_data[loop_parameters,]
  } else {
    print("File missing")
  }
  
}
final_data=final_data_all %>% filter(collective==1)



gs_vec<- c(5,20,50)
fnc_vec<- c(4)
threshold_vec<-seq(1.5,8,0.5)#c(0.2,seq(0.5,4,0.5))#
Difference=c(0.4)


result=(data.frame(GS=(final_data$Group_Size),FNC=final_data$FNC,threshold=final_data$m2,bias=final_data$m1,SI=final_data$m3)) %>% 
  filter(GS%in%gs_vec & FNC%in%fnc_vec)
result=result[sort(rep(seq_len(nrow(result)), length(threshold_vec))), ]
result$threshold=rep(threshold_vec,length(result$FNC)/length(threshold_vec))
result$Difference=Difference


cl<-makeCluster(30) #change the 2 to your number of CPU cores
registerDoParallel(cl)
clusterExport(cl, ls()) #export workspace into cluster


dummy_result_t=foreach::foreach(x1=1:dim(result)[1], .combine=rbind)%dopar% {
  
  
  
  accuracy=diff=performance=maj_size=predator=high=low=left=rep(NA,reps)
  for(i in 1:reps){
    
    false_costs=c(2/(1+result$FNC[x1]),(2*result$FNC[x1])/(1+result$FNC[x1]))
    
    lik=0.5
    index_low=0
    while(mean(index_low)<0.2 |mean(index_low)>0.8  ){
      index_low<-(runif(result$GS[x1])<lik)
    }
    
    threshold_dummy<-rep(NA,result$GS[x1])
    threshold_dummy[index_low]<-result$threshold[x1]-result$Difference[x1]/2
    threshold_dummy[!index_low]<-result$threshold[x1]+result$Difference[x1]/2
    
    dummy1<-simulation(0,result$GS[x1],initial_variance,initial_knowledge,0,0,  result$bias[x1],
                       threshold_dummy, result$SI[x1],false_costs,time_cost)
    
    
    accuracy[i]=mean(dummy1$final_decision)
    performance[i]=mean(dummy1$fitness)
    high[i]<-mean(dummy1$fitness[!index_low])
    low[i]<-mean(dummy1$fitness[index_low])
  }
  
  data.frame(accuracy=mean(accuracy),performance=mean(performance),maj_size=mean(maj_size),left=mean(left),TP=mean(accuracy[predator==1]),diff=mean(high,na.rm=T)-mean(low,na.rm=T),low=mean(low,na.rm=T),high=mean(high,na.rm=T))
  
}
stopCluster(cl)#stop cluster

result$diff=dummy_result_t$diff
result$performance=dummy_result_t$performance
result$accuracy=dummy_result_t$accuracy
result$lower=dummy_result_t$low
result$higher=dummy_result_t$high
result$mean = (dummy_result_t$high + dummy_result_t$low)/2

result2 = result %>%  gather(type,value,c(lower,higher))

#opti_data = final_data %>% filter(Group_Size %in% gs_vec ,  FNC%in%fnc_vec)
opti_data = result %>% filter(GS %in% gs_vec ,  FNC%in%fnc_vec)%>%group_by(GS) %>% 
  summarise(m =  threshold[mean==max(mean)], y=  diff[mean==max(mean)], y2=max(mean))

p1_thres=ggplot(result,aes(x=threshold,y=diff,color=as.factor(GS)))+geom_line(size=1.2)+
  geom_hline(yintercept=0)+xlab("Average boundary seperation")+ylab(" Benefit of \n higher boundary speration")+
  theme(legend.position="none") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  scale_linetype_manual(name=c("Boundary seperation"),values=c("solid", "dotted"))+
  geom_point(data = opti_data, aes(x =m,y=y),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)


#p2_thres=  ggplot(result2,aes(x=threshold,y=value,linetype=type,color=as.factor(GS)))+geom_line(size=1.2)+
#  xlab("Average boundary seperation")+
#  ylab("Performance") +
#  theme(legend.position=c(0.38,0.87),legend.box = "horizontal") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
#  scale_linetype_manual(name=c("Boundary \nseperation:"),values=c("solid", "dotted"))+ coord_cartesian(ylim=c(-0.1,1))


p2_thres=  ggplot(result,aes(x=threshold,y=mean,color=as.factor(GS)))+geom_line(size=1.2)+
  xlab("Average boundary seperation")+
  ylab("Payoff") +
  theme(legend.position=c(0.48,0.87),legend.box = "horizontal") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  coord_cartesian(ylim=c(-0.1,1))+
  geom_point(data = opti_data, aes(x =m,y=y2),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)




p_theshold <- plot_grid(p2_thres,p1_thres,nrow=1,labels="auto")
p_theshold
if (saveplot==1)ggsave("final_plots/benefit_of_higher_threshold_opti.jpg",p_theshold, width = 12,height = 6,dpi = 800)













##############




gs_vec<- c(5,20,50)
fnc_vec<-c(4)
Difference=0.1#c(0,0.1,0.4)
SI_vec<-seq(0.05,2.05,0.1)#seq(0.2,10,0.5)#


result=(data.frame(GS=(final_data$Group_Size),FNC=final_data$FNC,threshold=final_data$m2,bias=final_data$m1,SI=final_data$m3)) %>% 
  filter(GS%in%gs_vec & FNC%in%fnc_vec)
result=result[sort(rep(seq_len(nrow(result)), length(SI_vec))), ]
result$SI=rep(SI_vec,length(result$FNC)/length(SI_vec))
result$Difference=Difference



cl<-makeCluster(30) #change the 2 to your number of CPU cores
registerDoParallel(cl)
clusterExport(cl, ls()) #export workspace into cluster


dummy_result=foreach::foreach(x1=1:dim(result)[1], .combine=rbind)%dopar% {
  
  
  
  accuracy=diff=performance=maj_size=predator=high=low=higha=first_perf=lowa=left=rep(NA,reps)
  for(i in 1:reps){
    
    false_costs=c(2/(1+result$FNC[x1]),(2*result$FNC[x1])/(1+result$FNC[x1]))
    lik=0.5
    index_low=0
    while(mean(index_low)<0.2 |mean(index_low)>0.8  ){
      index_low<-(runif(result$GS[x1])<lik)
    }
    SI_dummy<-rep(NA,result$GS[x1])
    SI_dummy[index_low]<-result$SI[x1]-result$Difference[x1]/2
    SI_dummy[!index_low]<-result$SI[x1]+result$Difference[x1]/2
    
    
    dummy1<-simulation(0,result$GS[x1],initial_variance,initial_knowledge,0,0,  result$bias[x1],
                       rep(result$threshold[x1],result$GS[x1]),SI_dummy,false_costs,time_cost*3)
    
    
    accuracy[i]=mean(dummy1$final_decision)
    performance[i]=mean(dummy1$fitness)
    higha[i]<-mean(dummy1$final_decision[!index_low])
    lowa[i]<-mean(dummy1$final_decision[index_low])
    high[i]<-mean(dummy1$fitness[!index_low])
    low[i]<-mean(dummy1$fitness[index_low])
    
    #get performance of first
    firsts=dummy1$fitness[dummy1$d_time==min(dummy1$d_time)]
    #firsts_t=mean(dummy1$d_time[dummy1$d_time==min(dummy1$d_time)])
    second=dummy1$fitness[dummy1$d_time==min(dummy1$d_time[dummy1$d_time!=min(dummy1$d_time)])]
    #second_t=mean(dummy1$d_time[dummy1$d_time==min(dummy1$d_time[dummy1$d_time!=min(dummy1$d_time)])])
    f_correct = ifelse(mean(firsts>0)<0.5,min(firsts),ifelse(mean(firsts>0)>0.5,max(firsts),0))
    s_correct = ifelse(mean(second>0)<0.5,min(second),ifelse(mean(second>0)>0.5,max(second),0))
    
    first_perf[i]<- ifelse(f_correct!=0,f_correct, s_correct) 
    
    
    
  }
  
  data.frame(accuracy=mean(accuracy),performance=mean(performance),maj_size=mean(maj_size),left=mean(left),TP=mean(accuracy[predator==1]),diff=mean(high,na.rm=T)-mean(low,na.rm=T),diffa=mean(higha,na.rm=T)-mean(lowa,na.rm=T),
             lowa=mean(lowa,na.rm=T),higha=mean(higha,na.rm=T),first_perf=mean(first_perf),low=mean(low,na.rm=T),high=mean(high,na.rm=T))
  
}
stopCluster(cl)#stop cluster



result$diff=dummy_result$diff
result$performance=dummy_result$performance
result$accuracy=dummy_result$accuracy
result$lower=dummy_result$low
result$higher=dummy_result$high
result$first=dummy_result$first_perf
result$mean = (dummy_result$high + dummy_result$low)/2

result2 = result %>%  gather(type,value,c(lower,higher))
#result2 = result2 %>%  gather(type_accuracy,valuea,c(low_accuracy,high_accuracy))
result3 =  result %>% group_by(GS) %>% summarise(first=mean(first))



#opti_data = final_data %>% filter(Group_Size %in% gs_vec ,  FNC%in%fnc_vec)
opti_data = result %>% filter(GS %in% gs_vec ,  FNC%in%fnc_vec)%>%group_by(GS) %>% 
  summarise(m =  SI[mean==max(mean)], y=  diff[mean==max(mean)], y2=max(mean))





p1=ggplot(result,aes(x=SI,y=diff,color=as.factor(GS)))+geom_line(size=1.2)+
  geom_hline(yintercept=0)+xlab("Average social drift rate")+ylab("Benefit of \n higher social drift")+
  theme(legend.position="none") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  scale_linetype_manual(name=c("social drift"),values=c("solid", "dotted")) +
  geom_point(data = opti_data, aes(x =m,y=y),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)






p2=  ggplot(result,aes(x=SI,y=mean,color=as.factor(GS)))+geom_line(size=1.2)+
  xlab("Average social drift rate")+
  ylab("Payoff") +
  theme(legend.position=c(0.51,0.25),legend.box = "horizontal") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  coord_cartesian(ylim=c(-0.1,1)) +
  geom_hline(data=result3,mapping=aes(yintercept=first,color=factor(GS)),linetype="dashed",size=1) +
  geom_point(data = opti_data, aes(x =m,y=y2),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)








p_sdrift <- plot_grid(p2,p1,nrow=1,labels="auto")
p_sdrift
if (saveplot==1)ggsave("final_plots/benefit_of_higher_si_opti.jpg",p_theshold, width = 10,height = 5,dpi = 800)






















gs_vec<- c(5,20,50)
fnc_vec<-c(4)
bias_vec<-seq(0,1,by=0.1)#0#
Difference=c(0.2)


result=(data.frame(GS=(final_data$Group_Size),FNC=final_data$FNC,threshold=final_data$m2,bias=final_data$m1,SI=final_data$m3)) %>% 
  filter(GS%in%gs_vec & FNC%in%fnc_vec)
result=result[sort(rep(seq_len(nrow(result)), length(bias_vec))), ]
result$bias=rep(bias_vec,length(result$FNC)/length(bias_vec))
result$Difference=Difference



cl<-makeCluster(25) #change the 2 to your number of CPU cores
registerDoParallel(cl)
clusterExport(cl, ls()) #export workspace into cluster


dummy_result=foreach::foreach(x1=1:dim(result)[1], .combine=rbind)%dopar% {
  
  
  
  accuracy=diff=performance=maj_size=predator=high=low=left=rep(NA,reps)
  for(i in 1:reps){
    
    false_costs=c(2/(1+result$FNC[x1]),(2*result$FNC[x1])/(1+result$FNC[x1]))
    
    lik=0.5
    index_low=0
    while(mean(index_low)<0.2 |mean(index_low)>0.8  ){
      index_low<-(runif(result$GS[x1])<lik)
    }
    
    bias_dummy<-rep(NA,result$GS[x1])
    bias_dummy[index_low]<-result$bias[x1]-result$Difference[x1]/2
    bias_dummy[!index_low]<-result$bias[x1]+result$Difference[x1]/2
    
    dummy1<-simulation(0,result$GS[x1],initial_variance,initial_knowledge,0,0,  bias_dummy,
                       rep(result$threshold[x1],result$GS[x1]), result$SI[x1],false_costs,time_cost)
    
    
    accuracy[i]=mean(dummy1$final_decision)
    performance[i]=mean(dummy1$fitness)
    high[i]<-mean(dummy1$fitness[!index_low])
    low[i]<-mean(dummy1$fitness[index_low])
  }
  
  data.frame(accuracy=mean(accuracy),performance=mean(performance),maj_size=mean(maj_size),left=mean(left),TP=mean(accuracy[predator==1]),diff=mean(high,na.rm=T)-mean(low,na.rm=T),low=mean(low,na.rm=T),high=mean(high,na.rm=T))
  
}
stopCluster(cl)#stop cluster

result$diff=dummy_result$diff
result$performance=dummy_result$performance
result$accuracy=dummy_result$accuracy
result$lower=dummy_result$low
result$higher=dummy_result$high
result$mean = (dummy_result$high + dummy_result$low)/2


result2 = result %>%  gather(type,value,c(lower,higher))

#opti_data = final_data %>% filter(Group_Size %in% gs_vec ,  FNC%in%fnc_vec)
opti_data = result %>% filter(GS %in% gs_vec ,  FNC%in%fnc_vec)%>%group_by(GS) %>% 
  summarise(m =  bias[mean==max(mean)], y=  diff[mean==max(mean)], y2=max(mean))




p1_bias=ggplot(result,aes(x=bias,y=diff,color=as.factor(GS)))+geom_line(size=1.2)+
  geom_hline(yintercept=0)+xlab("Average bias")+ylab(" Benefit of \n stronger bias")+
  theme(legend.position="none") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  scale_linetype_manual(name=c("Bias"),values=c("solid", "dotted")) +
  geom_point(data = opti_data, aes(x =m,y=y),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)












p2_bias=  ggplot(result,aes(x=bias,y=mean,color=as.factor(GS)))+geom_line(size=1.2)+
  xlab("Average bias")+
  ylab("Payoff") +
  theme(legend.position=c(0.48,0.83),legend.box = "horizontal") + scale_color_manual(name=c("Group size:"),values = c( "#56B4E9","#E69F00","#009E73"))+
  coord_cartesian(ylim=c(-0.1,1)) +
  geom_point(data = opti_data, aes(x =m,y=y2),fill= c( "#56B4E9","#E69F00","#009E73" ),color="black",shape=21,size=3)








p_bias <- plot_grid(p2_bias,p1_bias,nrow=1,labels="auto")
if (saveplot==1)ggsave("final_plots/benefit_of_higher_p_bias_opti.jpg",p_bias, width = 12,height = 6,dpi = 800)





benefit_higher  <- plot_grid(p2_bias,p1_bias,p2_thres,p1_thres,p2,p1,nrow=3,labels="auto")
if (saveplot==1)ggsave("final_plots/benefit_of_higher_all_opti.jpg",benefit_higher, width = 10,height = 15,dpi = 800)



benefit_higher_h  <- plot_grid(p2_bias,p2_thres+ theme(legend.position="none"),p2+ theme(legend.position="none"),p1_bias,p1_thres,p1,nrow=2,labels="auto",align="hv")
if (saveplot==1)ggsave("final_plots/benefit_of_higher_all_h_opti_collective4.jpg",benefit_higher_h, width = 12,height = 6,dpi = 800)

#######


#########################

# NOTES

# this R script runs the analysis of the results from 'EA_simulations_cluster' 

# Tardis thereby runs many of this scrips in parallel and asigns a diferent 'clusterid' to each worker

# The script thereby makes all important figures related to the EA (no static results)
# and saves a summary with diagnosis as a Markdown file
##########################





#Load packages
library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(viridis)

#######
#Sumarise results in data frames:
#######
name="divstart_new" #which name?

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


# Singles don't have social drift. Therefore drift = NA
final_data=final_data_all#[final_data_all$average_pdrift==0.2 & final_data_all$time_cost==0.01,]
final_data$m3[final_data$Group_Size==1]<-NA
final_data$sd3[final_data$Group_Size==1]<-NA
final_data$m4[final_data$Group_Size==1]<-NA
final_data$sd4[final_data$Group_Size==1]<-NA
final_data$m5[final_data$Group_Size==1]<-NA
final_data$sd5[final_data$Group_Size==1]<-NA

final_data$FNC <-  factor(as.numeric((final_data$FNC)))
final_data$Group_Size<-as.factor(final_data$Group_Size)
final_data$collective<-as.factor(final_data$collective)


######
# Make summarising plot 
######
p1=ggplot(data=final_data,aes(x=Group_Size,y=m1,goup=(collective),color=(collective)))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m1-sd1, ymax=m1+sd1),width=0.25, position=position_dodge(width=0.5))+
  ylab("Bias")+xlab("Group size")+theme(legend.position = c(0.5, 0.8),legend.background = element_rect(fill="white"))+ 
  scale_color_discrete(name = "Cooperative")+
  ylim(lb[1],ub[1])+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0),color="gray")
  #geom_text(aes(label = paste("time cost: ",time_cost,"\n initial knowledge: ",initial_knowledge,"\n initial variance: ",initial_variance), x = 1.5, y = -1),color="black")
  

p2=ggplot(data=final_data, mapping=aes(x=Group_Size, ymin=m2-sd2, ymax=m2+sd2,group=collective,color=collective))+geom_errorbar(width=0.25, position=position_dodge(width=0.5))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(data=final_data,aes(x=Group_Size,y=m2,group=collective,color=collective),size=2, position=position_dodge(width=0.5))+ylim(lb[2],ub[2])+
  ylab("Boundary Seperation") +xlab("Group size")+ theme(legend.position="none") #+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")


p3=ggplot(data=final_data,aes(x=Group_Size,y=m3,goup=collective,color=collective))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m3-sd3, ymax=m3+sd3),width=0.25, position=position_dodge(width=0.5))+
  ylim(lb[3],ub[3])+
  ylab("Social Drift") +xlab("Group size")+ theme(legend.position="none") +
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0),color="gray")



p4=ggplot(data=final_data,aes(x=Group_Size,y=m4,goup=collective,color=collective))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(size=2, position=position_dodge(width=0.5))+
  #geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m4-sd4, ymax=m4+sd4),width=0.25, position=position_dodge(width=0.5))+
  ylim(lb[4],ub[4])+
  ylab("Use Social Information") +xlab("Group size")+ theme(legend.position="none") +
 # ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0,1),color="gray")


p5=ggplot(data=final_data,aes(x=Group_Size,y=m5,goup=collective,color=collective))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(size=2, position=position_dodge(width=0.5))+
  #geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m5-sd5, ymax=m5+sd5),width=0.25, position=position_dodge(width=0.5))+
  ylim(lb[5],ub[5])+
  ylab("Follow first") +xlab("Group size")+ theme(legend.position="none") +
 # ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0,1),color="gray")



p6=ggplot(data=final_data,aes(x=Group_Size,y=m6,goup=collective,color=collective))+
  facet_grid(FNC ~time_cost, labeller = label_both)+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m6-sd6, ymax=m6+sd6),width=0.25, position=position_dodge(width=0.5))+
  geom_line(data=final_data,aes(x=Group_Size,y=m7,group=collective,color=collective), position=position_dodge(width=0.5))+
  ylim(-0.1,1)+
  ylab("Performance") +xlab("Group size")+ theme(legend.position="none") +
#  ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0,1),color="gray")

result_grid=plot_grid(p1,p2,p3,p4,p6,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
result_grid


#####
# Run Markdown
####

end<-'.html'
rmarkdown::render('EA_Markdown.Rmd',
                  output_file = paste0(c('EA_results/','Analysis_',name, Sys.Date(), 
                                         end), collapse=""))





p1=ggplot(data=final_data,mapping=aes(x=Group_Size,y=m1,goup=(FNC),color=(FNC)))+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m1-sd1, ymax=m1+sd1,group=FNC,color=FNC),width=0.25, position=position_dodge(width=0.5))+
  ylab("Bias")+xlab("Group size")+theme(legend.position = c(0.5, 0.8),legend.background = element_rect(fill="white"))+ 
  scale_color_discrete(name = "Cooperative")+
  ylim(lb[1],ub[1])+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0),color="gray")
#geom_text(aes(label = paste("time cost: ",time_cost,"\n initial knowledge: ",initial_knowledge,"\n initial variance: ",initial_variance), x = 1.5, y = -1),color="black")


p2=ggplot(data=final_data, mapping=aes(x=Group_Size, ymin=m2-sd2, ymax=m2+sd2,group=FNC,color=FNC))+geom_errorbar(width=0.25, position=position_dodge(width=0.5))+
  geom_point(data=final_data,aes(x=Group_Size,y=m2,group=FNC,color=FNC),size=2, position=position_dodge(width=0.5))+ylim(lb[2],ub[2])+
  ylab("Boundary Seperation") +xlab("Group size")+ theme(legend.position="none") #+
#ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")


p3=ggplot(data=final_data,aes(x=Group_Size,y=m3,goup=FNC,color=FNC))+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m3-sd3, ymax=m3+sd3),width=0.25, position=position_dodge(width=0.5))+
  ylim(lb[3],ub[3])+
  ylab("Social Drift") +xlab("Group size")+ theme(legend.position="none") +
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0),color="gray")



p4=ggplot(data=final_data,aes(x=Group_Size,y=m6,goup=FNC,color=FNC))+
  geom_point(size=2, position=position_dodge(width=0.5))+
  geom_errorbar(data=final_data, mapping=aes(x=Group_Size, ymin=m6-sd6, ymax=m6+sd6),width=0.25, position=position_dodge(width=0.5))+
  geom_line(data=final_data,aes(x=Group_Size,y=m7,group=FNC,color=FNC), position=position_dodge(width=0.5))+
  ylim(-0.1,1)+
  ylab("Performance") +xlab("Group size")+ theme(legend.position="none") +
  #  ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
  geom_hline(yintercept = c(0,1),color="gray")

result_grid=plot_grid(p1,p2,p3,p4,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
result_grid


final_data$Index =  1:nrow(final_data)
selection = final_data %>% filter(Group_Size!=1, FNC==4)
which_example= sample(selection$Index,1)
load(paste0(name,"/",which_example,name,".RData"))
example1=NULL

example1[[1]]<-result[[1]]
example1[[2]]<-result[[2]]
example1[[3]]<-result[[3]]



###Make example plots:
param=1
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))

p_1 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none")+ ylab("Bias")


###Make example plots:
param=2
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))

p_2 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none")+ ylab("Boundary seperation")



param=3
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))

p_3 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none") + ylab("Social drift")



result_grid=plot_grid(p_1,p_2,p_3,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
result_grid
if (saveplot==1)ggsave(paste0("final_plots/",which_example,"good_example.jpg"),result_grid, width = 9,height = 3,dpi = 800)




#Random for supplement


num_of_examples=6

final_data$Index =  1:nrow(final_data)
selection = final_data %>% filter(Group_Size!=1)
which_example_vec= sample(selection$Index,num_of_examples)
p_1=p_2=p_3=grids=NULL
ex=0
for (ex2 in 1:num_of_examples){


which_example= which_example_vec[ex2]
load(paste0("ServerResults/",name,"/",which_example,name,".RData"))
example1=NULL

example1[[1]]<-result[[1]]
example1[[2]]<-result[[2]]
example1[[3]]<-result[[3]]



###Make example plots:
param=1
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))

ex=ex+1
p_1 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+ylim(-0.02,2)+
  theme_classic()+
  theme(legend.position = "none")+ ylab("Bias")


###Make example plots:
param=2
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))

ex=ex+1
p_2 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+
  theme_classic()+
  theme(legend.position = "none")+ ylab("Boundary seperation")



gs = final_data$Group_Size[ which_example_vec[ex2]]
coll=final_data$collective[ which_example_vec[ex2]]
asy=final_data$FNC[ which_example_vec[ex2]]

param=3
pops <- data.frame(example1[[param]]) %>% dplyr::mutate(Generations=c(1:length(X1))) %>%
  gather("pop","parameter",X1:paste0("X",reps))   
means <- data.frame(y=apply(example1[[param]],1,mean)) %>% dplyr::mutate(x=1:length(y))
ex=ex+1
p_3 <- ggplot() + 
  geom_line(data = pops,aes(x=Generations,y=parameter,color=pop,group=pop), size = 0.5,alpha = 0.7 )+#,color="gray"
  geom_line(data=means,aes(x=x,y=y),color="black",size=1)+ylim(-0.2,2)+
  theme_classic()+
  theme(legend.position = "none") + ylab("Social drift")+
  geom_text(aes(label = paste("Group size: ",final_data$Group_Size[ which_example_vec[ex2]],
                              "\nCost asymmetry: ",asy,
                              "\nCooperative:  ",coll), x = 750, y = 0.7),color="black")


grids[[ex2]]= plot_grid(p_1,p_2,p_3,nrow=1)
print(gs)
}
  
  
  result_grid = plot_grid(grids[[1]],grids[[2]],grids[[3]],
                          grids[[4]],grids[[5]],grids[[6]],ncol=1)
  
 


if (saveplot==1)ggsave(paste0("final_plots/",which_example_vec[1],"random_example.jpg"),result_grid, width = 9,height = 1.7*num_of_examples,dpi = 600)







rundas=0

if (rundas==1){

  
  
  
  
  final_data_comp <- final_data %>% filter(collective==0)
  
  p1=ggplot(data=final_data_comp,aes(x=Group_Size,y=m1,goup=(FNC),color=(FNC)))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_comp, mapping=aes(x=Group_Size, ymin=m1-sd1, ymax=m1+sd1),width=0.25, position=position_dodge(width=0.5))+
    ylab("Bias")+xlab("Group size")+theme(legend.position = c(0.4, 0.8),legend.background = element_rect(fill="white"))+ 
    scale_color_discrete(name = "Cost \nasymmetry")+
    ylim(-0.2,1.15)+
    geom_hline(yintercept = c(0),color="gray")
  #geom_text(aes(label = paste("time cost: ",time_cost,"\n initial knowledge: ",initial_knowledge,"\n initial variance: ",initial_variance), x = 1.5, y = -1),color="black")
  
  
  p2=ggplot(data=final_data_comp, mapping=aes(x=Group_Size, ymin=m2-sd2, ymax=m2+sd2,group=FNC,color=FNC))+geom_errorbar(width=0.25, position=position_dodge(width=0.5))+
    geom_point(data=final_data_comp,aes(x=Group_Size,y=m2,group=FNC,color=FNC),size=2, position=position_dodge(width=0.5))+
    ylim(-0.,6)+
    ylab("Boundary Seperation") +xlab("Group size")+ theme(legend.position="none") #+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")
  
  
  p3=ggplot(data=final_data_comp,aes(x=Group_Size,y=m3,goup=FNC,color=FNC))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_comp, mapping=aes(x=Group_Size, ymin=m3-sd3, ymax=m3+sd3),width=0.25, position=position_dodge(width=0.5))+
    ylim(lb[3],ub[3])+
    ylab("Social Drift") +xlab("Group size")+ theme(legend.position="none") +
    #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0),color="gray")
  
  
  p6=ggplot(data=final_data_comp,aes(x=Group_Size,y=m6,goup=FNC,color=FNC))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_comp, mapping=aes(x=Group_Size, ymin=m6-sd6, ymax=m6+sd6),width=0.25, position=position_dodge(width=0.5))+
    geom_line(data=final_data_comp,aes(x=Group_Size,y=m7,group=FNC,color=FNC), position=position_dodge(width=0.5))+
    ylim(-0.01,1)+
    ylab("Performance") +xlab("Group size")+ theme(legend.position="none") +
    #  ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0,1),color="gray")
  
  
  
  
  
  result_grid=plot_grid(p1,p2,p3,p6,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
  result_grid
  
  
  
  final_data_coop <- final_data %>% filter(collective==1)
  
  p1=ggplot(data=final_data_coop,aes(x=Group_Size,y=m1,goup=(FNC),color=(FNC)))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_coop, mapping=aes(x=Group_Size, ymin=m1-sd1, ymax=m1+sd1),width=0.25, position=position_dodge(width=0.5))+
    ylab("Bias")+xlab("Group size")+theme(legend.position = c(0.4, 0.8),legend.background = element_rect(fill="white"))+ 
    scale_color_discrete(name = "Cost \nasymmetry")+
    ylim(-0.2,1.15)+
    geom_hline(yintercept = c(0),color="gray")
  #geom_text(aes(label = paste("time cost: ",time_cost,"\n initial knowledge: ",initial_knowledge,"\n initial variance: ",initial_variance), x = 1.5, y = -1),color="black")
  
  
  p2=ggplot(data=final_data_coop, mapping=aes(x=Group_Size, ymin=m2-sd2, ymax=m2+sd2,group=FNC,color=FNC))+geom_errorbar(width=0.25, position=position_dodge(width=0.5))+
    geom_point(data=final_data_coop,aes(x=Group_Size,y=m2,group=FNC,color=FNC),size=2, position=position_dodge(width=0.5))+
    ylim(-0.,6)+
    ylab("Boundary Seperation") +xlab("Group size")+ theme(legend.position="none") #+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")
  
  
  p3=ggplot(data=final_data_coop,aes(x=Group_Size,y=m3,goup=FNC,color=FNC))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_coop, mapping=aes(x=Group_Size, ymin=m3-sd3, ymax=m3+sd3),width=0.25, position=position_dodge(width=0.5))+
    ylim(lb[3],ub[3])+
    ylab("Social Drift") +xlab("Group size")+ theme(legend.position="none") +
    #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0),color="gray")
  
  
  p6=ggplot(data=final_data_coop,aes(x=Group_Size,y=m6,goup=FNC,color=FNC))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_coop, mapping=aes(x=Group_Size, ymin=m6-sd6, ymax=m6+sd6),width=0.25, position=position_dodge(width=0.5))+
    geom_line(data=final_data_coop,aes(x=Group_Size,y=m7,group=FNC,color=FNC), position=position_dodge(width=0.5))+
    ylim(-0.01,1)+
    ylab("Performance") +xlab("Group size")+ theme(legend.position="none") +
    #  ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0,1),color="gray")
  
  
  
  
  
  result_grid=plot_grid(p1,p2,p3,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
  result_grid
  
  
  
  if (saveplot==1)ggsave("final_plots/coop_result1.jpg",result_grid, width = 9,height = 3,dpi = 800)
  
  
  
  
  
  
  final_data_compare <- final_data %>% filter(FNC==4 & Group_Size!=1)
  
  p1_c=ggplot(data=final_data_compare,aes(x=Group_Size,y=m1,goup=collective,color=(collective)))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_compare, mapping=aes(x=Group_Size, ymin=m1-sd1, ymax=m1+sd1),width=0.25, position=position_dodge(width=0.5))+
    ylab("Bias")+xlab("Group size")+theme(legend.position = c(0.4, 0.8),legend.background = element_rect(fill="white"))+ 
    scale_color_manual(name = "Groups are:",labels=c("Competitive","Cooperative"),values = c( "#C4961A","#293352"))+
    ylim(-0.2,1.15)+
    geom_hline(yintercept = c(0),color="gray")
  #geom_text(aes(label = paste("time cost: ",time_cost,"\n initial knowledge: ",initial_knowledge,"\n initial variance: ",initial_variance), x = 1.5, y = -1),color="black")
  
  
  p2_c=ggplot(data=final_data_compare, mapping=aes(x=Group_Size, ymin=m2-sd2, ymax=m2+sd2,group=collective,color=collective))+
    geom_errorbar(width=0.25, position=position_dodge(width=0.5))+
    geom_point(data=final_data_compare,aes(x=Group_Size,y=m2,group=collective,color=collective),size=2, position=position_dodge(width=0.5))+
    scale_color_manual(name = "Groups are:",labels=c("Cooperative", "Competitive"),values = c( "#C4961A","#293352"))+
    ylim(-0.,6)+
    ylab("Boundary Seperation") +xlab("Group size")+ theme(legend.position="none") #+
  #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")
  
  
  p3_c=ggplot(data=final_data_compare,aes(x=Group_Size,y=m3,goup=collective,color=collective))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_compare, mapping=aes(x=Group_Size, ymin=m3-sd3, ymax=m3+sd3),width=0.25, position=position_dodge(width=0.5))+
    scale_color_manual(name = "Groups are:",labels=c("Cooperative", "Competitive"),values = c( "#C4961A","#293352"))+
    ylim(lb[3],ub[3])+
    ylab("Social Drift") +xlab("Group size")+ theme(legend.position="none") +
    #ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0),color="gray")
  
  
  p6_c=ggplot(data=final_data_compare,aes(x=Group_Size,y=m6,goup=collective,color=collective))+
    geom_point(size=2, position=position_dodge(width=0.5))+
    geom_errorbar(data=final_data_compare, mapping=aes(x=Group_Size, ymin=m6-sd6, ymax=m6+sd6),width=0.25, position=position_dodge(width=0.5))+
    scale_color_manual(name = "Groups are:",labels=c("Cooperative", "Competitive"),values = c( "#C4961A","#293352"))+
    geom_line(data=final_data_compare,aes(x=Group_Size,y=m7,group=collective,color=collective), position=position_dodge(width=0.5))+
    ylim(-0.01,1)+
    ylab("Performance") +xlab("Group size")+ theme(legend.position="none") +
    #  ggtitle("Intital variance (horizontal)\nIntitial Knowledge (vertical)")+
    geom_hline(yintercept = c(0,1),color="gray")
  
  
  
  
  result_grid_compare=plot_grid(p1_c,p2_c,p3_c,nrow=1,labels="auto")#,rel_widths = c(1,1.5,1)
  
  result_grid=plot_grid(p1,p2,p3,p6,p1_c,p2_c,p3_c,p6_c,nrow=2,labels="auto")#,rel_widths = c(1,1.5,1)
  result_grid
  
  
  
  if (saveplot==1)ggsave("final_plots/ea_result1.jpg",result_grid, width = 12,height = 6,dpi = 800)
  
  
  
  
  if (saveplot==1)ggsave("final_plots/ea_result_compare1.jpg",result_grid_compare, width = 9,height = 3,dpi = 800)
  
  



full=rbind(upperrow,bias_l,theta_l,delta_l)
print( xtable::xtable(full, type = "latex",
                      caption = "Investigated parameter space in the Evolutionary Algorithm",
                      label = "tab:range"),
       sanitize.text.function=identity,file="Bounds.tex",
       include.rownames=FALSE,
       include.colnames=FALSE,
       hline.after = 1, caption.placement = "top")

  
}




#########################

# NOTES

# this R script contains important functions to run the EA algorithm


# 'simulation' is the core socialDDM simulation

# 'runEA' runs the evolutionary algorithm

##########################


simulation<-function(plot_yes,n,initial_variance,initial_knowledge, average_pdrift, variance_pdrift ,initial_bias,theta,delta,false_costs,time_cost){

  
  time_limit=Inf
  if (time_cost>1){time_limit<-time_cost; time_cost=0; }
  
  p_drift =  rnorm(n,average_pdrift,variance_pdrift)
  
  base_line=0.5
  
  #save stuff
  d_time = rep(NA,n)
  
  
  h = .05;#time steps 
  sigma =1#(exp(rnorm(n,log(1),0.5)))#1; 
  t = 1;
  
  predator=runif(1)<base_line
  if(predator)    intercept=rnorm(n,0.5*theta+initial_knowledge+initial_bias,initial_variance)
  if(!predator)    intercept=rnorm(n,0.5*theta+initial_knowledge-initial_bias,initial_variance)
  
  
  L = intercept;
  
  

  dec=rep(FALSE,n)
  social_strength=0
  
  new_dec=TRUE
  if(plot_yes==1){ L_large=L}
  
  while (any(dec==FALSE) & (t)*h<time_limit){#| 
    
    dec_before <- dec
    d_time[!(L<= 0 | L >= theta)==dec]=t #get decision time (in steps)
    dec= L <= 0 | L >= theta; # yes if decision is made
    
    if (new_dec==TRUE) {
      if (sum(dec)>0){
        social_strength=  ( sum(L[dec] >= theta[dec]*0.5)- sum(L[dec] <= theta[dec]*0.5))
      }
    }
    
    randomNorm = rnorm(n,0,1); #noise
    evidence = (p_drift+(social_strength*delta))*h+(sigma*h^0.5)*randomNorm; # drift plus noise
    
    
     evidence[dec]=0; # no further evidence for individuals who decided already
    L = L + evidence #old evidence plus new evidence
    
    t = t+1; # new step
    
    new_dec <- any((dec)!=(dec_before))
    if(plot_yes==1){L_large<-cbind(L_large,L)}
    
    
  }
  #store stuff  
  d_time[is.na(d_time)]<- t #after all repetitions they are forced to decide
  d_time=(d_time-1)*h #get decision time
  
  final_decision<-L >= theta #if not decided wrong; #after all repetitions they are forced to decide#
  
  if (predator)   fitness = ifelse(final_decision,1,-false_costs[2])-(d_time*time_cost)
  if (!predator)   fitness = ifelse(final_decision,1,-false_costs[1])-(d_time*time_cost)
  
  
  
  sim_data<-data.frame(final_decision,d_time,fitness,predator)
  
  if(plot_yes==1){matplot(seq(h,t*h,h),t(L_large),type="l",ylim=c(0,max(theta)))}
  
  return(sim_data)
}







#############################################################################################################################################################################################
#The Evolutionary algorithm 
#############################################################################################################################################################################################

runEA<-function(numAgents,pop_size,number_of_generations,gif,false_costs,time_cost,initial_knowledge,initial_variance,average_pdrift,variance_pdrift,collective){
  
  if(collective==0) n_eval=ceiling(pop_size*10)#25#/as.numeric(numAgents)
  if(collective==1) n_eval=ceiling(pop_size*10) #10
  
  
  library(scales)
  
  result=NULL
  
  # randomly allocate genes to inddividuals
  init="rand"#"rand"#"evo"
  genpool=matrix(NA, pop_size,length(ub))
  
  if (init=="rand"){
  for (gene_i in 1:length(ub)){
    start_mean_gen <- runif(1,0.1,0.9)
    a=1;  b=(a/start_mean_gen)-a
    chromosom=rbeta(pop_size,a,b)
    chromosom=rescale(chromosom,c(lb[gene_i],ub[gene_i]),c(0,1))
    genpool[,gene_i] <- chromosom
    
  }
    genpool[,c(4,5)] <- round(genpool[,c(4,5)]) #make binomial
  } else{
    inits <- c(0,5,0.1,0,0)
    for (gene_i in 1:length(ub)){
      genpool[,gene_i] <- inits[gene_i]
    }  
  }
  
  result=rbind(result,c(apply(genpool,2,mean),0,0))
  
  ##For the gif:
  fitness_long=NULL
  results_long=NULL
  for(gen in 1:(number_of_generations-1)){
    
    
    
    
    probs=rep(1,pop_size)
    fitness=rep(0,pop_size)
    count=rep(0,pop_size)
    first_perf=rep(NA,n_eval) #saves the performance of the first
    for (evals in 1:n_eval){
      
      index=sample(1:pop_size,numAgents,prob=probs)
   
      count[index]= count[index]+1
      #each time a individual is picked the probaibity that it is picked again is a fith of before 
      #because the probabilities a relative it should assure a relative uniform likelihood to be picked.
      #Note that the randomness helps the EA to maintain some variance in the genpool.
      probs[index]=probs[index]/5 #reduce probs performaing ind. 
      probs<-probs/max(probs) #scale to avoid super small number
      
      # Define defauls
      plot_yes=0; 
      deltas = genpool[index,3]
      
      heursitics=1
      if (heursitics>=1){
      deltas[genpool[index,4]==0]=0 #Social information use
      }
      if (heursitics==2){
        deltas[genpool[index,5]==1]=99 #copying first (dominant)
      }
      
      
      if (collective==0){ 
        sim_result<-simulation(plot_yes,numAgents,initial_variance,initial_knowledge,average_pdrift, variance_pdrift ,initial_bias=genpool[index,1],theta=genpool[index,2],delta=deltas,false_costs,time_cost)
        fitness[index]<-  fitness[index]+sim_result$fitness
        
         } else {
           sim_result<-simulation(plot_yes,numAgents,initial_variance,initial_knowledge,average_pdrift, variance_pdrift ,initial_bias=genpool[index,1],theta=genpool[index,2],delta=deltas,false_costs,time_cost)
           fitness[index]<-  fitness[index]+mean(sim_result$fitness)
         }
      
      
      #get performanc eof first
      firsts=sim_result$fitness[sim_result$d_time==min(sim_result$d_time)]
      second=sim_result$fitness[sim_result$d_time==min(sim_result$d_time[sim_result$d_time!=min(sim_result$d_time)])]
      correct = ifelse(mean(firsts>0)<0.5,min(firsts),ifelse(mean(firsts>0)>0.5,max(firsts),0))
      first_perf[evals]<- ifelse(correct!=0,correct, sample(second,1) ) 
      
    
      
    }
    #plot(fitness~genpool[,3])
    
    if (gif==1){
      #fitness_long=c(fitness_long,rescale(fitness))
      fitness_long=c(fitness_long,(fitness))
      results_long = rbind(results_long,(genpool))
    }
    
    turnement=1
   if (turnement==1){
    #k-turnement selection
    k=3
    winners1=winners2=rep(NA,pop_size)
    for (turns in 1:pop_size){
      indexes=sample(1:pop_size,k)
      winners1[turns]=indexes[nnet::which.is.max(fitness[indexes])]
      
    }
    hist(winners1)
   }else{
     winners1=sample(pop_size,replace = T,prob = fitness+abs(min(fitness)) )
    }
    
    genpool <- genpool[winners1,]
    
    ####crossover
    for (cross in 1:(ncol(genpool))){
      corssover_rate=0.05
      dummy_index<-which(runif(pop_size)<corssover_rate)
      dummy_index_partner<-sample(pop_size,length(dummy_index))
      if(length(dummy_index)>0){
      for (corss_process in 1:length(dummy_index)){
        which_gen<-sample(length(ub),1)
        if (which_gen %in% c(4,5)) which_gen <- c(4,5)
          
          
        genpool[c(dummy_index[corss_process],dummy_index_partner[corss_process]),which_gen] =
          genpool[c(dummy_index_partner[corss_process],dummy_index[corss_process]),which_gen]
      }
    }}
    ###mutation
    
    for (mut in 1:ncol(genpool)){
      mutation_rate=0.02
      dummy_index<-runif(pop_size)<mutation_rate
      if (mut<=3){
      genpool[dummy_index,mut]<-  genpool[dummy_index,mut] + rnorm(sum(dummy_index),0,(ub-lb)[mut]*0.05)

      genpool[genpool[,mut]<lb[mut],mut] <- lb[mut] - ( genpool[genpool[,mut]<lb[mut],mut] - lb[mut]) #mirror back to avoid bias to drifting to bounds
      genpool[genpool[,mut]>ub[mut],mut] <- ub[mut] - ( genpool[genpool[,mut]>ub[mut],mut] - ub[mut])
      } else {
        genpool[dummy_index,mut] = rbinom(sum(dummy_index),1,0.5)
        
        if (mut==4) genpool[dummy_index,mut+1][genpool[dummy_index,mut]==1]=0 #If I follow I cant use SI and vice versa
        if (mut==5) genpool[dummy_index,mut-1][genpool[dummy_index,mut]==1]=0 
      }
      }
    
    
    result=rbind(result,c(apply(genpool,2,mean),mean(fitness)/(n_eval/(pop_size/numAgents)),mean(first_perf)))

    
    print(gen)
  }
  
  if (gif==1){
    library(gganimate)
    library(tweenr)
    library(magick)
    library(purrr)
    library(tidyr)
    library(dplyr)
    library(cowplot)
    
    
   
    
    plot_data=data.frame(ease=rep('cubic-in-out',length(fitness_long)),fitness_long,x1=results_long[,1],x2=results_long[,2],x3=results_long[,3],x4=results_long[,4],x5=results_long[,5],id=rep(1:pop_size,(number_of_generations-1)),gen=sort(rep(1:(number_of_generations-1),pop_size)))
   
    number_of_frames=number_of_generations*2
    if (number_of_generations>99){
      number_of_frames=round(number_of_generations/2)
      plot_data= plot_data %>% filter(gen %in% seq(1,number_of_generations,5))
    }
    
    
    plot_data2=plot_data %>%   gather(type,phen,c(x1,x2,x3,x4,x5))  
    plot_data2$max=ifelse(plot_data2$type=="x1",ub[1],ifelse(plot_data2$type=="x2",ub[2],ub[3]))
    plot_data2$min=ifelse(plot_data2$type=="x1",lb[1],ifelse(plot_data2$type=="x2",lb[2],lb[3]))
    plot_data2=plot_data2 %>% mutate(type=ifelse(type=="x1","Bias",ifelse(type=="x2","Boundary Seperation",ifelse(type=="x3","Social Drift",ifelse(type=="x4","SI use","Follow")))))
    pnew=ggplot(plot_data2,aes(x=phen,y=fitness_long,color=as.factor(id)))+geom_point() +transition_states(gen,1,1)+ facet_wrap(~type, scales="free")+ theme(legend.position="none")+ labs(title = 'Generation: {closest_state}', x = 'Phenotype', y = 'Fitness')+ ylim(0,max(plot_data2$fitness_long)) 
     #+geom_smooth(aes(color="b"),se=FALSE,span = 2)
    anim_save(paste("Output/tournement","test",".gif"),animation=animate(pnew,nframes=number_of_frames,width=1100,height=600))

    # p=ggplot(plot_data,aes(x=x1,y=fitness_long,color=as.factor(id)))+geom_point()+ transition_states(gen,2,1) + theme(legend.position="none")+ labs(title = 'Generation: {closest_state}', x = 'Bias', y = 'Fitness') + xlim(lb[1],ub[1]) + ylim(0,max(fitness_long))
    # p2=ggplot(plot_data,aes(x=x2,y=fitness_long,color=as.factor(id)))+geom_point()+ transition_states(gen,2,1) + theme(legend.position="none")+ labs(title = 'Generation: {closest_state}', x = 'Boundary Seperation', y = 'Fitness')  + xlim(lb[2],ub[2]) + ylim(0,max(fitness_long))
    # p3=ggplot(plot_data,aes(x=x3,y=fitness_long,color=as.factor(id)))+geom_point()+ transition_states(gen,2,1) + theme(legend.position="none")+ labs(title = 'Generation: {closest_state}', x = 'Social Drift', y = 'Fitness')  + xlim(lb[3],ub[3]) + ylim(0,max(fitness_long))
    # 
    # anim_save(paste("Output/p1",r,".gif"),animation=animate(p,nframes=number_of_generations*8))
    # anim_save(paste("Output/p2",r,".gif"),animation=animate(p2,nframes=number_of_generations*8))
    # anim_save(paste("Output/p3",r,".gif"),animation=animate(p3,nframes=number_of_generations*8))
    # 
    # map2(
    #   paste("Output/p1",r,".gif") %>% image_raster() %>% as.list(),
    #   paste("Output/p2",r,".gif") %>% image_raster() %>% as.list(),
    #   ~image_append(c(.x, .y))
    # ) %>%
    #   lift(image_join)(.) %>%
    #   image_write(paste("Output/",r,"result.gif"))
    # 
    # 
    # map2(
    #   paste("Output/",r,"result.gif") %>% image_read() %>% as.list(),
    #   paste("Output/p3",r,".gif") %>% image_read() %>% as.list(),
    #   ~image_append(c(.x, .y))
    # ) %>%
    #   lift(image_join)(.) %>%
    #   image_write(paste("Output/",r,"result2.gif"))
    
    
    
    
  }
  cbind(result)
}


library(data.table)
library(Hmisc)
options(scipen = 999)

dealvaroutoflimits<-function(causevar, maxlimit, minlimit){
  
  big_indexes<- which(causevar> maxlimit )
  small_indexes<- which(causevar< minlimit)
  if(length(big_indexes)>0){
    causevar[big_indexes]<-unname(maxlimit) 
  }
  if(length(small_indexes)>0){
    causevar[small_indexes]<-unname(maxlimit) 
  }
  causevar
}
diffofeveryrow<-function(causeseffect){
  h<-sapply(1:(nrow(causeseffect)-1), function(j) 
    sapply(1:ncol(causeseffect),function(i) as.numeric(diff(causeseffect[,i],j))))
  
  t<-sapply(1:(nrow(causeseffect)-1), function(lag) 
                 cbind(c((1+lag):nrow(causeseffect)),c(1:(nrow(causeseffect)-lag))))
  t<-do.call(rbind,t)
  
  
  diff_causeseffect1 <-do.call(rbind,h)
  diff_causeseffect<-data.frame(cbind(t,diff_causeseffect1))
  diff_causeseffect
}
dividetolimits<-function(causeseffect, limits){
  maxdiff<-limits[,2]-limits[,1]   #maximum change in cause variables  
  causeseffect<-sapply(1:ncol(causeseffect), function(y) (causeseffect[,y]-limits[y,1])/maxdiff[y])
  data.frame(causeseffect)
}
varImpmultiplication<-function(causeseffect,varImp){
  causeseffect[,1:(ncol(causeseffect)-1)]<-sapply(1:(ncol(causeseffect)-1),
                                                  function(x) causeseffect[,x]*varImp[x])
  causeseffect
}
seperatez0dataset<-function(diff_causeseffect, threshold1, threshold3){
  
  output <- unlist(data.table(abs(diff_causeseffect[,
                     3:(ncol(diff_causeseffect)-1)]))<=threshold1)
  output1<-output*1
  indexes0z<- which(abs(diff_causeseffect[,
                    ncol(diff_causeseffect)])<=(threshold3) & rowSums(output1)<((ncol(diff_causeseffect)-3)-1))
  z0<-FALSE
  if(length(indexes0z)>0){
    diff_causeseffect[indexes0z,ncol(diff_causeseffect)]<-0
    
    sign_ind3<-sapply(3:ncol(diff_causeseffect)
                      , function(x) as.numeric(sign(diff_causeseffect[indexes0z,x]))) 
    if(length(indexes0z)==1){
      datdz0<-data.frame(cbind(diff_causeseffect[indexes0z,1]
                               ,diff_causeseffect[indexes0z,2],t(sign_ind3)))
      
    }else{
      datdz0<-data.frame(cbind(diff_causeseffect[indexes0z,1]
                               ,diff_causeseffect[indexes0z,2],sign_ind3))
      
    }
    diff_causeseffect<-diff_causeseffect[-indexes0z,]
    z0=TRUE
  }
  if(z0==TRUE){return(list(diff_causeseffect=diff_causeseffect,z0=z0, datdz0=datdz0))
    }else{return(list(diff_causeseffect=diff_causeseffect,z0=z0))}
  
}
findcandidateindexes<-function(diff_causeseffect, threshold1){
  
  candlistindexes<-as.list(NULL)
  for(i in 3:(ncol(diff_causeseffect)-1)){
    candlistindexes[[i-2]]<- which(abs(diff_causeseffect[,i])<=threshold1)
     }
  #reduce points where we do not move significantly in any direction
  deletedcommons<-Reduce(intersect, candlistindexes)
  
  if(length(deletedcommons)>0){
    for(i in 1:length(candlistindexes)){
      row_sub = sapply( 1:length(candlistindexes[[i]]), function(row) all(find.matches(candlistindexes[[i]][row],deletedcommons)$match==0))
      candlistindexes[[i]]<- candlistindexes[[i]][row_sub]
      
    }
     
    }
  return(list(diff_causeseffect=diff_causeseffect, candlistindexes=candlistindexes))
}
setcausevarzero<-function(diff_causeseffect, candlistindexes,threshold1, threshold2){
  diff_causeseffect3<-diff_causeseffect
  bigratios<-NULL
  smallratios<-NULL
  for(i in 1:(ncol(diff_causeseffect)-3)){
    if(length(candlistindexes[[i]])!=0){
      
      for(j in candlistindexes[[i]]){
        ratioss<- abs(diff_causeseffect3[j,3:(ncol(diff_causeseffect)-1)])/
                  sum(abs(diff_causeseffect3[j,3:(ncol(diff_causeseffect)-1)]))
      
          sumofcand<- sum(ratioss[which((abs(diff_causeseffect3[j,
                          3:(ncol(diff_causeseffect)-1)]))<=threshold1)])
          howmanycand<- sum((abs(diff_causeseffect3[j,
                            3:(ncol(diff_causeseffect)-1)])<=threshold1)*1)
          if(unname(sumofcand<=threshold2 | ratioss[i]<=threshold2/howmanycand)){
            
            diff_causeseffect[j,i+2]<-0
            smallratios<-c(smallratios,unname(ratioss[i]))
          }else if (ratioss[i]>threshold2){
            bigratios<-c(bigratios,unname(ratioss[i]))
          } 
       
       
      }
    }
  }
  return(list(diff_causeseffect=diff_causeseffect, smallratios=smallratios,
              bigratios=bigratios))
}
obtainallcorners<-function(sign_diff_causeseffect){
  dat1<-sign_diff_causeseffect
  candlistindexes2<-NULL
  
  for (i in 3:(ncol(dat1))){
    candlistindexes2<-which(dat1[,i]==0)
    if(length(candlistindexes2)>0){
      for(j in candlistindexes2){
        newline1<-dat1[j,]
        newline1[i]<-1
        newline2<-dat1[j,]
        newline2[i]<--1
        newlines<-rbind(newline1, newline2)
        dat1<-rbind(dat1,newlines)
      }
    }
    rownames(dat1)<-1:nrow(dat1)}
  #multiply rows with -1 where effect is 1
  dat1[which(dat1[,ncol(dat1)]==1),3:ncol(dat1)]<-dat1[which(dat1[, ncol(dat1)]==1) ,3:ncol(dat1)]*(-1)
  for(x in 3:ncol(dat1)){
    dat1[,x] <- factor(dat1[,x], levels = c("-1","0","1"))
  }
  
  factors<- list()
  factors[[1]]<- dat1[,3] 
  for(i in 3:(ncol(dat1))){
    
    factors[[i-2]]<- dat1[,i] 
  }
  
  a<-split(dat1,f=factors, drop=TRUE)
  values<-sapply(1:length(a), function(x) strsplit(names(a)[x],".", fixed=TRUE))
  signvalues<-NULL
  perc<- sapply(1:length(a), function(p) nrow(a[[p]]))
  
  for(i in 1:length(a)){
    signvalues<-rbind(signvalues,as.numeric(values[[i]]))
  }
  return(list(signvalues=signvalues, perc=perc))
}
createcorners<-function(n){
  alt<-c(-1,1)
  alt<-unname(data.frame(replicate(n,alt)))
  all<-expand.grid(alt)
  all1<-data.frame(all)
  return(all1)
}
eliminatecorners<-function(signvalues,all1){
  for(i in 1:nrow(signvalues))
  {
    if ( signvalues[i,ncol(signvalues)]==1){
      
      s<- (signvalues[i,-ncol(signvalues)]*(-1))
      s<-data.table(t(s))
      if(nrow(all1)!=0){
        m<-find.matches(all1,s)
        if(sum(m$matches)!=0){
          all1<-all1[-which(m$matches==1),]
        }
      }
      
    }else{
      s<- (signvalues[i,-ncol(signvalues)])
      s<-data.table(t(s))
      if(nrow(all1)!=0){
        m<-find.matches(all1,s)
        if(sum(m$matches)!=0){
          all1<-all1[-which(m$matches==1),]
        }
      }
    }
  }
  return(all1=all1)
}



discoverpolarity<-function(causes,effect,threshold1, threshold2=NA, 
                           limits=NULL,
                          varImp=rep(1,ncol(causes)), threshold3=NA){
  
  if(is.na(threshold2)){threshold2<-threshold1}
  if(is.na(threshold3)){ threshold3<-threshold1}
  if(is.null(limits)){ limits<-t(sapply(1:ncol(causes),
                      function(x) c(Min=min(causes[,x]), Max=max(causes[,x]))))}
  #add maximum diff in observed effect values
  limits<-rbind(limits, c(min(effect),max(effect))) 
  effect<-data.frame(effect)
  causes<-data.frame(causes)
  causesvarnames<- colnames(causes)  #names of cause variables
    
 
  #set the variables which are out of the limits as limits
  causes<- data.frame(sapply(1:ncol(causes),
                             function(k) dealvaroutoflimits(causes[,k], limits[k,2], 
                                           limits[k,1])))
  causeseffect<-data.frame(cbind(causes,effect))
  if(length(which(causeseffect[,ncol(causeseffect)]==0))!=0){
    causeseffect<-causeseffect[-which(causeseffect[,ncol(causeseffect)]==0),]
  }
  #delete rows where z=0
  if(length(which(causeseffect[,ncol(causeseffect)]==0))>0){
    causeseffect<-causeseffect[-which(causeseffect[,ncol(causeseffect)]==0),]
  }
  #divide each variable to its limits
  causeseffect<-dividetolimits(causeseffect, limits)
  #multiply variable importances
  causeseffect<-varImpmultiplication(causeseffect, varImp)
  #take differences of each point
  diff_causeseffect <-diffofeveryrow(causeseffect)
  #create all the possible corners
  all1<-createcorners((ncol(causeseffect)-1))
  
  
  #APPLY FIRST PHASE
  #take signs of diffirences
  sign_diff_causeseffect<-sapply(3:ncol(diff_causeseffect)
                                 , function(x) as.numeric(sign(diff_causeseffect[,x]))) 
  sign_diff_causeseffect<-data.frame(cbind(diff_causeseffect[,1]
                                           ,diff_causeseffect[,2],sign_diff_causeseffect))
   #obtain overall observed corners
  a<-obtainallcorners(sign_diff_causeseffect)
  signvalues1<-a$signvalues
  perc1<-a$perc
  
  
  #eliminate corners
  all1<-eliminatecorners(signvalues1,all1)
  
  if(nrow(all1)<=1){
    #create result tables
    row_sub = apply(signvalues1, 1, function(row) all(row !=0 ))
    
    if(length(row_sub)==1){
      eliminated1proc<-data.frame(cbind(t(signvalues1[row_sub,1:(ncol(signvalues1)-1)]),perc1[row_sub]))
    }else{
      eliminated1proc<-data.frame(cbind(signvalues1[row_sub,1:(ncol(signvalues1)-1)],perc1[row_sub]))
    }
    colnames(eliminated1proc)<-c(causesvarnames, "NoofObservedDifferences")
    colnames(all1)<-causesvarnames
    
    if(nrow(all1)>0){rownames(all1)<-1:nrow(all1)}
    
    l1<-list(All1=all1,  EliminatedOptions1=eliminated1proc)
    return(l1)
    
  }else{#APPLY SECOND PHASE
    #find z=0 values and seperate it as a new set
    k<-seperatez0dataset(diff_causeseffect, 
                         threshold1, threshold3)
    diff_causeseffect<-k$diff_causeseffect
    z0<-k$z0
    datdz0<-k$datdz0
    
    #find candidate indexes 
    a<-findcandidateindexes(diff_causeseffect, threshold1)
    diff_causeseffect<-a$diff_causeseffect
    candlistindexes<-a$candlistindexes
    #set cause variables as 0
    s<-setcausevarzero(diff_causeseffect, candlistindexes,threshold1, threshold2)
    diff_causeseffect<-s$diff_causeseffect
    smallratios<-s$smallratios
    bigratios<-s$bigratios
    
    #take signs of diffirences
    sign_diff_causeseffect<-sapply(3:ncol(diff_causeseffect)
                                   , function(x) as.numeric(sign(diff_causeseffect[,x]))) 
    sign_diff_causeseffect<-data.frame(cbind(diff_causeseffect[,1]
                                             ,diff_causeseffect[,2],sign_diff_causeseffect))
    
    #add rows where z=0
    if(z0){
      sign_diff_causeseffect<-rbind(sign_diff_causeseffect, datdz0)
    }
    sign_diff_causeseffect<-data.frame(sign_diff_causeseffect)
    # #multiply rows with -1 where effect is -1
    # sign_diff_causeseffect[which(sign_diff_causeseffect[,ncol(sign_diff_causeseffect)]==-1)
    #     ,3:ncol(sign_diff_causeseffect)]<-sign_diff_causeseffect[which(sign_diff_causeseffect[,
    #             ncol(sign_diff_causeseffect)]==-1) ,3:ncol(sign_diff_causeseffect)]*(-1)
    # 
    
    #obtain overall observed corners
    a2<-obtainallcorners(sign_diff_causeseffect)
    signvalues2<-a2$signvalues
    perc2<-a2$perc
    
    #create all the possible corners
    all<-createcorners((ncol(causeseffect)-1))
    
    
    #eliminate corners
    all2<-eliminatecorners(signvalues2,all)
    
    #create result tables
    row_sub = apply(signvalues1, 1, function(row) all(row !=0 ))
    
    if(length(row_sub)==1){
      eliminated1proc<-data.frame(cbind(t(signvalues1[row_sub,1:(ncol(signvalues1)-1)]),perc1[row_sub]))
    }else{
      eliminated1proc<-data.frame(cbind(signvalues1[row_sub,1:(ncol(signvalues1)-1)],perc1[row_sub]))
    }
    colnames(eliminated1proc)<-c(causesvarnames, "NoofObservedDifferences")
    
    row_sub2 = apply(signvalues2, 1, function(row) all(row !=0 ))
   
    if(length(row_sub2)==1){
      eliminated2proc<-data.frame(cbind(t(signvalues2[row_sub2,1:(ncol(signvalues2)-1)]),perc2[row_sub2]))
    }else{
      eliminated2proc<-data.frame(cbind(signvalues2[row_sub2,1:(ncol(signvalues2)-1)],perc2[row_sub2]))
    }
    colnames(eliminated2proc)<-c(causesvarnames, "NoofObservedDifferences")
    colnames(all1)<-causesvarnames
    colnames(all2)<-causesvarnames
    
    if(nrow(all1)>0){rownames(all1)<-1:nrow(all1)}
    if(nrow(all2)>0){rownames(all2)<-1:nrow(all2)}
    
    l1<-list(All1=all1,  EliminatedOptions1=eliminated1proc,
             All2=all2,  EliminatedOptions2=eliminated2proc)
    return(l1)}
  
}

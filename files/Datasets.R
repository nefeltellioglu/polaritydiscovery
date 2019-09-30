library(ggplot2)
library(akima)
library(data.table)
library(ppcor)

#Please set your working directory with setwd() function.
#setwd(Working Directory)
source("Polarity Discovery Code.R")


#Dataset1
{
  
  set.seed(1)
  x1<-runif(3000,30,70)
  x2<-runif(3000,0,30)
  
  y<- x1*(100-100/(1+exp(-0.15*(x2-x2[1]))))
   datx1<-data.frame(x1=x1[1:1000], fx1=(x1)[1:1000])
  datx2<-data.frame(x2=x2[1:1000], fx2=(200-80/(1+exp(-0.1*(x2-x2[1]))))[1:1000])
  
  ggplot(datx1) + 
    aes(x = x1, y = fx1, group="l")+ geom_line(size=2)+
    theme_bw()+
    theme(plot.title=element_text(size=14,face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    labs(x="X1", y = "f1(X1)")+ 
    ggtitle("Effect of X1 on Y") +theme(plot.title = element_text(hjust = 0.5))
  
  ggplot(datx2) + theme_bw()+
    aes(x = x2, y = fx2, group="l")+ geom_line(size=2)+
    
    theme(plot.title=element_text(size=14,face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    labs(x="X2", y = "f2(X2)")+ 
    ggtitle("Effect of X2 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
   data1<-data.frame(x1,x2,y)
  fld <- with(data1, interp(x = x1, y =x2, z = y, duplicate = "mean"))
  gdat <- interp2xyz(fld, data.frame=TRUE)
  ggplot(gdat) + 
    aes(x = x, y = y, z = z, fill = z) + 
    geom_tile() + 
     geom_contour(color = "white", alpha = 1) + 
    scale_fill_distiller(palette="Reds", na.value="white", trans = "reverse") + 
    theme_bw()+
    theme(plot.title=element_text(size=14,face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14))+
    labs(x="X1", y = "X2")+ labs(fill = "Y")+
    ggtitle("Heatmap of Y") +theme(plot.title = element_text(hjust = 0.5))
  
  
  x1<- c(35,seq(45,50,length.out =5),seq(50.5,65,length.out =20) )
  
  x2<- c(25, seq(24,20,length.out =5),seq(12,5,length.out =20))
  cor(x1,x2)
  ind<-data.frame(cbind(x1,x2))
  y<- x1*(100-100/(1+exp(-0.15*(x2-x2[1])))) 

  data1<-data.frame(x1,x2,y)
  ggplot(data1, aes(x=x1, y=x2,  label = round(y,3)))  + theme_bw()+
    geom_point(aes(colour = y)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    scale_color_gradient(low="coral1", high="red4")+
    labs(x="X1", y = "X2") +
    ggtitle("Y Values") +theme(plot.title = element_text(size=14,hjust = 0.5, face = "bold"))+
    xlim(30,70)+ylim(0,30)
  
  c(sign(pcor.test(x1,y, c(x2), method="spearman")$estimate),
    sign(pcor.test(x2,y, c(x1), method="spearman")$estimate))
  c((pcor.test(x1,y, c(x2), method="spearman")$p.value ),
    (pcor.test(x2,y, c(x1), method="spearman")$p.value))
  c((pcor.test(x1,y, c(x2), method="spearman")$estimate),
    (pcor.test(x2,y, c(x1), method="spearman")$estimate))
 
  discoverpolarity(ind,y,threshold1=0.15) 
  
}

#Dataset2 
{
  
  #effect functions
  {
    plot(x1,((15/(1+exp(-0.4*(x1-x1[1]))))-2)/7.5)
    plot(x2,(x2/20+0.75))
    plot(x3, (14-(20/(1+exp(-0.1*(x3-x3[1])))))/13.77+0.3)
    
    x1<-runif(3000,min(x1),max(x1))
    x2<-runif(3000,min(x2),max(x2))
    x3<- runif(3000,min(x3),max(x3))
    
    y<- 100*(((15/(1+exp(-0.4*(x1-x1[1]))))-2)/7.5)*(x2/20+0.75)*((14-(20/(1+exp(-0.1*(x3-x3[1])))))/13.77+0.3)
    
    datx1<-data.frame(x1=x1[1:3000], fx1=(((15/(1+exp(-0.4*(x1-x1[1]))))-2)/7.5)[1:3000])
    datx2<-data.frame(x2=x2[1:3000], fx2=(x2/20+0.75)[1:3000])
    datx3<-data.frame(x3=x3[1:3000], fx3=((14-(20/(1+exp(-0.1*(x3-x3[1])))))/13.77+0.3 )[1:3000])
    
    
    ggplot(datx1) + 
      aes(x = x1, y = fx1, group="l")+ geom_line(size=2)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X1", y = "f1(X1)")+ 
      ggtitle("Effect of X1 on Y") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx2) + theme_bw()+
      aes(x = x2, y = fx2, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X2", y = "f2(X2)")+ 
      ggtitle("Effect of X2 on Y") +theme(plot.title = element_text(hjust = 0.5)) #+
    
    ggplot(datx3) + theme_bw()+
      aes(x = x3, y = fx3, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X3", y = "f3(X3)")+ 
      ggtitle("Effect of X3 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
    
  }
  x1<- c(seq(5,14,length.out =80), seq(13,7,length.out =10), seq(7,9,length.out =10))
  x2<- c(seq(11,4,length.out =50),seq(4,7,length.out =20),seq(7,1,length.out =30) )
  k<-c(seq(60,10,length.out =100) )
  x3<-50/(1+exp(-0.07*(k-k[1])))
  
  ind<-data.frame(cbind(x1,x2,x3))
  cor(ind)
  y<- 100*(((15/(1+exp(-0.4*(x1-x1[1]))))-2)/7.5)*(x2/20+0.75)*((14-(20/(1+exp(-0.1*(x3-x3[1])))))/13.77+0.3)
  all<-data.frame(cbind(x1,x2,x3,y))
  m<-as.matrix(cor(all, method=c("spearman")))
  m
  xtable(m, digit=4)
  
  #dataset plot
  {
    
    datx1<-data.frame(X1=x1, Y=y, X2=x2, X3=x3)
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X1)),y = X1, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X1 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X2)),y = X2, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X2 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X3)),y = X3, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X3 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(Y)),y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("Y Variable") +theme(plot.title = element_text(hjust = 0.5))
    
  }
  #dataset cross sectional plot
  {
    
    datx1<-data.frame(X1=x1, Y=y, X2=x2, X3=x3)
    
    ggplot(datx1) + 
      aes( x= X1,y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X1", y = "Y")+ 
      ggtitle("X1 vs Y") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= X2,y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X2", y = "Y")+ 
      ggtitle("X2 vs Y") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= X3,y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X3", y = "Y")+ 
      ggtitle("X3 vs Y") +theme(plot.title = element_text(hjust = 0.5))
    
  }
  limits<-t(sapply(1:ncol(ind), function(x) c(Min=min(ind[,x]), Max=max(ind[,x]))))
  limits[1,2]<-12.5
  
  discoverpolarity(causes=ind,effect =y,threshold1=0.05,limits=limits)
  
  
  
  c(sign(pcor.test(x1,y, c(x2,x3), method="spearman")$estimate),
    sign(pcor.test(x2,y, c(x1,x3), method="spearman")$estimate),
    sign(pcor.test(x3,y, c(x1,x2), method="spearman")$estimate) )
  c(pcor.test(x1,y, c(x2,x3), method="spearman")$estimate,
    pcor.test(x2,y, c(x1,x3), method="spearman")$estimate,
    pcor.test(x3,y, c(x1,x2), method="spearman")$estimate )
  c((pcor.test(x1,y, c(x2,x3), method="spearman")$p.value),
    (pcor.test(x2,y, c(x1,x3), method="spearman")$p.value),
    (pcor.test(x3,y, c(x1,x2), method="spearman")$p.value))
  
}

#Dataset3
{
  #effect functions
  {
    plot(x1,x1)
    plot(x2,100-100/(1+exp(-0.2*(x2-x2[1]))))
    plot(x3, x3)
    plot(x4, 60/(1+exp(-0.15*(x4-x4[1]))))
    set.seed(1)
    x1<-runif(3000,min(x1),max(x1))
    x2<-runif(3000,min(x2),max(x2))
    x3<- runif(3000,min(x3),max(x3))
    x3<- runif(3000,min(x4),max(x4))
    
    y<- x1+100-100/(1+exp(-0.2*(x2-x2[1])))+x3+60/(1+exp(-0.15*(x4-x4[1])))
    
    datx1<-data.frame(x1=x1[1:1000], fx1=(x1)[1:1000])
    datx2<-data.frame(x2=x2[1:1000], fx2=(100-100/(1+exp(-0.2*(x2-x2[1]))))[1:1000])
    datx3<-data.frame(x3=x3[1:1000], fx3=(x3)[1:1000])
    datx4<-data.frame(x4=x4[1:1000], fx4=(60/(1+exp(-0.15*(x4-x4[1]))))[1:1000])
    
    
    ggplot(datx1) + 
      aes(x = x1, y = fx1, group="l")+ geom_line(size=2)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X1", y = "f1(X1)")+ 
      ggtitle("Effect of X1 on Y") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx2) + theme_bw()+
      aes(x = x2, y = fx2, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X2", y = "f2(X2)")+ 
      ggtitle("Effect of X2 on Y") +theme(plot.title = element_text(hjust = 0.5)) #+
    
    ggplot(datx3) + theme_bw()+
      aes(x = x3, y = fx3, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X3", y = "f3(X3)")+ 
      ggtitle("Effect of X3 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
    
    ggplot(datx4) + theme_bw()+
      aes(x = x4, y = fx4, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X4", y = "f4(X4)")+ 
      ggtitle("Effect of X4 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
    
    
  }
  
  x1<- c(seq(70,90,length.out =10), seq(90,1,length.out =90))
 
  x2<- c(seq(31,44,length.out =50),seq(44,35,length.out =50) )
 
  k<-c(seq(120,30,length.out =40),seq(30,60,length.out =30),seq(59,30,by=-1) )
  x3<-90/(1+exp(-0.02*(k-k[1])))
 
  x4<- c(seq(1,30,length.out =40), seq(29,15,length.out =10),seq(16,70,length.out =50))
  x4<-60/(1+exp(-0.15*(x4-x4[1])))
   ind<-data.frame(cbind(x1,x2,x3,x4))
  cor(ind)
  y<- x1+100-100/(1+exp(-0.2*(x2-x2[1])))+x3+60/(1+exp(-0.15*(x4-x4[1])))
  #dataset plot
  {
     
    datx1<-data.frame(X1=x1, Y=y, X2=x2, X3=x3,X4=x4)
    
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X1)),y = X1, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X1 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X2)),y = X2, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X2 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X3)),y = X3, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X3 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X4)),y = X4, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X4 Variable") +theme(plot.title = element_text(hjust = 0.5))
    ggplot(datx1) + 
      aes( x= seq(1, length(Y)),y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("Y Variable") +theme(plot.title = element_text(hjust = 0.5))
    
  }
  limits<-t(sapply(1:ncol(ind), function(x) c(Min=min(ind[,x]), Max=max(ind[,x]))))
  limits[4,2]<-55
  discoverpolarity(causes=ind,effect =y,threshold1=0.05,
                   limits=limits)
  
  c(sign(pcor.test(x1,y, c(x4,x2,x3), method="spearman")$estimate),
    sign(pcor.test(x2,y, c(x1,x4,x3), method="spearman")$estimate),
    sign(pcor.test(x3,y, c(x1,x4,x2), method="spearman")$estimate),
    sign(pcor.test(x4,y, c(x1,x2,x3), method="spearman")$estimate))
  c(sign(pcor.test(x1,y, c(x4,x2,x3), method="spearman")$p.value),
    sign(pcor.test(x2,y, c(x1,x4,x3), method="spearman")$p.value),
    sign(pcor.test(x3,y, c(x1,x4,x2), method="spearman")$p.value),
    sign(pcor.test(x4,y, c(x1,x2,x3), method="spearman")$p.value))

  
}

#Dataset4
{
  
  #effect functions
  {
    plot(x1,100-x1)
    plot(x2,(160-100/(1+exp(-0.2*(x2-x2[1])))))
    plot(x3, 100-x3)
    plot(x4, 120/(1+exp(-0.1*(x4-x4[1]))))
    set.seed(1)
    x1<-runif(3000,min(x1),max(x1))
    x2<-runif(3000,min(x2),max(x2))
    x3<- runif(3000,min(x3),max(x3))
    x4<- runif(3000,min(x4),max(x4))
    
    y<- 100-x1+(160-100/(1+exp(-0.2*(x2-x2[1]))))+100-x3+120/(1+exp(-0.1*(x4-x4[1])))
    
    datx1<-data.frame(x1=x1[1:1000], fx1=(100-x1)[1:1000])
    datx2<-data.frame(x2=x2[1:1000], fx2=(160-100/(1+exp(-0.2*(x2-x2[1]))))[1:1000])
    datx3<-data.frame(x3=x3[1:1000], fx3=( 100-x3)[1:1000])
    datx4<-data.frame(x4=x4[1:1000], fx4=(120/(1+exp(-0.1*(x4-x4[1]))))[1:1000])
    
    
    ggplot(datx1) + 
      aes(x = x1, y = fx1, group="l")+ geom_line(size=2)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X1", y = "f1(X1)")+ 
      ggtitle("Effect of X1 on Y") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx2) + theme_bw()+
      aes(x = x2, y = fx2, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X2", y = "f2(X2)")+ 
      ggtitle("Effect of X2 on Y") +theme(plot.title = element_text(hjust = 0.5)) #+
    
    ggplot(datx3) + theme_bw()+
      aes(x = x3, y = fx3, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X3", y = "f3(X3)")+ 
      ggtitle("Effect of X3 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
    
    ggplot(datx4) + theme_bw()+
      aes(x = x4, y = fx4, group="l")+ geom_line(size=2)+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="X4", y = "f4(X4)")+ 
      ggtitle("Effect of X4 on Y") +theme(plot.title = element_text(hjust = 0.5)) 
    
    
  }
  x1<- c(seq(60,90,length.out =30), seq(90,1,length.out =70))
  x2<- c(seq(31,44,length.out =50),seq(44,35,length.out =50) )
  k<-c(seq(120,30,length.out =40),seq(30,60,length.out =30),seq(59,30,by=-1) )
  x3<-90/(1+exp(-0.02*(k-k[1])))
  
  x4<- c(seq(1,30,length.out =40), seq(29,15,length.out =10),seq(16,70,length.out =50))
  x4<-60/(1+exp(-0.15*(x4-x4[1])))
  ind<-data.frame(cbind(x1,x2,x3,x4))
  cor(ind)
  y<- 100-x1+(160-100/(1+exp(-0.2*(x2-x2[1]))))+100-x3+120/(1+exp(-0.1*(x4-x4[1])))
  #dataset plot
  {
    
    datx1<-data.frame(X1=x1, Y=y, X2=x2, X3=x3,X4=x4)
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X1)),y = X1, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X1 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X2)),y = X2, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X2 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X3)),y = X3, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X3 Variable") +theme(plot.title = element_text(hjust = 0.5))
    
    ggplot(datx1) + 
      aes( x= seq(1, length(X4)),y = X4, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("X4 Variable") +theme(plot.title = element_text(hjust = 0.5))
    ggplot(datx1) + 
      aes( x= seq(1, length(Y)),y = Y, group="l")+ geom_point(size=1)+
      theme_bw()+
      theme(plot.title=element_text(size=16,face="bold"),
            axis.text=element_text(size=14),
            axis.title=element_text(size=16))+
      labs(x="Time", y = "")+ 
      ggtitle("Y Variable") +theme(plot.title = element_text(hjust = 0.5))
    
  }
  limits<-t(sapply(1:ncol(ind), function(x) c(Min=min(ind[,x]), Max=max(ind[,x]))))
  limits[4,2]<-55
  limits[2,2]<-42
  discoverpolarity(causes=ind,effect =y,threshold1=0.15)
  
  
  c(sign(pcor.test(x1,y, c(x4,x2,x3), method="spearman")$estimate),
    sign(pcor.test(x2,y, c(x1,x4,x3), method="spearman")$estimate),
    sign(pcor.test(x3,y, c(x1,x4,x2), method="spearman")$estimate),
    sign(pcor.test(x4,y, c(x1,x2,x3), method="spearman")$estimate))
  c(sign(pcor.test(x1,y, c(x4,x2,x3), method="spearman")$p.value),
    sign(pcor.test(x2,y, c(x1,x4,x3), method="spearman")$p.value),
    sign(pcor.test(x3,y, c(x1,x4,x2), method="spearman")$p.value),
    sign(pcor.test(x4,y, c(x1,x2,x3), method="spearman")$p.value))
  
}



rm(list = ls())

geno_up2_east<-geno_up2[c(17:26,37:45,81:90,131:167),]
geno_up2_west<-geno_up2[-c(17:26,37:45,81:90,131:167),]

geno_up3_east1 <- c()
pos_east <- c()
for(i in 1:dim(geno_up2_east)[2]){
  snp_colnames<-colnames(geno_up2_east)[i]
  snp <- c(snp_colnames,geno_up2_east[,i])
  index <- names(table(snp))
  if(length(index)==4)
  geno_up3_east1 <- cbind(geno_up3_east1,snp)
  pos_east <- c(pos_east,i)
  
}

geno_up3_east<-geno_up3_east1[-1,]
colnames(geno_up3_east)<-geno_up3_east1[1,]

pvalue3_east <- c();
R1_east <- c();
lr_east <- c();
for(j in 1:dim(geno_up3_east)[2]){
  fp <- c();fq <- c()
  p=0.3;q=0.7
  
  snp <- geno_up3_east[,j]
  index <- names(table(snp))
  snp.n <- as.numeric(table(snp))
  lg <- length(snp.n)
  
  n00 <- snp.n[1]; n21 <-snp.n[2];n22 <- snp.n[3]; n <- n00+n21+n22
  
  for(i in 1:100){
    
    fai1= (56*p^7*q+168*p^6*q^2+280*p^5*q^3+280*p^4*q^4+168*p^3*q^5+56*p^2*q^6+8*p*q^7)/(8*p^7*q+28*p^6*q^2+56*p^5*q^3+70*p^4*q^4+56*p^3*q^5+28*p^2*q^6+8*p*q^7)
    fai2= (8*p^7*q+56*p^6*q^2+168*p^5*q^3+280*p^4*q^4+280*p^3*q^5+168*p^2*q^6+56*p*q^7)/(8*p^7*q+28*p^6*q^2+56*p^5*q^3+70*p^4*q^4+56*p^3*q^5+28*p^2*q^6+8*p*q^7)
    
    p=(8*n22+fai1*n21)/(8*n)
    q=(fai2*n21+8*n00)/(8*n)
    
    fp <- c(fp,p)
    fq <- c(fq,q)
    
    p <- p
    q <- q
  }
  p=fp[100];q=fq[100]
  
  R22=p^8;
  R21=8*(p^7)*q+28*(p^6)*(q^2)+56*(p^5)*(q^3)+70*(p^4)*(q^4)+56*(p^3)*(q^5)+28*(p^2)*(q^6)+8*p*(q^7)
  R00=q^8
  
  L1 <- n22*log(n22/n)+n21*log(n21/n)+n00*log(n00/n)
  L2 <- n22*log(R22)+n21*log(R21)+n00*log(R00)
  LR_east <- -2*(L2-L1)
  
  pv <-  pchisq(LR_east,1,lower.tail=F)
  pvalue3_east <- c(pvalue3_east,pv)
  
  lr_east<-c(lr_east,LR_east)
  R1_east <- rbind(R1_east,c(R22,R21,R00))
  
} 

colnames(R1_east) <- c("R22","R21","R00")

write.csv(pvalue3_east,file="Pvalue_east.csv")
write.csv(R1_east,file="Pgenotype_east.csv")
write.csv(lr_east,file="LR_east.csv")
write.csv(pos_east,file="pos_east.csv")

annot <- read.csv("annot.csv")
pvalue_east <- read.csv("Pvalue_east.csv")
lp.all_east <- -log10(pvalue_east[,2])
name_east <- colnames(geno_up3_east)

pos.up_east<- c()
for(j in 1:length(name_east)){
  
  pos1_east <- which(as.character(annot[,2])==name_east[j])
  pos.up_east <- c(pos.up_east,pos1_east)
  
}

lp.annot_east <- as.matrix(lp.all_east)[(1:length(pos.up_east)),]
lp.unannot_east<-as.matrix(lp.all_east)[-(1:length(pos.up_east)),]

annot.all_east <- annot[pos.up_east,]
snp.annot_east <- annot.all_east[,2]
snp.unannot_east <- rep(8136,8136)

chr.annot_east <- annot.all_east[,7]
chr.unannot_east <- rep(19,8136)

data.annot_east <- cbind(snp.annot_east,as.character(chr.annot_east),lp.annot_east)
data.unannot_east <- cbind(snp.unannot_east,chr.unannot_east,lp.unannot_east)

data.all_east <- cbind(c(snp.annot_east,snp.unannot_east),c(rep(1,1224),rep(2,1291),rep(3,1040),rep(4,818),rep(5,1624),rep(6,700),rep(7,864),rep(8,295),rep(9,1845),rep(10,1130),rep(11,1074),rep(12,524),rep(13,715),rep(14,1381),rep(15,702),rep(16,631),rep(17,241),rep(18,624),chr.unannot_east),c(lp.annot_east,lp.unannot_east))
data_east<- as.data.frame(data.all_east)
LR_east<-as.data.frame(data.all_east[,3])
lr_east<-as.matrix(LR_east)
threshold1_east <- -log10(0.05/length(lp.all_east))
colnames(data_east) <- c("SNP","CHR","LR")

over_east<-c()
for(m in 1:length(lp.all_east)){
  
  over1_east<-c(which(as.matrix(lp.all_east)[m,]>threshold1_east))
  over_east<-c(over_east,over1_east)
 
}

p <- ggplot(data_east, aes(x=1:24859,y=as.numeric(data_east[,3])),size=1) +theme_bw() +scale_y_continuous(expand = c(0, 0),limits = c(0,100))+
  scale_x_continuous(expand = c(0, 0),breaks = c(612.5,1870,3035.5,3964.5,5185.5,6374.5,7129.5,7709,8779,10266.5,11368.5,12167.5,12787, 13835,14876.5,15543,15979,16411.5,20791.5),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","Unannotated"))+
  geom_point(aes( col=as.factor(CHR)),size=0.7) +
  scale_color_manual(values = rep(c("skyblue2", "steelblue"), 22 )) +	
  labs(x = 'Chromosomes', y = '-log(P-value)', size = 'relative abundance (%)') +
  theme(panel.grid = element_blank(),  panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  theme(axis.title.x =element_text(size=19), axis.title.y=element_text(size=18),axis.text.x = element_text(color="black", size=11),axis.text.y = element_text(color="black",size=14))+
  
  guides(color = 'none')+expand_limits(x = 0, y = 0)+ggtitle(' ')+theme(plot.title = element_text(size=10,face = "bold",hjust =1,vjust=1))+
  geom_hline(yintercept = threshold1_east,colour="black",linetype="dashed",lwd=0.8)


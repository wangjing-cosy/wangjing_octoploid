rm(list = ls())
geno_up2_east<-geno_up2[c(17:26,37:45,81:90,131:167),]
geno_up2_west<-geno_up2[-c(17:26,37:45,81:90,131:167),]

geno_up3_west1 <- c()
pos_west <- c()
for(i in 1:dim(geno_up2_west)[2]){
  snp_colnames<-colnames(geno_up2_west)[i]
  snp <- c(snp_colnames,geno_up2_west[,i])
  index <- names(table(snp))
  if(length(index)==4)
    geno_up3_west1 <- cbind(geno_up3_west1,snp)
  pos_west <- c(pos_west,i)
  
}

geno_up3_west<-geno_up3_west1[-1,]
colnames(geno_up3_west)<-geno_up3_west1[1,]

pvalue3_west <- c();
R1_west <- c();
lr_west <- c();
for(j in 1:dim(geno_up3_west)[2]){
  fp <- c();fq <- c()
  p=0.3;q=0.7
  
  snp <- geno_up3_west[,j]
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
  LR_west <- -2*(L2-L1)
  
  pv <-  pchisq(LR_west,1,lower.tail=F)
  pvalue3_west <- c(pvalue3_west,pv)
  
  lr_west<-c(lr_west,LR_west)
  R1_west <- rbind(R1_west,c(R22,R21,R00))
  
} 

colnames(R1_west) <- c("R22","R21","R00")

write.csv(pvalue3_west,file="Pvalue_west.csv")
write.csv(R1_west,file="Pgenotype_west.csv")
write.csv(lr_west,file="LR_west.csv")
write.csv(pos_west,file="pos_west.csv")

annot <- read.csv("annot.csv")
pvalue_west <- read.csv("Pvalue_west.csv")
lp.all_west <- -log10(pvalue_west[,2])
name_west <- colnames(geno_up3_west)

pos.up_west<- c()
for(j in 1:length(name_west)){
  
  pos1_west <- which(as.character(annot[,2])==name_west[j])
  pos.up_west <- c(pos.up_west,pos1_west)
  
}

lp.annot_west <- as.matrix(lp.all_west)[(1:length(pos.up_west)),]
lp.unannot_west<-as.matrix(lp.all_west)[-(1:length(pos.up_west)),]

annot.all_west <- annot[pos.up_west,]
snp.annot_west <- annot.all_west[,2]
snp.unannot_west <- rep(7897,7897)

chr.annot_west <- annot.all_west[,7]
chr.unannot_west <- rep(19,7897)

data.annot_west <- cbind(snp.annot_west,as.character(chr.annot_west),lp.annot_west)
data.unannot_west <- cbind(snp.unannot_west,chr.unannot_west,lp.unannot_west)

data.all_west <- cbind(c(snp.annot_west,snp.unannot_west),c(rep(1,1155),rep(2,1431),rep(3,887),rep(4,677),rep(5,1215),rep(6,714),rep(7,856),rep(8,311),rep(9,1605),rep(10,1064),rep(11,1006),rep(12,632),rep(13,622),rep(14,1464),rep(15,536),rep(16,599),rep(17,270),rep(18,854),chr.unannot_west),c(lp.annot_west,lp.unannot_west))
data_west<- as.data.frame(data.all_west)
LR_west<-as.data.frame(data.all_west[,3])
lr_west<-as.matrix(LR_west)
threshold1_west <- -log10(0.05/length(lp.all_west))
colnames(data_west) <- c("SNP","CHR","LR")

over_west<-c()
for(m in 1:length(lp.all_west)){
  
  over1_west<-c(which(as.matrix(lp.all_west)[m,]>threshold1_west))
  over_west<-c(over_west,over1_west)
  
}

p <- ggplot(data_west, aes(x=1:23795,y=as.numeric(data_west[,3])),size=1) +theme_bw() +scale_y_continuous(expand = c(0, 0),limits = c(0,100))+
  scale_x_continuous(expand = c(0, 0),breaks = c(578,1871,3030,3812,4758,5722.5,6507.5,7091,8049,9383.5,10418.5,11237.5,11864.5, 12907.5,13907.5,14475,14909.5,15471.5,19846.5),labels = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","Unannotated"))+
  geom_point(aes( col=as.factor(CHR)),size=0.7) +
  scale_color_manual(values = rep(c("limegreen", "forestgreen"), 22 )) +	
  labs(x = 'Chromosomes', y = '-log(P-value)', size = 'relative abundance (%)') +
  theme(panel.grid = element_blank(),  panel.background = element_rect(fill = 'transparent'), legend.key = element_rect(fill = 'transparent')) +
  theme(axis.title.x =element_text(size=19), axis.title.y=element_text(size=18),axis.text.x = element_text(color="black", size=11),axis.text.y = element_text(color="black",size=14))+
  
  guides(color = 'none')+expand_limits(x = 0, y = 0)+ggtitle(' ')+theme(plot.title = element_text(size=10,face = "bold",hjust =1,vjust=1))+
  geom_hline(yintercept = threshold1_west,colour="black",linetype="dashed",lwd=0.8)

p

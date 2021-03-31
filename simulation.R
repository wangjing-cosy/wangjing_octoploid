
rm(list = ls())

P_gamete <- c()
for(i in 1:1000 ){
  p4=0.25; p3= 0.1; p2=0.2; p1=0.15; p0=0.3
  
  P2222= (p4)^2
  
  P0000= (p0)^2
  Phe=(2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0)
  
  prob <- c(P2222,Phe,P0000)
  
  index <- sample(c(1,2,3),1000,replace = T,prob = prob)
  
  NN <- rep(0,3)
  st <- table(index)
  NNn <- as.numeric(names(st))
  NN[NNn] <- as.numeric(st)
  n3 <- NN[1]; n2 <- NN[2]; n1 <- NN[3]
  N=n3+n2+n1
  
  fai1= (2*p4*p3+2*p4*p2+2*p4*p1+2*p4*p0)/((2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0))
  fai2= (2*p4*p3+2*p3^2+2*p3*p2+2*p3*p1+2*p3*p0)/((2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0))
  fai3= (2*p4*p2+2*p3*p2+2*p2^2+2*p2*p1+2*p2*p0)/((2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0))
  fai4= (2*p4*p1+2*p3*p1+2*p2*p1+2*p1^2+2*p1*p0)/((2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0))
  fai5= (2*p4*p0+2*p3*p0+2*p2*p0+2*p1*p0)/((2*p4*p3)+(2*p4*p2+(p3)^2)+(2*p4*p1+2*p3*p2)+(2*p4*p0+2*p3*p1+(p2)^2)+(2*p3*p0+2*p2*p1)+(2*p2*p0+(p1)^2)+(2*p1*p0))
  
  p4=(2*n3+fai1*n2)/(2*sum(N))
  p3=(fai2*n2)/(2*sum(N))
  p2=(fai3*n2)/(2*sum(N))
  p1=(fai4*n2)/(2*sum(N))
  p0=(fai5*n2+2*n1)/(2*sum(N))
  
  P_gamete <- rbind(P_gamete,c(p4,p3+p2+p1,p0))
}   
write.csv(P_gamete,"P_gamete.csv")

MP_gamete <- colMeans(P_gamete)

se4 <- sqrt(sum((P_gamete[,1]-MP_gamete[1])^2)/1000)

se0 <- sqrt(sum((P_gamete[,3]-MP_gamete[3])^2)/1000)

sehe <- sqrt(sum((P_gamete[,2]-MP_gamete[2])^2)/1000)


rm(list = ls())
P_allele <- c()
MP_allele<-c()
for(i in 1:1000 ){
  p=0.3;q=0.7
  
  P2222=p^8
  Phe=8*(p^7)*q+28*(p^6)*(q^2)+56*(p^5)*(q^3)+70*(p^4)*(q^4)+56*(p^3)*(q^5)+28*(p^2)*(q^6)+8*p*(q^7)
  P0000=q^8
  
  prob <- c(P2222,Phe,P0000)
  
  index <- sample(c(1,2,3),1000,replace = T,prob = prob)
  
  NN <- rep(0,3)
  st <- table(index)
  NNn <- as.numeric(names(st))
  NN[NNn] <- as.numeric(st)
  n3 <- NN[1]; n2 <- NN[2]; n1 <- NN[3]
  N=n3+n2+n1
  
  fai1= (56*p^7*q+168*p^6*q^2+280*p^5*q^3+280*p^4*q^4+168*p^3*q^5+56*p^2*q^6+8*p*q^7)/(8*p^7*q+28*p^6*q^2+56*p^5*q^3+70*p^4*q^4+56*p^3*q^5+28*p^2*q^6+8*p*q^7)
  fai2= (8*p^7*q+56*p^6*q^2+168*p^5*q^3+280*p^4*q^4+280*p^3*q^5+168*p^2*q^6+56*p*q^7)/(8*p^7*q+28*p^6*q^2+56*p^5*q^3+70*p^4*q^4+56*p^3*q^5+28*p^2*q^6+8*p*q^7)
  
  p=(8*n3+fai1*n2)/(8*sum(N))
  q=(fai2*n2+8*n1)/(8*sum(N))
  
  P_allele <- rbind(P_allele,c(p,q))

}   

write.csv(P_allele,"P_allele.csv")

MP_allele <- colMeans(P_allele)

se_p <- sqrt(sum((P_allele[,1]-MP_allele[1])^2)/1000)
se_q <- sqrt(sum((P_allele[,2]-MP_allele[2])^2)/1000)



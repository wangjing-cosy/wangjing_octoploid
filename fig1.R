rm(list = ls())

library(RColorBrewer)
library(mvtnorm)
y <- c()

for(j in 1:9){

times <- seq(0,20,1)
N <- length(times)

a <- numeric(N)
b <- numeric(N)
c <- numeric(N)
d <- numeric(N)
e <- numeric(N)
f <- numeric(N)
g <- numeric(N)
h <- numeric(N)
i <- numeric(N)


a[1] <- 0.1  
b[1] <- 0.1  
c[1] <- 0.15 
d[1] <- 0.1  
e[1] <- 0.2  
f[1] <- 0.1  
g[1] <- 0.05 
h[1] <- 0.1 
i[1] <- 0.1  

alpha=c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4)[j]

for(t in 1:(N-1)){
  
  a[t+1] <-( a[t]^2*1+2*a[t]*b[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*c[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*d[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*g[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*h[t]*(1/64*alpha^2)+0
             + b[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*b[t]*c[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*b[t]*e[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*b[t]*f[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+0
             + c[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*c[t]*e[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+0
             + d[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*d[t]*e[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*d[t]*h[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+0
             + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*e[t]*g[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*e[t]*h[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+0
             + f[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*f[t]*g[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*f[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+0
             + g[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*g[t]*h[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+0
             + h[t]^2*(1/4096*alpha^4)+0
             + i[t]^2*0 )
  
  
  b[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(3/8-1/16*alpha^2-5/16*alpha)+2*a[t]*c[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*d[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*g[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*h[t]*(1/16*alpha-1/16*alpha^2)+0
             + b[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*b[t]*c[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*b[t]*e[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+0
             + c[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+0
             + d[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+0
             + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*e[t]*h[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+0
             + f[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*f[t]*h[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+0
             + g[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*g[t]*h[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+0
             + h[t]^2*(-1/512*alpha^4+1/512*alpha^3)+0
             + i[t]^2*0 )
  
  
  c[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16+3/32*alpha^2+1/16*alpha)+2*a[t]*c[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*d[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*e[t]*(41/98+24/49*alpha^2-20/49*alpha)+2*a[t]*f[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*g[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*h[t]*(1/16+3/32*alpha^2+1/16*alpha)+0
             + b[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*b[t]*c[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*b[t]*e[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*b[t]*g[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+0
             + c[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*c[t]*e[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+0
             + d[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*d[t]*e[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+0
             + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*e[t]*g[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*e[t]*h[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+0
             + f[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*f[t]*g[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*f[t]*h[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+0
             + g[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*g[t]*h[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+0
             + h[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+0
             + i[t]^2*0 )
  
  
  d[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16*alpha-1/16*alpha^2)+2*a[t]*c[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*d[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*g[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*h[t]*(3/8-1/16*alpha^2-5/16*alpha)+0
             + b[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*b[t]*c[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*b[t]*e[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+0
             + c[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*c[t]*e[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+0
             + d[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+0
             + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*e[t]*g[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*e[t]*h[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+0
             + f[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*f[t]*h[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+0
             + g[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*g[t]*h[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+0
             + h[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+0
             + i[t]^2*0 ) 
  
  
  e[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*(1/64*alpha^2)+2*a[t]*c[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*d[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*g[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*h[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*i[t]*1
              + b[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*b[t]*c[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*b[t]*d[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*b[t]*e[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*b[t]*f[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*b[t]*g[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*b[t]*h[t]*((35*alpha^4)/2048+(25*alpha^3)/512+(63*alpha^2)/512-alpha/64+59/128)+2*b[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
              + c[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*c[t]*d[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*c[t]*e[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*c[t]*f[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*c[t]*g[t]*((405*alpha^4)/2744+(747*alpha^2)/5488-(57*alpha)/343+15077/43904)+2*c[t]*h[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*c[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
              + d[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*d[t]*e[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*d[t]*f[t]*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(40725*alpha^2)/175616-(345*alpha)/1568+13529/43904)+2*d[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*d[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*d[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
              + e[t]^2*((160*alpha^4)/343-(160*alpha^3)/343+(108*alpha^2)/343-(82*alpha)/343+821/2744)+2*e[t]*f[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*e[t]*g[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*e[t]*h[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
              + f[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*f[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*f[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*f[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
              + g[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*g[t]*h[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*g[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
              + h[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*h[t]*i[t]*(1/64*alpha^2)
              + i[t]^2*0 )                                                                                                   
  
  
  f[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
              + b[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+2*b[t]*c[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+2*b[t]*e[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+2*b[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
              + c[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*c[t]*e[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*c[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
              + d[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*d[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
              + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*e[t]*g[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*e[t]*h[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
              + f[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*f[t]*h[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*f[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
              + g[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*g[t]*h[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*g[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
              + h[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*h[t]*i[t]*(1/16*alpha-1/16*alpha^2)
              + i[t]^2*0 )
  
  
  g[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
              + b[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+2*b[t]*c[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+2*b[t]*e[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+2*b[t]*g[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+2*b[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
              + c[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*c[t]*e[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*c[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
              + d[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*d[t]*e[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*d[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
              + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*e[t]*g[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*e[t]*h[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*e[t]*i[t]*(41/98+24/49*alpha^2-20/49*alpha)
              + f[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*f[t]*g[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*f[t]*h[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*f[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
              + g[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*g[t]*h[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*g[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
              + h[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*h[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
              + i[t]^2*0 )
  
  
  h[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
              + b[t]^2*(-1/512*alpha^4+1/512*alpha^3)+2*b[t]*c[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+2*b[t]*e[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+2*b[t]*i[t]*(1/16*alpha-1/16*alpha^2)
              + c[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*c[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
              + d[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*d[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
              + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*e[t]*h[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
              + f[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*f[t]*h[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*f[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
              + g[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*g[t]*h[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*g[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
              + h[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*h[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
              + i[t]^2*0 )
  
  
  i[t+1] <-( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
             + b[t]^2*(1/4096*alpha^4)+2*b[t]*c[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+2*b[t]*d[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+2*b[t]*e[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+2*b[t]*f[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+2*b[t]*i[t]*(1/64*alpha^2)
             + c[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*c[t]*e[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*c[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
             + d[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*d[t]*e[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*d[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*d[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
             + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*e[t]*g[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*e[t]*h[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
             + f[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*f[t]*g[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*f[t]*h[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*f[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
             + g[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*g[t]*h[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*g[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
             + h[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*h[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
             + i[t]^2*1 )
  
  
  
  }

  y <- rbind(y,rbind(a,b,c,d,e,f,g,h,i))
 
 
}

write.csv(y,"y.csv")

colors_homozygote <- colorRampPalette(c("snow", "mediumpurple3"))(18)
colors_heterozygote <- colorRampPalette(c("snow", "gold3"))(18)

par(mfcol=c(3,3),mar=c(0,0,0,0),oma=c(3,3,3,3))

  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  
  axis(2, las = 1,seq(0,0.25,0.05), seq(0,0.25,0.05),cex.axis = 1.2,mgp = c(0.3, 0.7,0))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[1,],type = "l",lwd=4,col=colors_homozygote[2])
  lines(times,y[10,],type = "l",lwd=4,col=colors_homozygote[4])
  lines(times,y[19,],type = "l",lwd=4,col=colors_homozygote[6])
  lines(times,y[28,],type = "l",lwd=4,col=colors_homozygote[8])
  lines(times,y[37,],type = "l",lwd=4,col=colors_homozygote[10])
  lines(times,y[46,],type = "l",lwd=4,col=colors_homozygote[12])
  lines(times,y[55,],type = "l",lwd=4,col=colors_homozygote[14])
  lines(times,y[64,],type = "l",lwd=4,col=colors_homozygote[16])
  lines(times,y[73,],type = "l",lwd=4,col=colors_homozygote[18])
  
  
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  
  axis(2, las = 1,seq(0,0.25,0.05), seq(0,0.25,0.05),cex.axis = 1.2,mgp = c(0.3, 0.7,0))
  
 
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[4,],type = "l",lwd=4,col=colors_heterozygote[2] )
  lines(times,y[13,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[22,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[31,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[40,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[49,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[58,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[67,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[76,],type = "l",lwd=4,col=colors_heterozygote[18])
  
  
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  axis(1, las = 1,seq(0,20,5), seq(0,20,5),cex.axis = 1.2,mgp = c(0.1, 0.9,0))
  axis(2, las = 1,seq(0,0.25,0.05), seq(0,0.25,0.05),cex.axis = 1.2,mgp = c(0.3, 0.7,0))
  
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[7,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[16,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[25,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[34,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[43,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[52,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[61,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[70,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[79,],type = "l",lwd=4,col=colors_heterozygote[18]) 
  
   
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[2,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[11,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[20,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[29,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[38,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[47,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[56,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[65,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[74,],type = "l",lwd=4,col=colors_heterozygote[18])

  
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[5,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[14,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[23,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[32,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[41,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[50,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[59,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[68,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[77,],type = "l",lwd=4,col=colors_heterozygote[18])
  
  
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  axis(1, las = 1,seq(0,20,5), seq(0,20,5),cex.axis = 1.2,mgp = c(0.1, 0.9,0))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[8,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[17,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[26,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[35,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[44,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[53,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[62,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[71,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[80,],type = "l",lwd=4,col=colors_heterozygote[18])   
  

  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[3,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[12,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[21,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[30,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[39,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[48,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[57,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[66,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[75,],type = "l",lwd=4,col=colors_heterozygote[18])
  

  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[6,],type = "l",lwd=4,col=colors_heterozygote[2])
  lines(times,y[15,],type = "l",lwd=4,col=colors_heterozygote[4])
  lines(times,y[24,],type = "l",lwd=4,col=colors_heterozygote[6])
  lines(times,y[33,],type = "l",lwd=4,col=colors_heterozygote[8])
  lines(times,y[42,],type = "l",lwd=4,col=colors_heterozygote[10])
  lines(times,y[51,],type = "l",lwd=4,col=colors_heterozygote[12])
  lines(times,y[60,],type = "l",lwd=4,col=colors_heterozygote[14])
  lines(times,y[69,],type = "l",lwd=4,col=colors_heterozygote[16])
  lines(times,y[78,],type = "l",lwd=4,col=colors_heterozygote[18])
  

  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,0.25))
  
  axis(1, las = 1,seq(0,20,5), seq(0,20,5),cex.axis = 1.2,mgp = c(0.1, 0.9,0))
  mycolors<-brewer.pal(9, "Oranges")[1:9]
  lines(times,y[9,],type = "l",lwd=4,col=colors_homozygote[2])
  lines(times,y[18,],type = "l",lwd=4,col=colors_homozygote[4])
  lines(times,y[27,],type = "l",lwd=4,col=colors_homozygote[6])
  lines(times,y[36,],type = "l",lwd=4,col=colors_homozygote[8])
  lines(times,y[45,],type = "l",lwd=4,col=colors_homozygote[10])
  lines(times,y[54,],type = "l",lwd=4,col=colors_homozygote[12])
  lines(times,y[63,],type = "l",lwd=4,col=colors_homozygote[14])
  lines(times,y[72,],type = "l",lwd=4,col=colors_homozygote[16])
  lines(times,y[81,],type = "l",lwd=4,col=colors_homozygote[18])    
  

  homozygote<-matrix(0,nrow=9,ncol=21,byrow=T)  
  heterozygote<-matrix(0,nrow=9,ncol=21,byrow=T)
  
  for (m in 1:9) {
  
  homozygote[m,] <- y[9*m-8,]+y[9*m,]
  heterozygote[m,] <-y[9*m-7,]+y[9*m-6,]+y[9*m-5,]+y[9*m-4,]+y[9*m-3,]+y[9*m-2,]+y[9*m-1,]

  
  }
  
  
  par(mar=c(2.5,2.5,1,0.8))
 
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,1))
  
  axis(1, las = 1,seq(0,20,5), seq(0,20,5),cex.axis = 1.5,mgp = c(0.1, 1,0))
  axis(2, las = 1,seq(0,1,0.2), seq(0,1,0.2),cex.axis = 1.5,mgp = c(0.3, 0.7,0))  
  
  lines(times,homozygote[1,],type = "l",lwd=6,col=colors_homozygote[2])
  lines(times,homozygote[2,],type = "l",lwd=6,col=colors_homozygote[4] )
  lines(times,homozygote[3,],type = "l",lwd=6,col=colors_homozygote[6])
  lines(times,homozygote[4,],type = "l",lwd=6,col=colors_homozygote[8])
  lines(times,homozygote[5,],type = "l",lwd=6,col=colors_homozygote[10])
  lines(times,homozygote[6,],type = "l",lwd=6,col=colors_homozygote[12] )
  lines(times,homozygote[7,],type = "l",lwd=6,col=colors_homozygote[14])
  lines(times,homozygote[8,],type = "l",lwd=6,col=colors_homozygote[16])
  lines(times,homozygote[9,],type = "l",lwd=6,col=colors_homozygote[18])

  lines(times,heterozygote[1,],type = "l",lwd=6,col=colors_heterozygote[2])
  lines(times,heterozygote[2,],type = "l",lwd=6,col=colors_heterozygote[4] )
  lines(times,heterozygote[3,],type = "l",lwd=6,col=colors_heterozygote[6]  )
  lines(times,heterozygote[4,],type = "l",lwd=6,col=colors_heterozygote[8])
  lines(times,heterozygote[5,],type = "l",lwd=6,col=colors_heterozygote[10])
  lines(times,heterozygote[6,],type = "l",lwd=6,col=colors_heterozygote[12])
  lines(times,heterozygote[7,],type = "l",lwd=6,col=colors_heterozygote[14] )
  lines(times,heterozygote[8,],type = "l",lwd=6,col=colors_heterozygote[16] )
  lines(times,heterozygote[9,],type = "l",lwd=6,col=colors_heterozygote[18])
  
  rm(list = ls())
  setwd("E:/CCB/mine/多倍体/8octoploid")
  library(RColorBrewer)
  library(mvtnorm)
  y <- c()
  
  for(j in 1:9){
    
    times <- seq(0,20,1)
    N <- length(times)
    
    a <- numeric(N)
    b <- numeric(N)
    c <- numeric(N)
    d <- numeric(N)
    e <- numeric(N)
    f <- numeric(N)
    g <- numeric(N)
    h <- numeric(N)
    i <- numeric(N)
    
    
    a[1] <- 0.45  
    b[1] <- 0.001  
    c[1] <- 0.002 
    d[1] <- 0.002  
    e[1] <- 0.002 
    f[1] <- 0.001  
    g[1] <- 0.001
    h[1] <- 0.001
    i[1] <- 0.54
    
    alpha=c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4)[j]
    
    for(t in 1:(N-1)){
      
      a[t+1] <-( a[t]^2*1+2*a[t]*b[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*c[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*d[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*g[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*h[t]*(1/64*alpha^2)+0
                 + b[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*b[t]*c[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*b[t]*e[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*b[t]*f[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+0
                 + c[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*c[t]*e[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+0
                 + d[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*d[t]*e[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*d[t]*h[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+0
                 + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*e[t]*g[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*e[t]*h[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+0
                 + f[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*f[t]*g[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*f[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+0
                 + g[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*g[t]*h[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+0
                 + h[t]^2*(1/4096*alpha^4)+0
                 + i[t]^2*0 )
      
      
      b[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(3/8-1/16*alpha^2-5/16*alpha)+2*a[t]*c[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*d[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*g[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*h[t]*(1/16*alpha-1/16*alpha^2)+0
                 + b[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*b[t]*c[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*b[t]*e[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+0
                 + c[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+0
                 + d[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+0
                 + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*e[t]*h[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+0
                 + f[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*f[t]*h[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+0
                 + g[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*g[t]*h[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+0
                 + h[t]^2*(-1/512*alpha^4+1/512*alpha^3)+0
                 + i[t]^2*0 )
      
      
      c[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16+3/32*alpha^2+1/16*alpha)+2*a[t]*c[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*d[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*e[t]*(41/98+24/49*alpha^2-20/49*alpha)+2*a[t]*f[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*g[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*h[t]*(1/16+3/32*alpha^2+1/16*alpha)+0
                 + b[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*b[t]*c[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*b[t]*e[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*b[t]*g[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+0
                 + c[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*c[t]*e[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+0
                 + d[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*d[t]*e[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+0
                 + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*e[t]*g[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*e[t]*h[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+0
                 + f[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*f[t]*g[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*f[t]*h[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+0
                 + g[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*g[t]*h[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+0
                 + h[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+0
                 + i[t]^2*0 )
      
      
      d[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16*alpha-1/16*alpha^2)+2*a[t]*c[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*d[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*g[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*h[t]*(3/8-1/16*alpha^2-5/16*alpha)+0
                 + b[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*b[t]*c[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*b[t]*e[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+0
                 + c[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*c[t]*e[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+0
                 + d[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+0
                 + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*e[t]*g[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*e[t]*h[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+0
                 + f[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*f[t]*h[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+0
                 + g[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*g[t]*h[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+0
                 + h[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+0
                 + i[t]^2*0 ) 
      
      
      e[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*(1/64*alpha^2)+2*a[t]*c[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*d[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*g[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*h[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*i[t]*1
                  + b[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*b[t]*c[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*b[t]*d[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*b[t]*e[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*b[t]*f[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*b[t]*g[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*b[t]*h[t]*((35*alpha^4)/2048+(25*alpha^3)/512+(63*alpha^2)/512-alpha/64+59/128)+2*b[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
                  + c[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*c[t]*d[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*c[t]*e[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*c[t]*f[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*c[t]*g[t]*((405*alpha^4)/2744+(747*alpha^2)/5488-(57*alpha)/343+15077/43904)+2*c[t]*h[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*c[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
                  + d[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*d[t]*e[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*d[t]*f[t]*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(40725*alpha^2)/175616-(345*alpha)/1568+13529/43904)+2*d[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*d[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*d[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
                  + e[t]^2*((160*alpha^4)/343-(160*alpha^3)/343+(108*alpha^2)/343-(82*alpha)/343+821/2744)+2*e[t]*f[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*e[t]*g[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*e[t]*h[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
                  + f[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*f[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*f[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*f[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
                  + g[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*g[t]*h[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*g[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
                  + h[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*h[t]*i[t]*(1/64*alpha^2)
                  + i[t]^2*0 )                                                                                                   
      
      
      f[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+2*b[t]*c[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+2*b[t]*e[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+2*b[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
                  + c[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*c[t]*e[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*c[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
                  + d[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*d[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
                  + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*e[t]*g[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*e[t]*h[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
                  + f[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*f[t]*h[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*f[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
                  + g[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*g[t]*h[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*g[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
                  + h[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*h[t]*i[t]*(1/16*alpha-1/16*alpha^2)
                  + i[t]^2*0 )
      
      
      g[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+2*b[t]*c[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+2*b[t]*e[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+2*b[t]*g[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+2*b[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
                  + c[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*c[t]*e[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*c[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
                  + d[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*d[t]*e[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*d[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
                  + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*e[t]*g[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*e[t]*h[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*e[t]*i[t]*(41/98+24/49*alpha^2-20/49*alpha)
                  + f[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*f[t]*g[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*f[t]*h[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*f[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
                  + g[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*g[t]*h[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*g[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
                  + h[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*h[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
                  + i[t]^2*0 )
      
      
      h[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(-1/512*alpha^4+1/512*alpha^3)+2*b[t]*c[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+2*b[t]*e[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+2*b[t]*i[t]*(1/16*alpha-1/16*alpha^2)
                  + c[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*c[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
                  + d[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*d[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
                  + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*e[t]*h[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
                  + f[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*f[t]*h[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*f[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
                  + g[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*g[t]*h[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*g[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
                  + h[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*h[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
                  + i[t]^2*0 )
      
      
      i[t+1] <-( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                 + b[t]^2*(1/4096*alpha^4)+2*b[t]*c[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+2*b[t]*d[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+2*b[t]*e[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+2*b[t]*f[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+2*b[t]*i[t]*(1/64*alpha^2)
                 + c[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*c[t]*e[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*c[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
                 + d[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*d[t]*e[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*d[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*d[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
                 + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*e[t]*g[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*e[t]*h[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
                 + f[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*f[t]*g[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*f[t]*h[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*f[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
                 + g[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*g[t]*h[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*g[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
                 + h[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*h[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
                 + i[t]^2*1 )
      
      
      
    }
    
    y <- rbind(y,rbind(a,b,c,d,e,f,g,h,i))
    
    
  }
  
  write.csv(y,"y.csv")
  
  homozygote<-matrix(0,nrow=9,ncol=21,byrow=T)  
  heterozygote<-matrix(0,nrow=9,ncol=21,byrow=T)
  
  for (m in 1:9) {
    
    homozygote[m,] <- y[9*m-8,]+y[9*m,]
    heterozygote[m,] <-y[9*m-7,]+y[9*m-6,]+y[9*m-5,]+y[9*m-4,]+y[9*m-3,]+y[9*m-2,]+y[9*m-1,]
    
    
  }
  
  colors_homozygote <- colorRampPalette(c("snow", "mediumpurple3"))(18)
  colors_heterozygote <- colorRampPalette(c("snow", "gold3"))(18)
  
  
  par(mar=c(2.5,2.5,1,0.8))
  
  plot(0,0,type="l",xlab=" ",ylab=" ",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",xlim=c(0,20),ylim=c(0,1))
  
  axis(1, las = 1,seq(0,20,5), seq(0,20,5),cex.axis = 1.5,mgp = c(0.1, 1,0))
  axis(2, las = 1,seq(0,1,0.2), seq(0,1,0.2),cex.axis = 1.5,mgp = c(0.3, 0.7,0))  
  
  lines(times,homozygote[1,],type = "l",lwd=6,col=colors_homozygote[2])
  lines(times,homozygote[2,],type = "l",lwd=6,col=colors_homozygote[4] )
  lines(times,homozygote[3,],type = "l",lwd=6,col=colors_homozygote[6])
  lines(times,homozygote[4,],type = "l",lwd=6,col=colors_homozygote[8])
  lines(times,homozygote[5,],type = "l",lwd=6,col=colors_homozygote[10])
  lines(times,homozygote[6,],type = "l",lwd=6,col=colors_homozygote[12] )
  lines(times,homozygote[7,],type = "l",lwd=6,col=colors_homozygote[14])
  lines(times,homozygote[8,],type = "l",lwd=6,col=colors_homozygote[16])
  lines(times,homozygote[9,],type = "l",lwd=6,col=colors_homozygote[18])
  
  lines(times,heterozygote[1,],type = "l",lwd=6,col=colors_heterozygote[2])
  lines(times,heterozygote[2,],type = "l",lwd=6,col=colors_heterozygote[4])
  lines(times,heterozygote[3,],type = "l",lwd=6,col=colors_heterozygote[6])
  lines(times,heterozygote[4,],type = "l",lwd=6,col=colors_heterozygote[8])
  lines(times,heterozygote[5,],type = "l",lwd=6,col=colors_heterozygote[10])
  lines(times,heterozygote[6,],type = "l",lwd=6,col=colors_heterozygote[12])
  lines(times,heterozygote[7,],type = "l",lwd=6,col=colors_heterozygote[14])
  lines(times,heterozygote[8,],type = "l",lwd=6,col=colors_heterozygote[16])
  lines(times,heterozygote[9,],type = "l",lwd=6,col=colors_heterozygote[18])
  
  rm(list = ls())
  setwd("E:/CCB/mine/多倍体/8octoploid")
  library(RColorBrewer)
  library(mvtnorm)
  y <- c()
  
  for(j in 1:9){
    
    times <- seq(0,20,1)
    N <- length(times)
    
    a <- numeric(N)
    b <- numeric(N)
    c <- numeric(N)
    d <- numeric(N)
    e <- numeric(N)
    f <- numeric(N)
    g <- numeric(N)
    h <- numeric(N)
    i <- numeric(N)
    
    
    a[1] <- 0.1  
    b[1] <- 0.1  
    c[1] <- 0.15 
    d[1] <- 0.1  
    e[1] <- 0.2 
    f[1] <- 0.1  
    g[1] <- 0.05 
    h[1] <- 0.1 
    i[1] <- 0.1  
    
    alpha=c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4)[j]
    
    for(t in 1:(N-1)){
      
      a[t+1] <-( a[t]^2*1+2*a[t]*b[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*c[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*d[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*g[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*h[t]*(1/64*alpha^2)+0
                 + b[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*b[t]*c[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*b[t]*e[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*b[t]*f[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+0
                 + c[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*c[t]*e[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+0
                 + d[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*d[t]*e[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*d[t]*h[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+0
                 + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*e[t]*g[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*e[t]*h[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+0
                 + f[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*f[t]*g[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*f[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+0
                 + g[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*g[t]*h[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+0
                 + h[t]^2*(1/4096*alpha^4)+0
                 + i[t]^2*0 )
      
      
      b[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(3/8-1/16*alpha^2-5/16*alpha)+2*a[t]*c[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*d[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*g[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*h[t]*(1/16*alpha-1/16*alpha^2)+0
                 + b[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*b[t]*c[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*b[t]*e[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+0
                 + c[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+0
                 + d[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+0
                 + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*e[t]*h[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+0
                 + f[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*f[t]*h[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+0
                 + g[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*g[t]*h[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+0
                 + h[t]^2*(-1/512*alpha^4+1/512*alpha^3)+0
                 + i[t]^2*0 )
      
      
      c[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16+3/32*alpha^2+1/16*alpha)+2*a[t]*c[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*d[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*e[t]*(41/98+24/49*alpha^2-20/49*alpha)+2*a[t]*f[t]*(285/784+675/1568*alpha^2-255/784*alpha)+2*a[t]*g[t]*(87/392+27/98*alpha^2-6/49*alpha)+2*a[t]*h[t]*(1/16+3/32*alpha^2+1/16*alpha)+0
                 + b[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*b[t]*c[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*b[t]*e[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*b[t]*g[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+0
                 + c[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*c[t]*e[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+0
                 + d[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*d[t]*e[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+0
                 + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*e[t]*g[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*e[t]*h[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+0
                 + f[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*f[t]*g[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*f[t]*h[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+0
                 + g[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*g[t]*h[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+0
                 + h[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+0
                 + i[t]^2*0 )
      
      
      d[t+1] <-( a[t]^2*0+2*a[t]*b[t]*(1/16*alpha-1/16*alpha^2)+2*a[t]*c[t]*(3/98-9/49*alpha^2+15/98*alpha)+2*a[t]*d[t]*(45/392-225/784*alpha^2+135/784*alpha)+2*a[t]*e[t]*(12/49-16/49*alpha^2+4/49*alpha)+2*a[t]*f[t]*(75/196-225/784*alpha^2-75/784*alpha)+2*a[t]*g[t]*(45/98-9/49*alpha^2-27/98*alpha)+2*a[t]*h[t]*(3/8-1/16*alpha^2-5/16*alpha)+0
                 + b[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*b[t]*c[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*b[t]*e[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+0
                 + c[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*c[t]*e[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+0
                 + d[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+0
                 + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*e[t]*g[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*e[t]*h[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+0
                 + f[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*f[t]*h[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+0
                 + g[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*g[t]*h[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+0
                 + h[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+0
                 + i[t]^2*0 ) 
      
      
      e[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*(1/64*alpha^2)+2*a[t]*c[t]*(1/784+9/196*alpha^2+3/196*alpha)+2*a[t]*d[t]*(9/784+225/3136*alpha^2+45/784*alpha)+2*a[t]*e[t]*(9/196+4/49*alpha^2+6/49*alpha)+2*a[t]*f[t]*(25/196+225/3136*alpha^2+75/392*alpha)+2*a[t]*g[t]*(225/784+9/196*alpha^2+45/196*alpha)+2*a[t]*h[t]*(9/16+1/64*alpha^2+3/16*alpha)+2*a[t]*i[t]*1
                  + b[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*b[t]*c[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*b[t]*d[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*b[t]*e[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*b[t]*f[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*b[t]*g[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*b[t]*h[t]*((35*alpha^4)/2048+(25*alpha^3)/512+(63*alpha^2)/512-alpha/64+59/128)+2*b[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
                  + c[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*c[t]*d[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*c[t]*e[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*c[t]*f[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*c[t]*g[t]*((405*alpha^4)/2744+(747*alpha^2)/5488-(57*alpha)/343+15077/43904)+2*c[t]*h[t]*((45*alpha^4)/896+(225*alpha^3)/3136+(2507*alpha^2)/25088-(699*alpha)/12544+4359/12544)+2*c[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
                  + d[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*d[t]*e[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*d[t]*f[t]*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(40725*alpha^2)/175616-(345*alpha)/1568+13529/43904)+2*d[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952+(22509*alpha^2)/175616-(13941*alpha)/87808+26037/87808)+2*d[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088+(527*alpha^2)/25088-(45*alpha)/3136+2985/12544)+2*d[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
                  + e[t]^2*((160*alpha^4)/343-(160*alpha^3)/343+(108*alpha^2)/343-(82*alpha)/343+821/2744)+2*e[t]*f[t]*((1125*alpha^4)/2744-(2025*alpha^3)/5488+(10649*alpha^2)/43904-(4317*alpha)/21952+879/3136)+2*e[t]*g[t]*((90*alpha^4)/343-(45*alpha^3)/343+(197*alpha^2)/2744-(117*alpha)/1372+2481/10976)+2*e[t]*h[t]*((5*alpha^4)/56+(65*alpha^3)/784-(369*alpha^2)/6272+(149*alpha)/3136+451/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
                  + f[t]^2*((253125*alpha^4)/702464-(50625*alpha^3)/175616+(24975*alpha^2)/175616-(675*alpha)/6272+19575/87808)+2*f[t]*g[t]*((10125*alpha^4)/43904-(2025*alpha^3)/21952-(2691*alpha^2)/175616+(675*alpha)/87808+13045/87808)+2*f[t]*h[t]*((1125*alpha^4)/14336+(2025*alpha^3)/25088-(2623*alpha^2)/25088+(561*alpha)/6272+453/6272)+2*f[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
                  + g[t]^2*((405*alpha^4)/2744-(513*alpha^2)/5488+(27*alpha)/343+3429/43904)+2*g[t]*h[t]*(327/12544+45/896*alpha^4+225/3136*alpha^3-2533/25088*alpha^2+1149/12544*alpha)+2*g[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
                  + h[t]^2*(1/256+35/2048*alpha^4+25/512*alpha^3-27/512*alpha^2+7/128*alpha)+2*h[t]*i[t]*(1/64*alpha^2)
                  + i[t]^2*0 )                                                                                                   
      
      
      f[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(-7/512*alpha^4-3/512*alpha^3+3/256*alpha^2+1/128*alpha)+2*b[t]*c[t]*(15/6272-9/224*alpha^4+51/6272*alpha^3-13/12544*alpha^2+55/1792*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/3584)+(225*alpha^3)/12544-(633*alpha^2)/25088+(15*alpha)/256+9/784)+2*b[t]*e[t]*((-alpha^4/14)+(9*alpha^3)/784-(159*alpha^2)/3136+(5*alpha)/64+51/1568)+2*b[t]*f[t]*((-(225*alpha^4)/3584)-(285*alpha^3)/25088-(55*alpha^2)/784+(65*alpha)/896+225/3136)+2*b[t]*g[t]*((-(9*alpha^4)/224)-(243*alpha^3)/6272-(993*alpha^2)/12544+(39*alpha)/1792+855/6272)+2*b[t]*h[t]*((-(7*alpha^4)/512)-(3*alpha^3)/64-(39*alpha^2)/512-(25*alpha)/256+15/64)+2*b[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
                  + c[t]^2*((-(81*alpha^4)/686)+(135*alpha^3)/1372-(27*alpha^2)/392+(405*alpha)/5488+81/5488)+2*c[t]*d[t]*((-(2025*alpha^4)/10976)+(7425*alpha^3)/43904-(11043*alpha^2)/87808+(177*alpha)/1792+465/10976)+2*c[t]*e[t]*((-(72*alpha^4)/343)+(57*alpha^3)/343-(181*alpha^2)/1372+(239*alpha)/2744+243/2744)+2*c[t]*f[t]*((-(2025*alpha^4)/10976)+(3645*alpha^3)/43904-(7515*alpha^2)/87808+(405*alpha)/12544+6795/43904)+2*c[t]*g[t]*((-(81*alpha^4)/686)-(27*alpha^3)/686-(9*alpha^2)/392-(165*alpha)/2744+165/686)+2*c[t]*h[t]*((-(9*alpha^4)/224)-(705*alpha^3)/6272-(229*alpha^2)/12544-(2165*alpha)/12544+1077/3136)+2*c[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
                  + d[t]^2*((-(50625*alpha^4)/175616)+(50625*alpha^3)/175616-(2025*alpha^2)/10976+(2025*alpha)/21952+2025/21952)+2*d[t]*e[t]*((-(225*alpha^4)/686)+(225*alpha^3)/784-(3273*alpha^2)/21952+(717*alpha)/21952+54/343)+2*d[t]*f[t]*((-(50625*alpha^4)/175616)+(3375*alpha^3)/21952-(5625*alpha^2)/175616-(5655*alpha)/87808+2535/10976)+2*d[t]*g[t]*((-(2025*alpha^4)/10976)-(2025*alpha^3)/43904+(7857*alpha^2)/87808-(2025*alpha)/12544+3321/10976)+2*d[t]*h[t]*((-(225*alpha^4)/3584)-(4275*alpha^3)/25088+(921*alpha^2)/12544-(39*alpha)/196+1125/3136)+2*d[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
                  + e[t]^2*((-(128*alpha^4)/343)+(96*alpha^3)/343-(24*alpha^2)/343-(22*alpha)/343+78/343)+2*e[t]*f[t]*((-(225*alpha^4)/686)+(15*alpha^3)/112+(1655*alpha^2)/21952-(3665*alpha)/21952+3135/10976)+2*e[t]*g[t]*((-(72*alpha^4)/343)-(27*alpha^3)/343+(267*alpha^2)/1372-(615*alpha)/2744+873/2744)+2*e[t]*h[t]*((-alpha^4/14)-(159*alpha^3)/784+(417*alpha^2)/3136-(529*alpha)/3136+243/784)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
                  + f[t]^2*((-(50625*alpha^4)/175616)+(3375*alpha^3)/175616+(16875*alpha^2)/87808-(10125*alpha)/43904+3375/10976)+2*f[t]*g[t]*((-(2025*alpha^4)/10976)-(5805*alpha^3)/43904+(21465*alpha^2)/87808-(2715*alpha)/12544+12675/43904)+2*f[t]*h[t]*((-(225*alpha^4)/3584)-(2505*alpha^3)/12544+(3235*alpha^2)/25088-(1145*alpha)/12544+705/3136)+2*f[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
                  + g[t]^2*((-(81*alpha^4)/686)-(243*alpha^3)/1372+(81*alpha^2)/392-(729*alpha)/5488+1215/5488)+2*g[t]*h[t]*(405/3136-9/224*alpha^4-999/6272*alpha^3+807/12544*alpha^2+75/12544*alpha)+2*g[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
                  + h[t]^2*(3/64-7/512*alpha^4-45/512*alpha^3-3/128*alpha^2+5/64*alpha)+2*h[t]*i[t]*(1/16*alpha-1/16*alpha^2)
                  + i[t]^2*0 )
      
      
      g[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(7/1024*alpha^4-3/512*alpha^3+3/512*alpha^2)+2*b[t]*c[t]*(1/12544+9/448*alpha^4-117/6272*alpha^3+27/1792*alpha^2+37/12544*alpha)+2*b[t]*d[t]*((225*alpha^4)/7168-(75*alpha^3)/3136+(33*alpha^2)/1792+(9*alpha)/784+9/12544)+2*b[t]*e[t]*(alpha^4/28-(3*alpha^3)/196+(3*alpha^2)/224+(81*alpha)/3136+9/3136)+2*b[t]*f[t]*((225*alpha^4)/7168+(135*alpha^3)/25088+(15*alpha^2)/3584+(275*alpha)/6272+25/3136)+2*b[t]*g[t]*((9*alpha^4)/448+(177*alpha^3)/6272+(3*alpha^2)/1792+(765*alpha)/12544+225/12544)+2*b[t]*h[t]*((7*alpha^4)/1024+(9*alpha^3)/256+(3*alpha^2)/128+(9*alpha)/128+9/256)+2*b[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
                  + c[t]^2*((81*alpha^4)/1372-(81*alpha^3)/1372+(81*alpha^2)/2744+(87*alpha)/5488+33/21952)+2*c[t]*d[t]*((2025*alpha^4)/21952-(3375*alpha^3)/43904+(261*alpha^2)/12544+(3453*alpha)/87808+573/87808)+2*c[t]*e[t]*((36*alpha^4)/343-(18*alpha^3)/343-(9*alpha^2)/686+(185*alpha)/2744+25/1372)+2*c[t]*f[t]*((2025*alpha^4)/21952+(405*alpha^3)/43904-(675*alpha^2)/12544+(7695*alpha)/87808+3555/87808)+2*c[t]*g[t]*((81*alpha^4)/1372+(27*alpha^3)/343-(171*alpha^2)/2744+(111*alpha)/1372+1713/21952)+2*c[t]*h[t]*((9*alpha^4)/448+(639*alpha^3)/6272+(261*alpha^2)/12544+(271*alpha)/12544+1711/12544)+2*c[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
                  + d[t]^2*((50625*alpha^4)/351232-(16875*alpha^3)/175616-(2025*alpha^2)/175616+(405*alpha)/5488+135/6272)+2*d[t]*e[t]*((225*alpha^4)/1372-(75*alpha^3)/1372-(789*alpha^2)/10976+(45*alpha)/448+1089/21952)+2*d[t]*f[t]*((50625*alpha^4)/351232+(3375*alpha^3)/87808-(5625*alpha^2)/43904+(4395*alpha)/43904+1185/12544)+2*d[t]*g[t]*((2025*alpha^4)/21952+(6075*alpha^3)/43904-(1539*alpha^2)/12544+(4293*alpha)/87808+14013/87808)+2*d[t]*h[t]*((225*alpha^4)/7168+(4125*alpha^3)/25088+(687*alpha^2)/25088-(513*alpha)/6272+1557/6272)+2*d[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
                  + e[t]^2*((64*alpha^4)/343-(48*alpha^2)/343+(36*alpha)/343+135/1372)+2*e[t]*f[t]*((225*alpha^4)/1372+(135*alpha^3)/1372-(1965*alpha^2)/10976+(205*alpha)/3136+3595/21952)+2*e[t]*g[t]*((36*alpha^4)/343+(66*alpha^3)/343-(93*alpha^2)/686-(81*alpha)/2744+333/1372)+2*e[t]*h[t]*(alpha^4/28+(39*alpha^3)/196+(93*alpha^2)/1568-(585*alpha)/3136+1035/3136)+2*e[t]*i[t]*(41/98+24/49*alpha^2-20/49*alpha)
                  + f[t]^2*((50625*alpha^4)/351232+(30375*alpha^3)/175616-(30375*alpha^2)/175616-(375*alpha)/21952+375/1568)+2*f[t]*g[t]*((2025*alpha^4)/21952+(9855*alpha^3)/43904-(1035*alpha^2)/12544-(11625*alpha)/87808+27075/87808)+2*f[t]*h[t]*((225*alpha^4)/7168+(1215*alpha^3)/6272+(1425*alpha^2)/12544-(785*alpha)/3136+4465/12544)+2*f[t]*i[t]*(285/784+675/1568*alpha^2-255/784*alpha)
                  + g[t]^2*((81*alpha^4)/1372+(297*alpha^3)/1372+(81*alpha^2)/2744-(1215*alpha)/5488+7425/21952)+2*g[t]*h[t]*(3951/12544+9/448*alpha^4+933/6272*alpha^3+2109/12544*alpha^2-3033/12544*alpha)+2*g[t]*i[t]*(87/392+27/98*alpha^2-6/49*alpha)
                  + h[t]^2*(27/128+7/1024*alpha^4+39/512*alpha^3+93/512*alpha^2-9/64*alpha)+2*h[t]*i[t]*(1/16+3/32*alpha^2+1/16*alpha)
                  + i[t]^2*0 )
      
      
      h[t+1] <- ( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                  + b[t]^2*(-1/512*alpha^4+1/512*alpha^3)+2*b[t]*c[t]*(-9/1568*alpha^4+27/6272*alpha^3+17/12544*alpha^2+1/12544*alpha)+2*b[t]*d[t]*((-(225*alpha^4)/25088)+(45*alpha^3)/12544+(117*alpha^2)/25088+(9*alpha)/12544)+2*b[t]*e[t]*((-alpha^4/98)-alpha^3/784+(27*alpha^2)/3136+(9*alpha)/3136)+2*b[t]*f[t]*((-(225*alpha^4)/25088)-(225*alpha^3)/25088+(125*alpha^2)/12544+(25*alpha)/3136)+2*b[t]*g[t]*((-(9*alpha^4)/1568)-(99*alpha^3)/6272+(45*alpha^2)/12544+(225*alpha)/12544)+2*b[t]*h[t]*((-alpha^4/512)-alpha^3/64-(9*alpha^2)/512+(9*alpha)/256)+2*b[t]*i[t]*(1/16*alpha-1/16*alpha^2)
                  + c[t]^2*((-(81*alpha^4)/4802)+(81*alpha^3)/9604+(135*alpha^2)/19208+(51*alpha)/38416+3/38416)+2*c[t]*d[t]*((-(2025*alpha^4)/76832)+(1215*alpha^3)/307328+(10089*alpha^2)/614656+(3375*alpha)/614656+153/307328)+2*c[t]*e[t]*((-(72*alpha^4)/2401)-(27*alpha^3)/2401+(239*alpha^2)/9604+(281*alpha)/19208+33/19208)+2*c[t]*f[t]*((-(2025*alpha^4)/76832)-(10125*alpha^3)/307328+(14625*alpha^2)/614656+(19125*alpha)/614656+675/153664)+2*c[t]*g[t]*((-(81*alpha^4)/4802)-(243*alpha^3)/4802+(9*alpha^2)/19208+(1107*alpha)/19208+45/4802)+2*c[t]*h[t]*((-(9*alpha^4)/1568)-(297*alpha^3)/6272-(775*alpha^2)/12544+(1219*alpha)/12544+111/6272)+2*c[t]*i[t]*(3/98-9/49*alpha^2+15/98*alpha)
                  + d[t]^2*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/1229312+(18225*alpha^2)/614656+(5265*alpha)/307328+405/153664)+2*d[t]*e[t]*((-(225*alpha^4)/4802)-(1305*alpha^3)/38416+(5499*alpha^2)/153664+(5679*alpha)/153664+621/76832)+2*d[t]*f[t]*((-(50625*alpha^4)/1229312)-(10125*alpha^3)/153664+(28575*alpha^2)/1229312+(39825*alpha)/614656+2925/153664)+2*d[t]*g[t]*((-(2025*alpha^4)/76832)-(27135*alpha^3)/307328-(13851*alpha^2)/614656+(60831*alpha)/614656+11745/307328)+2*d[t]*h[t]*((-(225*alpha^4)/25088)-(1935*alpha^3)/25088-(747*alpha^2)/6272+(855*alpha)/6272+27/392)+2*d[t]*i[t]*(45/392-225/784*alpha^2+135/784*alpha)
                  + e[t]^2*((-(128*alpha^4)/2401)-(160*alpha^3)/2401+(72*alpha^2)/2401+(162*alpha)/2401+54/2401)+2*e[t]*f[t]*((-(225*alpha^4)/4802)-(3825*alpha^3)/38416-(325*alpha^2)/153664+(15325*alpha)/153664+1875/38416)+2*e[t]*g[t]*((-(72*alpha^4)/2401)-(279*alpha^3)/2401-(657*alpha^2)/9604+(2367*alpha)/19208+1755/19208)+2*e[t]*h[t]*((-alpha^4/98)-(73*alpha^3)/784-(549*alpha^2)/3136+(387*alpha)/3136+243/1568)+2*e[t]*i[t]*(12/49-16/49*alpha^2+4/49*alpha)
                  + f[t]^2*((-(50625*alpha^4)/1229312)-(151875*alpha^3)/1229312-(16875*alpha^2)/307328+(9375*alpha)/76832+1875/19208)+2*f[t]*g[t]*((-(2025*alpha^4)/76832)-(38475*alpha^3)/307328-(79875*alpha^2)/614656+(69525*alpha)/614656+25875/153664)+2*f[t]*h[t]*((-(225*alpha^4)/25088)-(1125*alpha^3)/12544-(5375*alpha^2)/25088+(625*alpha)/12544+825/3136)+2*f[t]*i[t]*(75/196-225/784*alpha^2-75/784*alpha)
                  + g[t]^2*((-(81*alpha^4)/4802)-(1053*alpha^3)/9604-(3645*alpha^2)/19208+(2025*alpha)/38416+10125/38416)+2*g[t]*h[t]*(2295/6272-9/1568*alpha^4-423/6272*alpha^3-2763/12544*alpha^2-909/12544*alpha)+2*g[t]*i[t]*(45/98-9/49*alpha^2-27/98*alpha)
                  + h[t]^2*(27/64-1/512*alpha^4-17/512*alpha^3-45/256*alpha^2-27/128*alpha)+2*h[t]*i[t]*(3/8-1/16*alpha^2-5/16*alpha)
                  + i[t]^2*0 )
      
      
      i[t+1] <-( a[t]^2*0+2*a[t]*b[t]*0+2*a[t]*c[t]*0+2*a[t]*d[t]*0+2*a[t]*e[t]*0+2*a[t]*f[t]*0+2*a[t]*g[t]*0+2*a[t]*h[t]*0+2*a[t]*i[t]*0
                 + b[t]^2*(1/4096*alpha^4)+2*b[t]*c[t]*(9/12544*alpha^4+3/12544*alpha^3+1/50176*alpha^2)+2*b[t]*d[t]*((225*alpha^4)/200704+(45*alpha^3)/50176+(9*alpha^2)/50176)+2*b[t]*e[t]*(alpha^4/784+(3*alpha^3)/1568+(9*alpha^2)/12544)+2*b[t]*f[t]*((225*alpha^4)/200704+(75*alpha^3)/25088+(25*alpha^2)/12544)+2*b[t]*g[t]*((9*alpha^4)/12544+(45*alpha^3)/12544+(225*alpha^2)/50176)+2*b[t]*h[t]*(alpha^4/4096+(3*alpha^3)/1024+(9*alpha^2)/1024)+2*b[t]*i[t]*(1/64*alpha^2)
                 + c[t]^2*((81*alpha^4)/38416+(27*alpha^3)/19208+(27*alpha^2)/76832+(3*alpha)/76832+1/614656)+2*c[t]*d[t]*((2025*alpha^4)/614656+(2295*alpha^3)/614656+(3681*alpha^2)/2458624+(153*alpha)/614656+9/614656)+2*c[t]*e[t]*((9*alpha^4)/2401+(33*alpha^3)/4802+(157*alpha^2)/38416+(33*alpha)/38416+9/153664)+2*c[t]*f[t]*((2025*alpha^4)/614656+(6075*alpha^3)/614656+(21825*alpha^2)/2458624+(675*alpha)/307328+25/153664)+2*c[t]*g[t]*((81*alpha^4)/38416+(27*alpha^3)/2401+(1287*alpha^2)/76832+(45*alpha)/9604+225/614656)+2*c[t]*h[t]*((9*alpha^4)/12544+(111*alpha^3)/12544+(1441*alpha^2)/50176+(111*alpha)/12544+9/12544)+2*c[t]*i[t]*(1/784+9/196*alpha^2+3/196*alpha)
                 + d[t]^2*((50625*alpha^4)/9834496+(10125*alpha^3)/1229312+(6075*alpha^2)/1229312+(405*alpha)/307328+81/614656)+2*d[t]*e[t]*((225*alpha^4)/38416+(1035*alpha^3)/76832+(6921*alpha^2)/614656+(621*alpha)/153664+81/153664)+2*d[t]*f[t]*((50625*alpha^4)/9834496+(43875*alpha^3)/2458624+(51525*alpha^2)/2458624+(2925*alpha)/307328+225/153664)+2*d[t]*g[t]*((2025*alpha^4)/614656+(11745*alpha^3)/614656+(84321*alpha^2)/2458624+(11745*alpha)/614656+2025/614656)+2*d[t]*h[t]*((225*alpha^4)/200704+(45*alpha^3)/3136+(1287*alpha^2)/25088+(27*alpha)/784+81/12544)+2*d[t]*i[t]*(9/784+225/3136*alpha^2+45/784*alpha)
                 + e[t]^2*((16*alpha^4)/2401+(48*alpha^3)/2401+(54*alpha^2)/2401+(27*alpha)/2401+81/38416)+2*e[t]*f[t]*((225*alpha^4)/38416+(1875*alpha^3)/76832+(22825*alpha^2)/614656+(1875*alpha)/76832+225/38416)+2*e[t]*g[t]*((9*alpha^4)/2401+(117*alpha^3)/4802+(2061*alpha^2)/38416+(1755*alpha)/38416+2025/153664)+2*e[t]*h[t]*(alpha^4/784+(27*alpha^3)/1568+(873*alpha^2)/12544+(243*alpha)/3136+81/3136)+2*e[t]*i[t]*(9/196+4/49*alpha^2+6/49*alpha)
                 + f[t]^2*((50625*alpha^4)/9834496+(16875*alpha^3)/614656+(16875*alpha^2)/307328+(1875*alpha)/38416+625/38416)+2*f[t]*g[t]*((2025*alpha^4)/614656+(15525*alpha^3)/614656+(173025*alpha^2)/2458624+(25875*alpha)/307328+5625/153664)+2*f[t]*h[t]*((225*alpha^4)/200704+(825*alpha^3)/50176+(3925*alpha^2)/50176+(825*alpha)/6272+225/3136)+2*f[t]*i[t]*(25/196+225/3136*alpha^2+75/392*alpha)
                 + g[t]^2*((81*alpha^4)/38416+(405*alpha^3)/19208+(6075*alpha^2)/76832+(10125*alpha)/76832+50625/614656)+2*g[t]*h[t]*(2025/12544+9/12544*alpha^4+153/12544*alpha^3+3681/50176*alpha^2+2295/12544*alpha)+2*g[t]*i[t]*(225/784+9/196*alpha^2+45/196*alpha)
                 + h[t]^2*(81/256+1/4096*alpha^4+3/512*alpha^3+27/512*alpha^2+27/128*alpha)+2*h[t]*i[t]*(9/16+1/64*alpha^2+3/16*alpha)
                 + i[t]^2*1 )
      
      
      
    }
    
    y <- rbind(y,rbind(a,b,c,d,e,f,g,h,i))
    
    
  }
  
  write.csv(y,"y.csv")
  
  
  
  homozygote<-matrix(0,nrow=9,ncol=21,byrow=T)  
  heterozygote<-matrix(0,nrow=9,ncol=21,byrow=T)
  
  for (m in 1:9) {
    
    homozygote[m,] <- y[9*m-8,]+y[9*m,]
    heterozygote[m,] <-y[9*m-7,]+y[9*m-6,]+y[9*m-5,]+y[9*m-4,]+y[9*m-3,]+y[9*m-2,]+y[9*m-1,]
    
    
  }
  
  colors_homozygote <- colorRampPalette(c("snow", "mediumpurple3"))(18)
  colors_heterozygote <- colorRampPalette(c("snow", "gold3"))(18)
  
  layout(matrix(c(1:2),2,1))
  par(mar=c(1.5,2.8,0.8,0.8))
  
  plot(0,0,type="l",lwd=1,xlab="",ylab="",cex.lab=1,col="black",xaxt="n",yaxt="n",xlim=c(0,0.25),ylim=c(0,0.1))
  
  axis(1,las = 1,c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),lwd=1,cex.axis=1.2,labels = c("0","1/11","1/10","1/9","1/8","1/7","1/6","1/5","1/4"),mgp = c(0.1, 0.6,0),tick = TRUE)
  axis(2,las = 1,seq(0,0.1,0.02),seq(0,0.1,0.02),lwd=1,mgp = c(0.3, 0.55,0),cex.axis=1.2)
  
  
  
  y1 <- as.numeric(homozygote[1:9,21])
  lines(c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),y1,type = "l",lwd=4,col="mediumpurple3")
  
  
  
  plot(0,0,type="l",lwd=1,col="black",xlab="",ylab="",cex.lab=1.5,xaxt="n",yaxt="n",xlim=c(0,0.25),ylim=c(0.90,1))
  
  axis(1,las = 1,c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),lwd=1,cex.axis=1.2,labels = c("0","1/11","1/10","1/9","1/8","1/7","1/6","1/5","1/4"),mgp = c(0.1, 0.6,0),tick = TRUE)
  axis(2,las = 1,seq(0.90,1,0.02),seq(0.90,1,0.02),lwd=1,mgp = c(0.3, 0.55,0),cex.axis=1.2)
  
  
  
  y2 <- as.numeric(heterozygote[1:9,21])
  lines(c(0,1/11,1/10,1/9,1/8,1/7,1/6,1/5,1/4),y2,type = "l",lwd=4,col="gold3")
  

  layout(matrix(c(1:2),1,2))
  par(mar=c(3,3,1,3))
  
  plot(0,0,type="l",xlab="",ylab="",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",axes="F",xlim=c(0,2),ylim=c(0,1))
  text.legend = c(expression(paste(alpha, " = 0")),expression(paste(alpha, " = 1/11")),expression(paste(alpha, " = 1/10")), expression(paste(alpha, " = 1/9")), expression(paste(alpha, " = 1/8")), expression(paste(alpha, " = 1/7")),expression(paste(alpha, " = 1/6")),  expression(paste(alpha, " = 1/5")),expression(paste(alpha, " = 1/4")))
  legend("center", legend = text.legend, lty = c(1,1),cex=2,lwd=8, col = c(colors_homozygote[2],colors_homozygote[4],colors_homozygote[6],colors_homozygote[8],colors_homozygote[10],colors_homozygote[12],colors_homozygote[14],colors_homozygote[16],colors_homozygote[18]))
  
  plot(0,0,type="l",xlab="",ylab="",cex.lab=1,lwd=1,col="black",xaxt="n",yaxt="n",axes="F",xlim=c(0,2),ylim=c(0,1))
  text.legend = c(expression(paste(alpha, " = 0")),expression(paste(alpha, " = 1/11")),expression(paste(alpha, " = 1/10")), expression(paste(alpha, " = 1/9")), expression(paste(alpha, " = 1/8")), expression(paste(alpha, " = 1/7")),expression(paste(alpha, " = 1/6")),  expression(paste(alpha, " = 1/5")),expression(paste(alpha, " = 1/4")))
  legend("center", legend = text.legend, lty = c(1,1),cex=2,lwd=8, col = c(colors_heterozygote[2],colors_heterozygote[4],colors_heterozygote[6],colors_heterozygote[8],colors_heterozygote[10],colors_heterozygote[12],colors_heterozygote[14],colors_heterozygote[16],colors_heterozygote[18]))


setwd("E:/CCB/mine/多倍体/8octoploid")
pdf(file="legend_purple.pdf",2,9) 
nlev <- colorRampPalette(c( "mediumpurple3","snow"))(50) 
plot(y = c(1, (length(nlev) + 1)), x = c(1, 2),xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n',type = 'n', ann = F)
rect(xleft = rep(0, (length(nlev) + 1)),ybottom = 1:length(nlev),xright = rep(2, (length(nlev) + 1)),ytop = 2:(length(nlev) + 1),col = nlev,border = nlev)
dev.off()

setwd("E:/CCB/mine/多倍体/8octoploid")
pdf(file="legend_gold.pdf",2,9) 
nlev <- colorRampPalette(c( "gold3","snow"))(50) 
plot(y = c(1, (length(nlev) + 1)), x = c(1, 2),xaxs = 'i', yaxs = 'i', xaxt = 'n', yaxt = 'n',type = 'n', ann = F)
rect(xleft = rep(0, (length(nlev) + 1)),ybottom = 1:length(nlev),xright = rep(2, (length(nlev) + 1)),ytop = 2:(length(nlev) + 1),col = nlev,border = nlev)
dev.off()

  
  
  
  
  
  
  
  
   
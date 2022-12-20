rm(list = ls())
setwd("C:/Users/zknap/Desktop/diplomka/AP")
library(survival)
library(ggplot2)
library(KMsurv)
library(dplyr)
library(tidyverse)
library(gridExtra)


source("420810-Knapekova_Zuzana-funkcie-analprez-2018.R")
#devtools::install_github('datarootsio/artyfarty')

# AFT modely  -----------------------------------------

t <- seq(0, 1, len=99)
h <- function(tt) {exp(-3*(tt-0.5)*dnorm(tt-0.5, sd=0.2))}
H <- function(tt) {
  val <- numeric(length(tt))
  for (i in 1:length(tt)) val[i] <- integrate(h, 0, tt[i])$value
  val
}
S <- function(tt) exp(-H(tt))

#df<-data.frame(t, S(t), S(t/2), S(t*2))

df1 <- data.frame(dates = t,Variable = S(t), risk=h(t))
df2 <- data.frame(dates = t,Variable = S(t/1.5),  risk=h(t/1.5)/1.5)
df3 <- data.frame(dates = t,Variable = S(t*1.5),  risk=h(t*1.5)*1.5 )


df4 <- df1 %>%  mutate(Type = "S(t)") %>%
  bind_rows(df2 %>%
              mutate(Type =  "S(t/1.5)")) %>%
  bind_rows(df3 %>%
              mutate(Type = "S(t*1.5)"))


g1<-ggplot(df4,aes(y = Variable,x = dates,color = Type)) + 
  geom_line(size=0.75) +
  xlab("Cas")+ylab("Funkcia prežívania")+
  #labs(title=expression("Funkcia prežívania pre rôzne hodnoty"~eta))+
  scale_colour_manual("", values=c("blue", "black", "red"),
                      breaks = c("S(t/1.5)", "S(t)", "S(t*1.5)"),
                      labels=c(expression(phantom(x)*eta==3/2), expression(phantom(x)*eta==1), expression(phantom(x)*eta==2/3)))+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 12))



g2<-ggplot(df4,aes(y = risk,x = dates,color = Type)) + 
  geom_line(size=0.75) +
  xlab("Cas")+ylab("Riziková funkcia")+
  #labs(title=expression("Riziková funkcia pre rôzne hodnoty"~eta))+
  scale_colour_manual("", values=c("blue", "black", "red"),
                      breaks = c("S(t/1.5)", "S(t)", "S(t*1.5)"),
                      labels=c(expression(phantom(x)*eta==3/2), expression(phantom(x)*eta==1), expression(phantom(x)*eta==2/3)))+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 12))



library(cowplot)
prow <- plot_grid( g1 + theme(legend.position="none"),
                   g2 + theme(legend.position="none"),
                   align = 'v',
                   nrow = 2, ncol=1
)

legend_b <- get_legend(g1 + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol=1, rel_heights = c(1, .2))
p

#grid.arrange(g1, g2, ncol=1)



# Priklad - Weibullovo rozdelenie -----------------------------------------


weibull.survival<-function(x, alpha, lambda){
  result=exp(-lambda*x^(alpha))
  return(result)
}

weibull.hazard<-function(x, alpha, lambda){
  result=alpha*lambda*(x*lambda)^(alpha-1)
  return(result)
}

exp.hazard<-function(x, lambda){
  result=lambda*x
  return(result)
}

alpha<- 0.5
lambda<-1
x<-seq(0,5,by=0.1)
y1<-alpha*lambda*x^(alpha-1)


alpha<- 1
lambda<-1
y2<-alpha*lambda*x^(alpha-1)


alpha<- 1.5
lambda<-1
y3<-alpha*lambda*x^(alpha-1)

df1 <- data.frame(dates = x,Variable = y1)
df2 <- data.frame(dates = x,Variable = y2)
df3 <- data.frame(dates = x,Variable = y3)


df4 <- df1 %>%  mutate(Type = "lambda==0.5") %>%
  bind_rows(df2 %>%
              mutate(Type =  "lambda==1")) %>%
  bind_rows(df3 %>%
              mutate(Type = "lambda==1.5"))

ggplot(df4,aes(y = Variable,x = dates,color = Type)) + 
  geom_line(size=0.75) +
  xlab("Cas")+ylab("Riziko")+
  #labs(title=expression("Riziková funkcia Weibullovho rozdelenia pre rôzne hodnoty"~alpha))+
  scale_colour_manual("", values=c("red", "blue", "green"),
                      breaks = c("lambda==0.5", "lambda==1", "lambda==1.5"),
                      labels=c(bquote(alpha==0.5), bquote(alpha==1), bquote(alpha==1.5)))+
    theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 14),
        #, axis.text.x = element_blank(), axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(), axis.ticks.y = element_blank()
        )



# Priklad - Log-logistické rozdelenie -------------------------------------



log.logistic.hazard<-function(x, alpha, lambda){
  result=alpha*x^(alpha-1)*lambda/(1+lambda*x^alpha)
  return(result)
}

lambda<-1

sigma<-0.5
x<-seq(0,5,by=0.01)
y1<-log.logistic.hazard(x,alpha=1/sigma, lambda )


sigma<-1
y2<-log.logistic.hazard(x,alpha=1/sigma, lambda )

sigma<-2
y3<-log.logistic.hazard(x,alpha=1/sigma, lambda )

df1 <- data.frame(dates = x,Variable = y1)
df2 <- data.frame(dates = x,Variable = y2)
df3 <- data.frame(dates = x,Variable = y3)


df4 <- df1 %>%  mutate(Type = "sigma==0.5") %>%
  bind_rows(df2 %>%
              mutate(Type =  "sigma==1")) %>%
  bind_rows(df3 %>%
              mutate(Type = "sigma==2"))
ggplot(df4,aes(y = Variable,x = dates,color = Type)) + 
  geom_line(size=0.75) +
  xlab("Cas")+ylab("Riziko")+
  #labs(title=expression("Riziková funkcia log-logistického rozdelenia pre rôzne hodnoty"~kappa))+
  scale_colour_manual("", values=c("red", "blue", "green"),
                      breaks = c("sigma==0.5", "sigma==1", "sigma==2"),
                      labels=c(bquote(kappa==0.5), bquote(kappa==1), bquote(kappa==2)))+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 14)
        #, axis.text.x = element_blank(), axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(), axis.ticks.y = element_blank()
  )



# Log-normálne rozdelenie -------------------------------------------------

log.normal.hazard<-function(x, mu, sigma){
  #arg<-(log(x)-mu)/sigma
  #result=dnorm(arg, mu, sigma)/(sigma*x*(1-pnorm(arg, 0, 1)))
  s<-(1-plnorm(x, mu, sigma))
  f<-dlnorm(x, mu, sigma)
  result<-f/s
  return(result)
}

x<-seq(0,5,by=0.01)
mu<-0
sigma<-0.5

y1<-log.normal.hazard(x,mu, sigma )

sigma<-1
y2<-log.normal.hazard(x,mu, sigma )


sigma<-2
y3.2<-log.normal.hazard(x,mu, sigma )[-c(1,2)]
y3<-c(y3.2, 0.1369096, 0.1367127)


df1 <- data.frame(dates = x,Variable = y1)
df2 <- data.frame(dates = x,Variable = y2)
df3 <- data.frame(dates = x,Variable = y3)


df4 <- df1 %>%  mutate(Type = "sigma==0.5") %>%
  bind_rows(df2 %>%
              mutate(Type =  "sigma==1")) %>%
  bind_rows(df3 %>%
              mutate(Type = "sigma==2"))

ggplot(df4,aes(y = Variable,x = dates,color = Type)) + 
  geom_line(size=0.75)+
  xlab("Cas")+ylab("Riziko")+
 # labs(title=expression("Riziková funkcia log-normálneho rozdelenia pre rôzne hodnoty"~sigma))+
  scale_colour_manual("", values=c("red", "blue", "green"),
                      breaks = c("sigma==0.5", "sigma==1", "sigma==2"),
                      labels=c(bquote(sigma==0.5), bquote(sigma==1), bquote(sigma==2)))+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 14)
        #, axis.text.x = element_blank(), axis.text.y = element_blank(),
        #axis.ticks.x = element_blank(), axis.ticks.y = element_blank()
  )



time<-c(10, 13, 18, 19, 23, 30, 36, 38, 54, 56, 59, 75, 93, 97, 104, 107, 107, 107 )
status<-c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0)
H<-cum.risk(time,status)$L
f<-qnorm(1-exp(-H))
x<-log(time[status==1])
df<-data.frame(x, f)


g1<-ggplot() +
  #geom_point(data = df, mapping = aes(x = x, y =f)) +
  #geom_line(data = df, mapping = aes(x = x, y =f))+
  geom_step(data = df, mapping = aes(x = x, y =f))+
  #scale_x_continuous(limit = c(0,3.5)) +
  #scale_y_continuous(limit = c(0,3.5)) +
  xlab("ln(t)")+
  ylab (expression(Phi^{-1}~"(exp(-"~hat(H)~"(t)))")) + 
  #ggtitle("Vhodne zvolený model")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


data(ovarian)
time<-ovarian$futime
status<-ovarian$fustat
H<-cum.risk(time,status)$L
f<-qnorm(1-exp(-H))
x<-log(time[status==1])
df<-data.frame(x, f)


g2<-ggplot() +
  #geom_point(data = df, mapping = aes(x = x, y =f)) +
  geom_step(data = df, mapping = aes(x = x, y =f))+
  #geom_line(data = df, mapping = aes(x = x, y =f))+
  #scale_x_continuous(limit = c(0,3.5)) +
  #scale_y_continuous(limit = c(0,3.5)) +
  xlab("ln(t)")+
  ylab (expression(Phi^{-1}~"(exp(-"~hat(H)~"(t)))")) + 
  #ggtitle("Nevhodne zvolený model")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



grid.arrange(g1, g2, ncol=2)




# QQ plot -----------------------------------------------------------------


#Collet example 1.2
time<-c(23, 47, 69, 70, 71, 100, 101, 148, 181, 198, 208, 212, 224, 5, 8, 10, 13, 18, 24, 26, 26, 31, 35, 40, 41, 48, 50, 59,
        61, 68, 71, 76, 105, 107, 109, 113, 116, 118, 143, 154, 162, 188, 212, 217, 225)
status<-c(1,1,1,0,0,0,0,1,1,0,0,0,0, rep(1, 16), 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, rep(0, 6))
HPA<-c(rep("1", 13), rep("2", 32))
#HPA: 1 - negative, 2 - positive
status.1<-status[HPA=="1"]
time.1<-time[HPA=="1"]
status.2<-status[HPA=="2"]
time.2<-time[HPA=="2"]

S1<-f.prezivania(time.1, status.1)$S
T1<-f.prezivania(time.1, status.1)$T
gr1<-quantile(S1, probs = seq(0,1,1/100))
result11<-rep(0, length(gr1))
diff<-rep(0, length(S1))

for (i in 1:length(gr1)){
  for (j in 1:length(S1)){
    diff[j]<-abs(gr1[i]-S1[j])
    result11[i]<-T1[which.min(diff)]
      }
}


S2<-f.prezivania(time.2, status.2)$S
T2<-f.prezivania(time.2, status.2)$T
gr2<-quantile(S2, probs = seq(0,1,1/100))
result22<-rep(0, length(gr2))
diff<-rep(0, length(S2))

for (i in 1:length(gr2)){
  for (j in 1:length(S2)){
    diff[j]<-abs(gr2[i]-S2[j])
    result22[i]<-T2[which.min(diff)]
  }
}

result1<-result11
result2<-result22
df<-data.frame(sort(result1), sort(result2))

ggplot() +
  geom_point(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  geom_line(data = df, mapping = aes(x = sort.result1., y =2*sort.result1.))+
  #geom_line(data = df, mapping = aes(x = gr1, y =gr1))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


ggplot() +
  geom_smooth(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  geom_point(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  #geom_line(data = df, mapping = aes(x = gr1, y =gr1))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))




# QQ plot2 ----------------------------------------------------------------

data("tongue", package="KMsurv")
status<-tongue$delta
time<-tongue$time
type<-tongue$type


status.1<-status[type=="1"]
time.1<-time[type=="1"]
status.2<-status[type=="2"]
time.2<-time[type=="2"]


S1<-f.prezivania(time.1, status.1)$S
T1<-f.prezivania(time.1, status.1)$T
gr1<-quantile(S1, probs = seq(0,1,1/100))
result11<-rep(0, length(gr1))
diff<-rep(0, length(S1))

for (i in 1:length(gr1)){
  for (j in 1:length(S1)){
    diff[j]<-abs(gr1[i]-S1[j])
    result11[i]<-T1[which.min(diff)]
  }
}


S2<-f.prezivania(time.2, status.2)$S
T2<-f.prezivania(time.2, status.2)$T
gr2<-quantile(S2, probs = seq(0,1,1/100))
result22<-rep(0, length(gr2))
diff<-rep(0, length(S2))

for (i in 1:length(gr2)){
  for (j in 1:length(S2)){
    diff[j]<-abs(gr2[i]-S2[j])
    result22[i]<-T2[which.min(diff)]
  }
}

result1<-result11
result2<-result22
df<-data.frame(sort(result1), sort(result2))



g2<-ggplot(data = df, mapping = aes(x = sort.result1., y =sort.result2.)) +
  #geom_point()+
  #geom_smooth(se=FALSE, colour="black")+
  geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  #ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))





# QQ plot 3 ---------------------------------------------------------------

weibull.survival<-function(x, alpha, lambda){
  result=exp(-lambda*x^(alpha))
  return(result)
}

alpha<- 0.5
lambda<-5
x1<-seq(2,6,by=0.01)
y1<-alpha*lambda*x1^(alpha-1)+3*rnorm(1)

gr1<-quantile(y1, probs = seq(0,1,1/100))
result1<-rep(0, length(gr1))
diff<-rep(0, length(y1))

for (i in 1:length(gr1)){
  for (j in 1:length(y1)){
    diff[j]<-abs(gr1[i]-y1[j])
    result1[i]<-x1[which.min(diff)]
  }
}


alpha<- 1.5
lambda<-3
x2<-seq(0,5,by=0.5)
y2<-alpha*lambda*x2^(alpha-1)+0.53*rnorm(1)

gr2<-quantile(y2, probs = seq(0,1,1/100))
result2<-rep(0, length(gr2))
diff<-rep(0, length(y2))

for (i in 1:length(gr2)){
  for (j in 1:length(y2)){
    diff[j]<-abs(gr2[i]-y2[j])
    result2[i]<-x2[which.min(diff)]
  }
}

df<-data.frame(sort(result1), sort(result2))

ggplot() +
  geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  #geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result1.))+
  #geom_line(data = df, mapping = aes(x = gr1, y =gr1))+
  #scale_x_continuous(limit = c(0,181)) +
  #scale_y_continuous(limit = c(0,181)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



# QQ plot IV --------------------------------------------------------------

data(alloauto)

# delta: 1 - udalost, 0 cenzura
# type: 1 - allo transplant patients, 2 - auto patients
# time - survival time

status.1<-alloauto$delta[alloauto$type=="1"]
time.1<-alloauto$time[alloauto$type=="1"]
status.2<-alloauto$delta[alloauto$type=="2"]
time.2<-alloauto$time[alloauto$type=="2"]


S1<-f.prezivania(time.1, status.1)$S
T1<-f.prezivania(time.1, status.1)$T
gr1<-quantile(S1, probs = seq(0,1,1/100))
result11<-rep(0, length(gr1))
diff<-rep(0, length(S1))

for (i in 1:length(gr1)){
  for (j in 1:length(S1)){
    diff[j]<-abs(gr1[i]-S1[j])
    result11[i]<-T1[which.min(diff)]
  }
}


S2<-f.prezivania(time.2, status.2)$S
T2<-f.prezivania(time.2, status.2)$T
gr2<-quantile(S2, probs = seq(0,1,1/100))
result22<-rep(0, length(gr2))
diff<-rep(0, length(S2))

for (i in 1:length(gr2)){
  for (j in 1:length(S2)){
    diff[j]<-abs(gr2[i]-S2[j])
    result22[i]<-T2[which.min(diff)]
  }
}

result1<-result11
result2<-result22
df<-data.frame(sort(result1), sort(result2))

ggplot() +
  #geom_point(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  #geom_line(data = df, mapping = aes(x = gr1, y =gr1))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


ggplot(data = df, mapping = aes(x = sort.result1., y =sort.result2.)) +
  #geom_point()+
  geom_smooth(se=FALSE, colour="black")+
  geom_line(data = df, mapping = aes(x = gr1, y =gr1))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  #ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



# QQ plot 5 ---------------------------------------------------------------


data(bmt)
bonemarrow=bmt
names(bonemarrow)=c("g","t1", "t2", "death", "relapse", "dfree", "ta", "a", "tc",
                    "c", "tp", "p", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10")
attach(bonemarrow)

status.1<-bonemarrow$dfree[bonemarrow$z10=="1"]
time.1<-bonemarrow$t2[bonemarrow$z10=="1"]
status.2<-bonemarrow$dfree[bonemarrow$z10=="0"]
time.2<-bonemarrow$t2[bonemarrow$z10=="0"]


S1<-f.prezivania(time.1, status.1)$S
T1<-f.prezivania(time.1, status.1)$T
gr1<-quantile(S1, probs = seq(0,1,1/100))
result11<-rep(0, length(gr1))
diff<-rep(0, length(S1))

for (i in 1:length(gr1)){
  for (j in 1:length(S1)){
    diff[j]<-abs(gr1[i]-S1[j])
    result11[i]<-T1[which.min(diff)]
  }
}


S2<-f.prezivania(time.2, status.2)$S
T2<-f.prezivania(time.2, status.2)$T
gr2<-quantile(S2, probs = seq(0,1,1/100))
result22<-rep(0, length(gr2))
diff<-rep(0, length(S2))

for (i in 1:length(gr2)){
  for (j in 1:length(S2)){
    diff[j]<-abs(gr2[i]-S2[j])
    result22[i]<-T2[which.min(diff)]
  }
}

result1<-result112
result2<-result22
df<-data.frame(sort(result1), sort(result2))

g1<-ggplot(data = df, mapping = aes(x = sort.result1., y =sort.result2.)) +
  #geom_point()+
  #geom_smooth(se=FALSE, colour="black")+
  geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  scale_x_continuous(limit = c(0,115)) +
  scale_y_continuous(limit = c(0,800)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  #ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))

grid.arrange(g1, g2, ncol=2)



# QQ plot VI --------------------------------------------------------------




data(bmt)
bonemarrow=bmt
names(bonemarrow)=c("g","t1", "t2", "death", "relapse", "dfree", "ta", "a", "tc",
                    "c", "tp", "p", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10")
attach(bonemarrow)

status.1<-bonemarrow$dfree[bonemarrow$z8==1]
time.1<-bonemarrow$t2[bonemarrow$z8==1]
status.2<-bonemarrow$dfree[bonemarrow$z8==0]
time.2<-bonemarrow$t2[bonemarrow$z8==0]


S1<-f.prezivania(time.1, status.1)$S
T1<-f.prezivania(time.1, status.1)$T
gr1<-quantile(S1, probs = seq(0,1,1/100))
result11<-rep(0, length(gr1))
diff<-rep(0, length(S1))

for (i in 1:length(gr1)){
  for (j in 1:length(S1)){
    diff[j]<-abs(gr1[i]-S1[j])
    result11[i]<-T1[which.min(diff)]
  }
}


S2<-f.prezivania(time.2, status.2)$S
T2<-f.prezivania(time.2, status.2)$T
gr2<-quantile(S2, probs = seq(0,1,1/100))
result22<-rep(0, length(gr2))
diff<-rep(0, length(S2))

for (i in 1:length(gr2)){
  for (j in 1:length(S2)){
    diff[j]<-abs(gr2[i]-S2[j])
    result22[i]<-T2[which.min(diff)]
  }
}

result1<-result112
result2<-result22
df<-data.frame(sort(result1), sort(result2))

ggplot(data = df, mapping = aes(x = sort.result1., y =sort.result2.)) +
  #geom_point()+
  #geom_smooth(se=FALSE, colour="black")+
  geom_line(data = df, mapping = aes(x = sort.result1., y =sort.result2.))+
  #scale_x_continuous(limit = c(0,12)) +
  #scale_y_continuous(limit = c(0,12)) +
  xlab("Odhadnuté percentily pre skupinu 1")+
  ylab ("Odhadnuté percentily pre skupinu 2") + 
  #ggtitle("Kvantilový graf")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))




# Collet - pr.1.1 - Log_cum_hazard_plot.pdf -------------------------------

#Collet priklad 1.1
#1 pre udalost, 0 pre cenzuru
time<-c(10, 13, 18, 19, 23, 30, 36, 38, 54, 56, 59, 75, 93, 97, 104, 107, 107, 107 )
status<-c(1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0)
x<-rep(1,18)




S.f<-f.prezivania(time,status,type='KM')$S
t.f<-f.prezivania(time,status,type='KM')$T
log_CH<-log(-log(S.f))
plot(log(t.f), log_CH, type="b", pch=19)

df<-data.frame(log(t.f), log_CH )


ggplot(df, aes(x=log.t.f., y=log_CH)) +
  geom_line(linetype = "dashed")+
  geom_point(size=4, colour="white") + 
  geom_point(size=2) + 
  xlab("Logaritmus èasu")+ylab("Log-kumulatívne riziko")+ggtitle("Graf log-kumulatívneho rizika oproti logaritmu èasu")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



fit<-survfit(Surv(time,status)~x,type="kaplan-meier")
log_CH<-log(-log(fit$surv))
plot(log(fit$time), log_CH, type="b", pch=19)

# Priklad - 4 log-kumulativne rizika --------------------------------------

#overenie predpokladov - log hazard plot
#priklad 1.3 str.9
#1 pre udalost, 0 pre cenzuru
time<-c(13, 52, 6, 40, 10, 7, 66, 10, 10, 14, 16, 4, 65, 5, 11, 10, 15, 5, 76, 56, 88, 24, 51, 4, 40, 8, 18, 5,
        16, 50, 40, 1, 36, 5, 10, 91, 18, 1, 18, 6, 1, 23, 15, 18, 12, 12, 17, 3)
status<-c(1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1,
          0, 1, 1, 0)
HB<-c(14.6, 12, 11.4, 10.2, 13.2, 9.9, 12.8, 14, 7.5, 10.6, 11.2, 10.1, 6.6, 9.7, 8.8, 9.6, 13, 10.4, 14, 12.5, 14, 12.4,
      10.1, 6.5, 12.8, 8.2, 14.4, 10.2, 10, 7.7, 5, 9.4, 11, 9, 14, 11, 10.8, 5.1, 13, 5.1, 11.3, 14.6, 8.8, 7.5, 4.9, 5.5,
      7.5, 10.2)
category<-ifelse(HB <= 7, "1", ifelse(HB<=10, "2", ifelse(HB<=13, "3", "4")))


fit<-survfit(Surv(time,status)~category,type="kaplan-meier")
log_CH<-log(-log(fit$surv))
plot(log(fit$time), log_CH, type="b", pch=19, lty=2, xlab="Logaritmus èasu", ylab="Log-kumulatívne riziko",
     main="Graf log-kumulatívneho rizika pre rôzne hodnoty hladiny hemoglobínu")



#priklad 5.8 str. 180
time<-c(59, 20, 71, 33, 25, 25, 15, 53, 47, 10, 4, 16, 18, 19, 35, 11, 88, 70, 54, 139, 31, 59, 111, 149, 30, 44, 25,
        25, 111, 152, 86, 212, 187, 54, 357, 301, 195)
status<-c(rep(1, 17), 0, rep(1, 9), 0, rep(1, 4), 0, rep(1,4))
category<- c(rep("1", 9), rep("2", 7), rep("3", 11), rep("4", 10))


fit<-survfit(Surv(time,status)~category,type="kaplan-meier")
log_CH<-log(-log(fit$surv))
plot(log(fit$time), log_CH, type="b", pch=19, lty=2, xlab="Logaritmus èasu", ylab="Log-kumulatívne riziko",
     main="Graf log-kumulatívneho rizika pre skupiny pod¾a dvoch kategoriálnych premenných")



# Priklad - Allo/Auto -----------------------------------------------------



data(alloauto)
attach(alloauto)
head(alloauto)
# delta: 1 - udalost, 0 cenzura
# type: 1 - allo transplant patients, 2 - auto patients
# time - survival time

fit<-survfit(Surv(alloauto$time,alloauto$delta)~alloauto$type, ctype=1)
log_CH<-log(fit$cumhaz)
plot(fit$time, log_CH, type="s", xlab="Logaritmus èasu", ylab="Log-kumulatívne riziko",
     main="Graf log-kumulatívneho rizika pre skupiny pod¾a dvoch kategoriálnych premenných", xlim=c(-2, 30))

status.1<-alloauto$delta[alloauto$type=="1"]
time.1<-alloauto$time[alloauto$type=="1"]
status.2<-alloauto$delta[alloauto$type=="2"]
time.2<-alloauto$time[alloauto$type=="2"]

fit<-f.prezivania(time.1,status.1,type='KM')

S.f<-fit$S
t.f<-fit$T
log_CH<-log(-log(S.f))

cat<-rep("allo transplantácia", length(t.f))
df1<-data.frame(t.f, log_CH, cat )

fit2<-f.prezivania(time.2,status.2,type='KM')
S.f<-fit2$S
t.f<-fit2$T
log_CH<-log(-log(S.f))
plot(t.f, log_CH, type="b", pch=19)
cat<-rep("auto transplantácia", length(t.f))
df2<-data.frame(t.f, log_CH, cat )
df<-rbind(df2, df1)
df$log<-log(df$t.f)

### Log-cumulative hazard plot ###



g1<-ggplot(df, aes(x=t.f, y=log_CH)) +
   geom_step(aes(linetype = cat))+
  xlim(-1,25)+
  xlab("Èas")+ylab("Log-kumulatívne riziko")+
  #ggtitle("Nesplnený predpoklad")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12)) + theme(legend.position = "none") 

fit<-survfit(Surv(time, delta)~ type, data=alloauto)

g1<-ggsurvplot(
  fit,
  fun = "cloglog",
  size = 0.5,
  censor = FALSE,
  ggtheme = theme_classic(),
  linetype = c("solid", "dashed"),
  palette = c("#222224", "#222224"),
  #legend = "right",
  #legend.title = " ",
  #legend.labs = c("Muži", "Ženy"),
  #xlim = c(1, 1000),
  axis.text = element_text(size = 10),
  axis.title = element_text(size = 12),
  legend.text = element_text(size = 16),
  legend="none",
) + xlab("Logaritmus èasu") + ylab("Log-kumulatívne riziko")




#### Andersenov graf ####


data(alloauto, package="KMsurv")
alloauto1<-alloauto[alloauto$type == "1",]
alloauto2<-alloauto[alloauto$type == "2",]


alloauto11<-alloauto1[alloauto1$delta == "1",]
alloauto22<-alloauto2[alloauto2$delta == "1",]

c<-coxph(Surv(time,delta) ~ strata(type), 
         data=alloauto)
c1 <- coxph(Surv(time,delta) ~ 1, 
            data=alloauto1)
c2 <- coxph(Surv(time,delta) ~ 1, 
           data=alloauto2)
#basehaz1<-basehaz(c1, centered=FALSE)
#basehaz2<-basehaz(c2, centered=FALSE)
#basehaz11<-basehaz1[basehaz1$time %in% alloauto11$time, ]
#basehaz22<-basehaz2[basehaz2$time %in% alloauto22$time, ]
base<-basehaz(c)
strata<-as.character(base$strata)
basehaz1<-base[strata== "type=1",]$hazard
basehaz2<-base[strata== "type=2",]$hazard
b2<-basehaz2[-c(48, 49)]
aux_x<-seq(0,1, length=47)
aux_y<-seq(0,1, length=47)
df<-data.frame(basehaz1, b2, aux_x, aux_y)


plot(basehaz1, b2,type="s", 
     xlim=c(0,1), ylim=c(0,1),
     xlab="Allo", ylab="Auto")
lines(aux_x, aux_y, lty=2)

ggplot(df, aes(x=basehaz1, y=b2)) +
  #geom_line(aes(linetype = cat))+
  geom_step()+
  #geom_line()+
  xlim(c(0,1))+ylim(c(0,1))+
  xlab("Kumulatívne riziko pre skupinu 1")+ylab("Kumulatívne riziko pre skupinu 2")+#ggtitle("Kumulatívne riziká v stratifikovanom Coxovom modeli")+
  theme_classic()+
  geom_abline (slope = 1,
               intercept = 0,
               lty = 2) +
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


#odhad cez cum_risk function (NA)
status.1<-alloauto$delta[alloauto$type=="1"]
time.1<-alloauto$time[alloauto$type=="1"]
status.2<-alloauto$delta[alloauto$type=="2"]
time.2<-alloauto$time[alloauto$type=="2"]

fit<-f.prezivania(time.1,status.1,type='KM')

S.f<-fit$S
t.f<-fit$T
CH<-cum.risk(time.1, status.1)$L
#plot(log(t.f), log_CH, type="b", pch=19)
#cat<-rep("allo", length(t.f))
#df1<-data.frame(log(t.f), log_CH, cat )

fit2<-f.prezivania(time.2,status.2,type='KM')
S.f<-fit2$S
t.f<-fit2$T
CH2<-cum.risk(time.2, status.2)$L
#plot(log(t.f), log_CH, type="b", pch=19)
#cat<-rep("pozitívny", length(t.f))
#df2<-data.frame(log(t.f), log_CH, cat )

df<-cbind(c(CH, rep(0.61998409, 7)), CH2)
plot(df, type="s")
lines(aux_x, aux_y, lty=2)



# Cox-Snell ---------------------------------------------------------------

data(alloauto)
c<-coxph(Surv(time,delta) ~ type, 
         data=alloauto)
mres = resid(c, type="martingale")
# type can be martingale, deviance, score, schoenfeld, dfbeta, dfbetas,
# scaledsch, partial
csres = alloauto$delta-mres
r.surv = survfit(Surv(csres,alloauto$delta)~1,type="fleming-harrington")


seq<-seq(0,1.2, length=59)
df<-data.frame(r.surv$time, r.surv$surv )
df2<-data.frame(seq, seq)

ggplot() +
  geom_step(data = df, mapping = aes(x = r.surv.time, y =-log( r.surv.surv))) +
  geom_line(data = df2, aes(x = seq, y = seq.1), linetype="dashed")+
  #scale_x_continuous(limit = c(0,3.5)) +
  #scale_y_continuous(limit = c(0,3.5)) +
  xlab("Cox-Snellove rezíduá")+
  ylab ("Odhad kumulatívneho rizika na základe rezíduí") + 
  ggtitle("Graf log-kumulatívneho rizika Cox-Snellových rezíduí")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))

# vykreslenie martingalových rezíduí
cox<-coxph(Surv(time,delta) ~ 1, 
         data=alloauto)
alloauto$resid_mart <- residuals(cox, type = "martingale")

ggplot(data = alloauto, mapping = aes(x = type, y = resid_mart)) +
  geom_point() +
  geom_smooth() +
  xlab("Typ transplntácie")+
  ylab ("Martingalové rezíduá") + 
  ggtitle("Graf log-kumulatívneho rizika Cox-Snellových rezíduí")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))




# Priklad - zeny  ---------------------------------------------------------


#Collet example 1.2
time<-c(23, 47, 69, 70, 71, 100, 101, 148, 181, 198, 208, 212, 224, 5, 8, 10, 13, 18, 24, 26, 26, 31, 35, 40, 41, 48, 50, 59,
        61, 68, 71, 76, 105, 107, 109, 113, 116, 118, 143, 154, 162, 188, 212, 217, 225)
status<-c(1,1,1,0,0,0,0,1,1,0,0,0,0, rep(1, 16), 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, rep(0, 6))
HPA<-c(rep("1", 13), rep("2", 32))
#HPA: 1 - negative, 2 - positive
status.1<-status[HPA=="1"]
time.1<-time[HPA=="1"]
status.2<-status[HPA=="2"]
time.2<-time[HPA=="2"]

fit<-f.prezivania(time.1,status.1,type='KM')

S.f<-fit$S
t.f<-fit$T
log_CH<-log(-log(S.f))
plot(log(t.f), log_CH, type="b", pch=19)
cat<-rep("negatívny", length(t.f))
df1<-data.frame(log(t.f), log_CH, cat )

fit2<-f.prezivania(time.2,status.2,type='KM')
S.f<-fit2$S
t.f<-fit2$T
log_CH<-log(-log(S.f))
plot(log(t.f), log_CH, type="b", pch=19)
cat<-rep("pozitívny", length(t.f))
df2<-data.frame(log(t.f), log_CH, cat )

df<-rbind(df2, df1)


g2<-ggplot(df, aes(x=log.t.f., y=log_CH)) +
  geom_step(aes(linetype = cat))+
  xlab("Logaritmus èasu")+ylab("Log-kumulatívne riziko")+
  ggtitle("Splnený predpoklad")+
  theme_classic()+
  theme(legend.title = element_blank())+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))+ theme(legend.position = "none")


wei.lung <- survreg(Surv(time, status) ~ ph.ecog + sex + age,
                    data = lung,
                    dist = 'weibull')

m <- survival::survfit(Surv(time, status) ~ sex, data = lung)

g2<-
  ggsurvplot(
    m,
    fun = "cloglog",
    size = 0.5,
    censor = FALSE,
    ggtheme = theme_classic(),
    linetype = c("solid", "dashed"),
    palette = c("#222224", "#222224"),
    #legend = "right",
    #legend.title = " ",
    #legend.labs = c("Muži", "Ženy"),
    xlim = c(1, 1000),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 16),
    legend="none",
    ) + xlab("Logaritmus èasu") + ylab("Log-kumulatívne riziko")


#ggpubr::ggpar(ggsurv, font.legend = list(size = 12, color = "black"))

grid.arrange(g2$plot, g1$plot, ncol=2)




# Priklad MTX -------------------------------------------------------------


data(bmt)
bonemarrow=bmt
names(bonemarrow)=c("g","t1", "t2", "death", "relapse", "dfree", "ta", "a", "tc",
                    "c", "tp", "p", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10")
attach(bonemarrow)
z11 = z1 - 28;
z22 = z2 - 28;
z1xz2 = z11*z22;
g2 = ifelse(g==2,1,0)
g3 = ifelse(g==3,1,0)
z7c = z7/30-9;
bonemarrow1 = cbind(bonemarrow,z11,z22,z1xz2,g2,g3,z7c)

#Andersen plot
fit1 = coxph(Surv(t2,dfree)~ z11+z22+z1xz2+g2+g3+z7c+z8+strata(z10),
             data = bonemarrow1)
MTX = subset(basehaz(fit1), basehaz(fit1)$strata == "z10=1")
noMTX = subset(basehaz(fit1), basehaz(fit1)$strata == "z10=0")

m <- resid(fit1) #martingale residuals
time<-fit1$y[,1]
delta<-fit1$y[,2]
r <- sign(m)*sqrt(-2*(m + delta*log(delta-m))) #deviance residuals

df<-data.frame(m, r, time, delta)
#1 udalost, 0 cenzura

graf1<-ggplot(data = df) +
  geom_point(data = df, aes(x = time/365.25, y = m, colour=factor(delta))) +
  geom_line(aes(x=seq(0, max(time/365.25)+1, length=dim(df)[1]), y=rep(0, dim(df)[1])), linetype="dashed")+
  xlab("Èas")+
  ylab ("martingalové rezíduá") + 
  scale_colour_manual("", values=c("red", "blue"),
                      breaks = c("0", "1"),
                      labels=c("cenzúra", "udalos"))+  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 12))


graf2<-ggplot(data = df) +
  geom_point(data = df, aes(x = time/365.25, y = r, colour=factor(delta))) +
  geom_line(aes(x=seq(0, max(time/365.25)+1, length=dim(df)[1]), y=rep(0, dim(df)[1])), linetype="dashed")+
  xlab("Èas")+
  ylab ("deviaèné rezíduá") + 
  scale_colour_manual("", values=c("red", "blue"),
                      breaks = c("0", "1"),
                      labels=c("cenzúra", "udalos"))+  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12),
        legend.text = element_text(size = 12))


library(cowplot)
prow <- plot_grid( graf1 + theme(legend.position="none"),
                   graf2 + theme(legend.position="none"),
                   align = 'h',
                   nrow = 1, ncol=2
)

legend_b <- get_legend(graf1 + theme(legend.position="bottom"))
p <- plot_grid( prow, legend_b, ncol=1, rel_heights = c(1, .2))
p


strata<-as.character(base$strata)
basehaz1<-base[strata== "type=1",]$hazard
basehaz2<-base[strata== "type=2",]$hazard
b2<-basehaz2[-c(48, 49)]

aux_x<-seq(0,1, length=47)
aux_y<-seq(0,1, length=47)
df<-data.frame(basehaz1, b2, aux_x, aux_y)


plot(noMTX$hazard[1:length(MTX$hazard)], MTX$hazard,type="s", 
     #xlim=c(0,1), ylim=c(0,1),
     xlab="No MTX", ylab="MTX")
lines(aux_x, aux_y, lty=2)



plot(0,0,lty=1,type="n",xlim = c(0,1200),ylim = c(-4,0.5),
     xlab="Time on study", ylab="Log Cum Hazards")
box()
lines(MTX$time, log(MTX$hazard), lty = 2)
lines(noMTX$time, log(noMTX$hazard),lty =1)



# Cox-Snell rezíduá -------------------------------------------------------

# Figure 11.1

data(bmt)
bonemarrow=bmt
dim(bonemarrow)
names(bonemarrow)=c("g","t1", "t2", "death", "relapse", "dfree", "ta", "a", "tc",
                    "c", "tp", "p", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10")
attach(bonemarrow)
z11 = z1 - 28;
z22 = z2 - 28;
z1xz2 = z11*z22;
g2 = ifelse(g==2,1,0)
g3 = ifelse(g==3,1,0)
z7c = z7/30-9;
bonemarrow1 = cbind(bonemarrow,z11,z22,z1xz2,g2,g3,z7c)
library(survival)
bmt2<-bonemarrow1[dfree==1,]
fit1 = coxph(Surv(t2,dfree)~ z11+z22+z1xz2+g2+g3+z7c+z8+z10, data = bonemarrow1)

mres = resid(fit1, type="martingale")
# type can be martingale, deviance, score, schoenfeld, dfbeta, dfbetas,
# scaledsch, partial
csres = dfree-mres
r.surv = survfit(Surv(csres,dfree)~1,type="fleming-harrington")


seq<-seq(0,4, length=137)
df<-data.frame(r.surv$time, r.surv$surv )
df2<-data.frame(seq, seq)

p2<-ggplot() +
  geom_step(data = df, mapping = aes(x = r.surv.time, y =-log( r.surv.surv))) +
  geom_line(data = df2, aes(x = seq, y = seq.1), linetype="dashed")+
  scale_x_continuous(limit = c(0,3.5)) +
  scale_y_continuous(limit = c(0,3.5)) +
  xlab("Cox-Snellove rezíduá")+
  ylab ("Odhad kumulatívneho rizika na základe rezíduí") + 
  #ggtitle("Nevhodne zvoleného modelu")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))

plot(r.surv$time, (-log(r.surv$surv) - r.surv$time)/r.surv$std.err, type='l', col='red', lwd=2,
     xlab='Residual', ylab=expression((hat(Lambda)(e)-e)/SE))


# Cox-Snell rezíduá 2 -----------------------------------------------------

data(heart)

fit1 <- coxph(formula = Surv(start, stop, event) ~ (age + surgery) * transplant,
              data    = heart,
              ties    = c("efron","breslow","exact")[1])

heart$resid_mart <- residuals(fit1, type = "martingale")

## Cox-Snell residuals
heart$resid_coxsnell <- -(heart$resid_mart - heart$event)


## Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, event) ~ 1,
                      data    = heart,
                      ties    = c("efron","breslow","exact")[1])

## Nelson-Aalen estimator for baseline hazard (all covariates zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)


seq<-seq(0,3, length=137)
df2<-data.frame(seq, seq)

## Plot
ggplot() +
  geom_step(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  geom_line(data = df2, aes(x = seq, y = seq.1), linetype="dashed")+
  scale_x_continuous(limit = c(0,3.5)) +
  scale_y_continuous(limit = c(0,3.5)) +
  labs(x = "Cox-Snell residuals as pseudo observed times",
       y = "Estimated cumulative hazard at pseudo observed times") +
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



# Cox-Snell residuals III -------------------------------------------------

data(cancer)
#fit<-survreg(Surv(time,status)~age+as.factor(sex)+ph.karno,na.action = "na.omit",
            # dist = "exponential"#,data=cancer)
data<-na.omit(cancer)             

fit1 <- coxph(formula = Surv(time,status)~age+as.factor(sex)+ph.karno,
              data    = data,
              ties    = c("efron","breslow","exact")[1])

data$resid_mart <- resid(fit1, type="martingale")

## Cox-Snell residuals
data$resid_coxsnell <- -(data$resid_mart - data$status)-1


## Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
#fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, status) ~ 1,
                     # data    = data,
                      #ties    = c("efron","breslow","exact")[1])

## Nelson-Aalen estimator for baseline hazard (all covariates zero)

df_base_haz <- #basehaz(fit_coxsnell, centered = FALSE)
  survfit(Surv(data$resid_coxsnell,data$status)~1,type="fleming-harrington")
df<-data.frame(df_base_haz$time, df_base_haz$surv )
seq<-seq(0,4, length=137)
df2<-data.frame(seq, seq)

p1<-ggplot() +
  geom_step(data = df, mapping = aes(x = df_base_haz.time, y =-log( df_base_haz.surv))) +
  geom_line(data = df2, aes(x = seq, y = seq.1), linetype="dashed")+
  scale_x_continuous(limit = c(0,3.5)) +
  scale_y_continuous(limit = c(0,3.5)) +
  labs(x = "Cox-Snellove rezíduá",
       y = "Odhad kumulatívneho rizika na základe rezíduí") +
  #ggtitle("Príklad vhodne zvoleného modelu")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))

grid.arrange(p1, p2, ncol=2)

# Slide 9: Alternative display
plot(df_base_haz$time, (-log(df_base_haz$surv) - df_base_haz$time)/-log(df_base_haz$std.err), type='l', col='red', lwd=2, bty='n', las=1, #ylim=c(-3,3),
     xlab='Residual', ylab=expression((hat(Lambda)(e)-e)/SE), xaxt='n')
at <- seq(-4, 1, len=4)
axis(1, at=at, lab=round(exp(at), 2))
abline(h=c(-2,2), col='gray')




# CS IV -------------------------------------------------------------------


data("tongue", package="KMsurv")
fit1 <- coxph(Surv(time, delta)~as.factor(type), data=tongue)


tongue$resid_mart <- residuals(fit1, type = "martingale")

## Cox-Snell residuals
tongue$resid_coxsnell <- -(tongue$resid_mart - tongue$delta)


## Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, delta) ~ 1,
                      data    = tongue,
                      ties    = c("efron","breslow","exact")[1])

## Nelson-Aalen estimator for baseline hazard (all covariates zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)


seq<-seq(0,3, length=137)
df2<-data.frame(seq, seq)

## Plot
ggplot() +
  geom_step(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  geom_line(data = df2, aes(x = seq, y = seq.1), linetype="dashed")+
  scale_x_continuous(limit = c(0,3.5)) +
  scale_y_continuous(limit = c(0,3.5)) +
  labs(x = "Cox-Snell residuals as pseudo observed times",
       y = "Estimated cumulative hazard at pseudo observed times") +
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))





# CS V --------------------------------------------------------------------
# Setup

source("http://myweb.uiowa.edu/pbreheny/7210/f18/notes/fun.R")
PBC <- subset(pbc, !is.na(trt))
PBC <- PBC[order(PBC$time),]
PBC$trt <- PBC$trt == 1
GVHD <- read.delim("https://s3.amazonaws.com/pbreheny-data-sets/gvhd.txt")

# Residuals: Cox-Snell ----------------------------------------------------

# Slide 4: Construction
fit <- coxph(Surv(time/365.25, status!=0) ~ trt + stage + hepato + bili, PBC)
sfit <- survfit(fit)
H0 <- -log(sfit$surv)
H <- approxfun(c(0, sfit$time), c(0, H0), method='constant')
e1 <- H(fit$y[,1])*exp(fit$linear.predictors)
e2 <- fit$y[,2]-residuals(fit)
head(e1)
head(e2)

# Slide 5: Diagnostic plot
efit <- survfit(Surv(e1, fit$y[,2])~1)
lim <- c(0,2)
plot(efit, fun='cumhaz', mark.time=FALSE, bty='n', conf.int=FALSE, lwd=2, las=1,
     xlab='Residual', ylab='Cumulative hazard', xlim=lim, ylim=lim)
ciband(efit, fun=function(x) -log(x))
lines(lim, lim, col='red', lwd=2)



# Martingalove rezíduá ----------------------------------------------------


data(bmt)
bonemarrow=bmt
names(bonemarrow)=c("g","t1", "t2", "death", "relapse", "dfree", "ta", "a", "tc",
                    "c", "tp", "p", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10")
attach(bonemarrow)
z11 = z1 - 28;
z22 = z2 - 28;
z1xz2 = z11*z22;
g2 = ifelse(g==2,1,0)
g3 = ifelse(g==3,1,0)
z7c = z7/30-9;
bonemarrow1 = cbind(bonemarrow,z11,z22,z1xz2,g2,g3,z7c)


fit2 = coxph(Surv(t2,dfree)~ 1, data = bonemarrow1)
bonemarrow1$resid_mart <- residuals(fit2, type = "martingale")
df<- as.data.frame(lowess(z22, bonemarrow1$resid_mart, iter=0))

ggplot(data = bonemarrow1, mapping = aes(x = z22, y = resid_mart)) +
  geom_point() +
  geom_line(data = df, aes(x = x, y = y), linetype="dashed", color="red")+
  xlab("Z")+
  ylab ("martingalové rezíduá") + 
  ggtitle("Graf martingalových rezíduí - kvadratická závislos")+
  theme_classic()+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



# MR II -------------------------------------------------------------------

PBC <- subset(pbc, !is.na(trt))
PBC <- PBC[order(PBC$time),]
PBC$trt <- PBC$trt == 1


fit.pbcO <- coxph(Surv(time, status!=0) ~ 1, data=PBC)
rr <- resid(fit.pbcO) #martingale residuals

plot(PBC$age, rr, xlab="Age", ylab='Residual')
y<-lowess(PBC$age, rr, iter=0)

lines(y, lty=2)
df<-data.frame(PBC$age, rr)
df2<-data.frame(PBC$age, y)

g1<-ggplot(data = df, mapping = aes(x = PBC.age, y = rr)) +
  geom_point() +
  geom_line(data = df2, aes(x = x, y = y), linetype="solid", color="red")+
  xlab(expression(Z[1]))+
  ylab ("Martingalové rezíduá") + 
  #ggtitle("Graf martingalových rezíduí - lineárna závislos")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))




# MR IV -------------------------------------------------------------------

PBC <- subset(pbc, !is.na(trt))
PBC <- PBC[order(PBC$time),]
PBC$trt <- PBC$trt == 1


fit.pbcO <- coxph(Surv(time, status!=0) ~ 1, data=PBC)
rr <- resid(fit.pbcO) #martingale residuals

plot(PBC$age, rr, xlab="Age", ylab='Residual')
y<-lowess(PBC$bili, rr, iter=0)

lines(y, lty=2)
df<-data.frame(PBC$bili, rr)
df2<-data.frame(PBC$bili, y)

g2<-ggplot(data = df, mapping = aes(x = PBC.bili, y = rr)) +
  geom_point() +
  geom_line(data = df2, aes(x = x, y = y), linetype="solid", color="red")+
  xlab(expression(Z[2]))+
  ylab ("Martingalové rezíduá") + 
  #ggtitle("Graf martingalových rezíduí - približne logaritmická závislos")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))
grid.arrange(g1, g2, ncol=2)


# MR III ------------------------------------------------------------------

halibut = read.csv("http://www.karlin.mff.cuni.cz/~pesta/prednasky/NMFM404/Data/halibut.csv", head=T)

fit = coxph(Surv(Time, Death) ~ 1, data=halibut)
halibut$martres = residuals(fit, type="martingale")

#plot(halibut$Deldepth, martres, xlab="Depth Range", ylab="Martingale Residuals")
df<-as.data.frame(lowess(halibut$Deldepth, halibut$martres), col=2)

g1<-ggplot(data = halibut, mapping = aes(x = Deldepth, y = martres)) +
  geom_point() +
  geom_line(data = df, aes(x = x, y = y), linetype="solid", color="red")+
  xlab("Z")+
  ylab ("Martingalové rezíduá") + 
  ggtitle("Pred transformáciou")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


halibut$Depthcat = cut(halibut$Deldepth, breaks=c(0,5,9,11,60))

df<-as.data.frame(lowess(halibut$Depthcat, halibut$martres), col=2)

g2<-ggplot(data = halibut, mapping = aes(x = Depthcat, y = martres)) +
  geom_point() +
  geom_line(data = df, aes(x = x, y = y), linetype="solid", color="red")+
  xlab("Z")+
  ylab ("martingalové rezíduá") + 
  ggtitle("Po transformácii")+
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


grid.arrange(g1, g2, ncol=2)
 

# MT IV -------------------------------------------------------------------



# Sk SCH R I --------------------------------------------------------------

uis <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2016/02/uis-1.csv", header=T)

library(survminer)
data("pbc2.id", package = "JM")
lung$Z2<-lung$age
lung$Z1<-lung$wt.loss

res.cox <- coxph(Surv(time, status) ~ Z2 , data =  lung)
test.ph <- cox.zph(res.cox)

#ggcoxzph(test.ph)


g2<-ggcoxdiagnostics(res.cox, type = "scaledsch",
                 linear.predictions = FALSE, ggtheme = theme_classic(), sline.lty ="solid",  hline.size=0.5, sline.size=0.7,
                 sline.col = "red", hline.col = "black", sline.alpha = 0, caption = "n")+ 
  #ggtitle("Nesplnený predpoklad")+
  xlab("t")+
  ylab (expression(beta[2] (t)))+ theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))



res.cox2 <- coxph(Surv(time, status) ~ Z1, data =  lung)


g1<-ggcoxdiagnostics(res.cox2, type = "scaledsch",
                 linear.predictions = FALSE, ggtheme = theme_classic(), sline.lty ="solid",  hline.size=0.5, sline.size=0.7,
                 sline.col = "red", hline.col = "black", sline.alpha = 0, caption = "n")+ 
  #ggtitle("Splnený predpoklad")+
  xlab("t")+
  ylab (expression(beta[1] (t)))+ theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))


grid.arrange(g1, g2, ncol=2)





resid<-residuals(res.cox  , type='martingale')
time<-res.cox$y[,1]
df2<-data.frame(time, resid)


ggplot(data = df2, mapping = aes(x = time, y = resid)) +
  geom_point() +
  xlab("Cas")+
  ylab ("martingalové rezíduá") + 
  theme_classic()+
  theme(plot.title=element_text(size=16,hjust=0.5),axis.text = element_text(size = 10),axis.title = element_text(size=12))




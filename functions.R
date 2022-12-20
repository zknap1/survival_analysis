#############################################################################################################################################################
#                                                           Funkcie
#############################################################################################################################################################l

pocetnosti<-function(time, status){
  t<-unique(sort(time[status==1]))
  d<-as.numeric(table(time[status==1]))
  n<-as.numeric(rev(cumsum(rev(table(time)))))[unique(sort(time))%in%t]
  tab<-data.frame(t,n,d)
  return(tab)
}


###Odhad funkcie prezivania###


f.prezivania<-function(time,status,type='KM',t=NULL){
  tab<-pocetnosti(time,status)
  m<-min(diff(sort(unique(time))))
  time2<-time-(0.001*m)*runif(length(time))*(status==1)
  tab2<-pocetnosti(time2,status)
  
  if(type=='KM'){S<-cumprod(1-tab$d/tab$n)}
  if(type=='Br'){S<-cumprod(exp(-tab$d/tab$n))}
  if(type=='FHmodB'){S<-exp(-cumsum(tab2$d/tab2$n)[cumsum(tab$d)])}
  
  Tc<-sort(unique(time[status==0]))
  Sc<-NULL
  if(length(Tc)!=0){
    for(i in 1:length(Tc)){Sc[i]<-min(c(S[tab$t<=Tc[i]],1))} }
  s=NULL
  if(!is.null(t)){
    for(i in 1:length(t)){s[i]<-min(c(S[tab$t<=t[i]],1))} }
  return(list(T=tab$t,S=S,Tc=Tc,Sc=Sc,t=t,s=s))}


### Odhad funkcie kumulativneho rizika ###


cum.risk<-function(time,status,type='NA',t=NULL){
  tab<-pocetnosti(time,status)
  m<-min(diff(sort(unique(time))))
  time2<-time-(0.001*m)*runif(length(time))*(status==1)
  tab2<-pocetnosti(time2,status)
  
  if(type=='NA'){L<-cumsum(tab$d/tab$n)}
  if(type=='KM'){L<--cumsum(log(1-tab$d/tab$n))}
  if(type=='FHmodNA'){L<-cumsum(tab2$d/tab2$n)[cumsum(tab$d)]}
  
  Tc<-sort(unique(time[status==0]))
  Lc<-NULL
  if(length(Tc)!=0){
    for(i in 1:length(Tc)){Lc[i]<-max(c(L[tab$t<=Tc[i]],0))} }
  l=NULL
  if(!is.null(t)){
    for(i in 1:length(Tc)){l[i]<-max(c(L[tab$t<=t[i]],0))} }
  return(list(T=tab$t,L=L,Tc=Tc,Lc=Lc,t=t,l=l))}


### odhady rozptylu kumulativnej rizikovej funkcie ###


varcum<-function(time, status, type="Klein",t=NULL){
  tab<-pocetnosti(time,status)
  var.kum.risk<-NULL
  
  if (type=="Klein"){
    var.kum.risk<-cumsum((tab$d*(tab$n-tab$d))/(tab$n^3))}
  
    if (type=="Tsia"){
    var.kum.risk<-cumsum((tab$d)/(tab$n^2))}
  
    if (type=="Green"){
    var.kum.risk<-cumsum((tab$d)/(tab$n*(tab$n-tab$d)))}
  
  if (type=="Bin"){
    var.kum.risk<-cumsum((tab$d)*(tab$n-tab$d)/(tab$n^2*(tab$n-1)))}
  
  if (type=="FHmod"){
    m<-min(diff(sort(unique(time))))
    time2<-time-(0.001*m)*runif(length(time))*(status==1)
    tab2<-pocetnosti(time2,status)
    var.kum.risk<-cumsum(tab2$d^2/tab2$n^2)[cumsum(tab$d)]}
  vart<-NULL
  if(!is.null(t)){
    for(i in 1:length(t)){vart[i]<-max(c(var.kum.risk[tab$t<=t[i]],0))} }
    return(list(var.kum.r=var.kum.risk,var.t=vart))
}


###Odhad rozptylu funkcie prezitia###

var.surv<-function(time,status,type.surv='KM',type.varcum="Green"){
  F<-f.prezivania(time,status,type=type.surv)
  var.s<-(F$S)^(2)*varcum(time,status,type=type.varcum)$var.kum.r
  return(list(var.S.t=var.s, S=F$S, T=F$T))
}

###IS Waldovho typu pre funkciu prezivania na arkus-sinusovej odmocninovej skale##


CI.surv<-function(time,status,type.surv='KM',type.varcum="Green"){
  est<-f.prezivania(time,status,type=type.surv)$S
  d<-(asin(sqrt(est))-qnorm(1-alpha/2)*sqrt((est*varcum(time, status,type=type.varcum)$var.kum.r)/(4*(1-est))))
  ds<-sin(d)^2
  h<-(asin(sqrt(est))+qnorm(1-alpha/2)*sqrt((est*varcum(time, status,type=type.varcum)$var.kum.r)/(4*(1-est))))
  hs<-sin(h)^2
  tab<-data.frame(ds,est,hs)
  return(tab)
}

###IS Waldovho typu pre kumulativne riziko na arkus-sinusovej odmocninovej skale##

CI.cum.risk<-function(time,status,type.cum.risk='NA',typ.var.cum="Klein"){
  odhad<-cum.risk(time,status,type=type.cum.risk)$L
  est<-exp(-cum.risk(time,status,type='NA')$L/2)
  var<-varcum(time, status,type=typ.var.cum)$var.kum.r/(4*(1-exp(-cum.risk(time,status,type=type.cum.risk)$L))*exp(cum.risk(time,status,type=type.cum.risk)$L))
  d<-asin(est)-qnorm(1-alpha/2)*sqrt(var)
  hs<--2*log(sin(d))
  h<-asin(est)+qnorm(1-alpha/2)*sqrt(var)
  ds<--2*log(sin(h))
  tab<-data.frame(ds,odhad,hs)
  return(tab)
}





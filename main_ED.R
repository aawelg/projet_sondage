
##################################################################

#######             ETUDE DESCRIPTIVE                     ########

##################################################################

setwd("F:/Master 2 MIGS/projet/") # chemin du dossier courant

library(ade4)
library(epicalc)

# si la table conso n'existe pas dans le workspace, alors
# on creer la table conso et nconso (sans var. qualitatives) avec la fonction struct_conso
if( exists("conso")=="FALSE" ){
  source('struct_conso.R')
  struct_conso=struct_conso()
  conso=struct_conso$conso; nconso=struct_conso$nconso
  n=struct_conso$n; p=struct_conso$p
}

### creation des classes de consommateurs:  Autres | PM entreprises | Résidentiels 
class.contrat=conso[,1]
fac=as.factor(class.contrat)
levels=c("Autres","PM entreprises","Résidentiels")

### moyenne, variance, minimum et maximum de la conso. prises toutes les 30 minutes
# en fonction des classes.
moy.conso=aggregate(nconso,list(class.contrat),mean)
var.conso=aggregate(nconso,list(class.contrat),var)
min.conso=aggregate(nconso,list(class.contrat),min)
max.conso=aggregate(nconso,list(class.contrat),max) 

### boxplot de la conso moyenne par jours pour chaque classe
index=seq(0,672,48) # les indices des var. representant les jours dans la table nconso
j=1:14 # nombre de jours
par(mfrow=c(3,1))
for ( i in 1:3 ){
  k = levels[i] #  k = Autres | PM entreprises | Résidentiels 
  aux = factor( (fac%in%k)*1 ) 
  nk = dim(nconso[aux%in%1,])[1]
  mat = matrix(  sapply( j, function(x) apply(nconso[aux%in%1,index[x]:index[(x+1)]],1,mean) )
                 , nrow=nk, ncol=14 )
  boxplot( mat,xlab="jours",ylab="conso d'électricité (KWh)" )
  title(k,cex.main = 2)
}

# conso. moyenne d'électricité (KWh) en fonction du temps
par(mfrow=c(3,1))
for ( k in levels ){
  aux = factor( (fac%in%k)*1 ) 
  plot( apply(nconso[aux%in%1,],2,mean),col="black",type="l",xlab="t (minutes)",lwd=1.5,ylab="conso moy d'électricité (KWh)" )
  sapply( index,function(x) abline(v=x,col="dark red") )
  title(k,cex.main = 2)
}


### mesure de la dispersion
var.ind.conso=apply(nconso,1,var)
aggregate.plot(var.ind.conso,list(class.contrat),bar.col=1:3, legend = FALSE, main="")


###             ACP    
conso.acp=dudi.pca(nconso,center=TRUE,scale=TRUE,scannf=FALSE,nf=15)


### part de la variance --> explique les axes en composantes principales 1,2
par(mfrow=c(1,1))
prop.valp=conso.acp$eig/sum(conso.acp$eig) 
prop.valp=prop.valp*100
barplot(prop.valp[1:5],ylim=c(0,100),names.arg=c("1","2","3","4","5"),
        xlab="composantes principales",ylab="inertie expliquée en %")
title("Part de l'intertie")

### individus dans le plan 1,2
s.class(conso.acp$li[,1:2],fac,col=1:3,cellipse=0,cstar=0)

### individus par classe dans le plan 1,2
for (i in levels){ 
  aux <- factor((fac%in%i)*1) 
  color=switch(i,"Autres"="black", "PM entreprises"="red", "Résidentiels"="green")
  plot(conso.acp$li[aux%in%1,1],conso.acp$li[aux%in%1,2],pch=18,col=color,
       xlab="axe 1",ylab="axe 2")
  abline(h = 0, col = "black", lty = "dotted")
  abline(v = 0, col = "black", lty = "dotted")
  title(i,cex.main = 2)
}

### cercle de correlation matin-midi/midi-soir pour chaque jours dans le plan 1,2
xmin=1; xmax=48; xinter=24
for(i in 1:14){
  par(mfrow=c(2,1))
  s.corcircle(conso.acp$co[xmin:xinter,1:2],cgrid = 0, full = FALSE, box = TRUE, clab = 0.8)
  s.corcircle(conso.acp$co[xinter:xmax,1:2], cgrid = 0, full = FALSE, box = TRUE, clab = 0.8)
  xmin=xmax+1; xinter=xmin+23; xmax=xinter+24
}








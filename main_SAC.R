 

#############################################################################

#######      CALAGE SUR CP ENTRE 2 et 336 AVEC UN PLAN SAS           ########

#############################################################################

setwd("F:/Master 2 MIGS/projet/") # chemin du dossier courant

# si la table conso n'existe pas dans le workspace, alors
# on creer la table conso et nconso (sans var. qualitative) avec la fonction struct_conso
if( exists("conso")=="FALSE" ){
  source('struct_conso.R')
  struct_conso=struct_conso()
  conso=struct_conso$conso; nconso=struct_conso$nconso
}

source('echantillonage.R')
source('calage_fun.R')
source('main_calage.R')

# install.packages("ade4")
library(ade4)
library(MASS)

N=nrow(nconso) # U
n=600 # S
P=ncol(nconso) # nombre de variable 
p=336 # nombre de variable auxiliaire de depart
sem1.var=1:p # indice des var de la sem1
sem2.var=(p+1):P # indice des var de la sem2
X_U=as.matrix(nconso[,sem1.var]) # conso des ind de U pour la sem1
Y_U=as.matrix(nconso[,sem2.var])  # conso des ind de U pour la sem2
yk=apply(Y_U,1,sum) # conso de l'individu k dans U pdt la sem2
ty=sum(yk) # conso total sem2
dk=N/n # l'inverse de la probabilite d'inclusion pour un plan p(.) SAS


# ACP centree reduite sur les variables auxiliaires
#--------------------------------------------------
conso.acp=dudi.pca(X_U,center=TRUE,scale=TRUE,scannf=FALSE,nf=p)

#
lnb.cp=seq(2,p,by=10) # le nombre de CP a garder, compris entre 2 et 332
lest=list()
k=1 

  for( pj in lnb.cp ){    
    X_U=-conso.acp$li[,1:pj] # matrice des var. auxiliaires sur les CP
    sem1.var=1:pj  # indice des var de la sem1 
    ni=10 # nombre d'echantillons I de taille n=600
    i=seq(1,ni,1) 
    lest[[k]]=sapply( i,function(x) main_calage(nconso,X_U,N,n,dk,sem1.var,sem2.var,"SAC") )
    k=k+1
    }

i=seq(1,(k-1),1)
est.ty=sapply( i,function(x) unlist(lest[[x]][1,]) ) # ni estimation par calage pour la ieme CP
est.hty=sapply( i,function(x) unlist(lest[[x]][2,]) ) # ni estimation par HT pour la ieme CP
mest.ty=sapply( i,function(x) mean(unlist(lest[[x]][1,])) )

tty = rep( ty, ni )
R=sapply( i,function(x) sum((est.ty[,x] - tty)^2) / sum((est.hty[,x] - tty)^2) )  
cv=sapply( i, function(x) sqrt( var(est.ty[,x]) ) / mest.ty[x] )


# representation de la qualite de l'estimateur en fonction du nombre de CP
#--------------------------------------------------------------------------

# affichage du coefficient R en fonction du nb de CP
plot(lnb.cp,R, type="p", col="red",xlab="nombre de CP")
lines(lnb.cp,R,col="black")

# affichage du coefficient de variation cv en fonction du nb de CP
plot(lnb.cp,cv, type="p", col="red",xlab="nombre de CP",ylab="cv")
lines(lnb.cp,cv,col="black")

# affichage de l'estimateur moyen en fonction du nb de CP
plot(lnb.cp,mest.ty, type="p", col="red",xlab="nombre de CP",ylab="estimateur moyen du total de la consommation")
lines(lnb.cp,mest.ty,col="black")
lines(2:p,rep(ty,(p-1)),col="red")



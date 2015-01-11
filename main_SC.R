


##################################################################

#######               CALAGE AVEC UN PLAN SAS             ########

##################################################################


setwd("F:/Master 2 MIGS/projet/")  # chemin du dossier courant

# si la table conso n'existe pas dans le workspace, alors
# on creer la table conso et nconso (sans var. qualitative) avec la fonction struct_conso
if( exists("conso")=="FALSE" ){
  source('struct_conso.R')
  struct_conso=struct_conso()
  conso=struct_conso$conso; nconso=struct_conso$nconso
}

source('echantillonage.R'); source('calage_fun.R'); source('main_calage.R')
library(MASS)

N=nrow(nconso) # U
n=600 # S
P=ncol(nconso) # nombre de variables
p=336 # nombre de variables auxiliaires
sem1.var=1:p # indice des var de la sem1
sem2.var=(p+1):P # indice des var de la sem2
X_U=as.matrix(nconso[,sem1.var]) # conso des ind de U pour la sem1
Y_U=as.matrix(nconso[,sem2.var])  # conso des ind de U pour la sem2
yk=apply(Y_U,1,sum) # conso de l'individu k dans U pdt la sem2
ty=sum(yk) # conso total sem2
dk=N/n # l'inverse de la probabilité d'inclusion pour un plan p(.) SAS 

ni=10 # nombre d'echantillons I de taille n=600
i=seq(1,ni,1)  
lest=sapply( i,function(x) main_calage(nconso,X_U,N,n,dk,sem1.var,sem2.var,"SC") )

est.ty=unlist(lest[1,]); est.hty=unlist(lest[2,])

tty = rep( ty, ni )
R = sum( (est.ty - tty)^2 ) / sum( (est.hty - tty)^2 )
cv = sqrt( var(est.ty) ) / mean(est.ty)

cat( sprintf( "\n L'estimateur moyen de la conso totale de la semaine 2 est de %d \n
              La qualite R de l'estimateur est de %f \n
              Le coefficient de variation est de %f \n" , round(mean(est.ty)), R, cv) )






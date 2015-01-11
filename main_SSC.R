
# ##################################################################
# 
# #######           STRATIFICATION + SAS + CALAGE           ########
# 
# ##################################################################

setwd("F:/Master 2 MIGS/projet/") # chemin du dossier courant

# si la table conso n'existe pas dans le workspace, alors
# on creer la table conso et nconso (sans var. qualitative) avec la fonction struct_conso
if( exists("conso")=="FALSE" ){
  source('struct_conso.R')
  struct_conso=struct_conso()
  conso=struct_conso$conso; nconso=struct_conso$nconso
  n=struct_conso$n; p=struct_conso$p
}

source('echantillonage.R'); source('main_calage.R'); source('calage_fun.R')
library(MASS)

N=nrow(nconso) # U
n=600; # S
P=ncol(nconso) # nombre total de variables
p=336 # nombre de variables auxiliaires
sem1.var=1:p # indice des var de la sem1
sem2.var=(p+1):P # indice des var de la sem2

class.contrat=conso[,1]
fac=as.factor(class.contrat)
levels=c("Autres","PM entreprises","Résidentiels")

sem2.var=(p+1):P # indice des var de la sem2
Y_U=as.matrix(nconso[,sem2.var]) 
yk=apply(Y_U,1,sum) # conso de l'individu k dans U pdt la sem2
ty=sum(yk) # conso total sem2


lest.ty=list(); lest.hty=list()

  for(j in 1:10){   
    twy=list(); htwy=list(); k=1
  
      for(i in levels){
        aux = factor( (fac%in%i)*1 )
        Nh = length( which(aux==1) ) # Uh
        nh = round(n*Nh/N) # Sh 
        dk = Nh/nh # l'inverse de la probabilité d'inclusion pour p(.) SAS pour la k-ieme strat
        
        mat = as.matrix(nconso[aux%in%1,]) # matrice de données pour la k-ieme strat
        X_U = mat[,sem1.var]; Y_U=mat[,sem2.var] 
     
        lest = main_calage(mat,X_U,Nh,nh,dk,sem1.var,sem2.var,"SSC")
        
        twy[[k]] = lest$est.ty # estimation par calage pour la k-ieme strat
        htwy[[k]] = lest$est.hty # estimation par horvitz-thompson pour la k-ieme strat
        k=k+1
      }
    
    lest.ty[[j]] = sum( unlist(twy) ) # j-ieme estimation par calage
    lest.hty[[j]] = sum( unlist(htwy) ) # j-ieme estimation par horvitz-thompson
  } 

est.ty=unlist(lest.ty); est.hty=unlist(lest.hty)
tty = rep( ty, length(est.ty) )
R = sum( (est.ty - tty)^2 ) / sum( (est.hty - tty)^2 )
cv = sqrt( var(est.ty) ) / mean(est.ty)

cat( sprintf( "\n L'estimateur moyen de la conso totale de la semaine 2 est de %d \n
              La qualite R de l'estimateur est de %f \n
              Le coefficient de variation est de %f \n" , round(mean(est.ty)), R, cv) )



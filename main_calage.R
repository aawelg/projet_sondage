#
# main_calage permet de calculer les estimateurs de calage et de horvitz-thompson
#
#=> en entrée :
# ~ tab : la matrice contenant toutes les données
# ~ X_U : la matrice contenant l'information auxiliaire dans U
# ~ N : la taille de la population U   # ~ n : la taille de la pop. S
# ~ dk : l'inverse de la probabilité d'inclusion pour un plan p(.) SAS 
# ~ sem1.var : les indices des var. de la semaine 1
# ~ sem2.var : les indices des var. de la semaine 2
# ~ nom_plan : SC = SAS+Calage | SSC = Strat+SAS+Calage | SAC = SAS+ACP+Calage

main_calage=function(tab,X_U,N,n,dk,sem1.var,sem2.var,nom_plan){ 

echt = echantillonage(tab,X_U,N,n,sem1.var,sem2.var,nom_plan) # echantillonage
X_S = echt$X_S; Y_S=echt$Y_S
  
yk=apply(Y_S,1,sum) # conso. de l'individu k dans S pdt la sem2

calage.conso = calage_fun(dk,X_S,X_U) # fonction de calage

est.ty = calage.conso$wk %*% yk # estimateur de calage
est.hty = sum(dk*yk)  # estimateur de hovitz-thompson

list( est.ty=est.ty, est.hty=est.hty)
}
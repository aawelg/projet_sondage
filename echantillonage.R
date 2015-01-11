#
# echantillonage renvoit les matrices X_S et Y_S prises aléatoirement dans U 
#
#=> en entrée :
# ~ tab : la matrice contenant toutes les données
# ~ X_U : la matrice contenant l'information auxiliaire dans U
# ~ N : la taille de la population U   # ~ n : la taille de la pop. S
# ~ sem1.var : les indices des var. de la semaine 1
# ~ sem2.var : les indices des var. de la semaine 2
# ~ nom_plan : SC = SAS+Calage | SSC = Strat+SAS+Calage | SAC = SAS+ACP+Calage

echantillonage=function(tab,X_U,N,n,sem1.var,sem2.var,nom_plan){
    
  rand.ind=sample(1:N, n, replace=F) # indice des individus appartenant a S pris aleatoirement
  
  if(nom_plan=="SC" | nom_plan=="SSC" ){ # plan SC ou SSC
    X_S=as.matrix(tab[rand.ind,sem1.var])  # conso. des ind. de S pour la sem1  
  }
  else{ # plan SAC
    X_S=as.matrix(X_U[rand.ind,])  }  # conso. des ind. de S pour la sem1  

  Y_S=as.matrix(tab[rand.ind,sem2.var])  # conso. des ind. de S pour la sem2

  list( X_S=X_S, Y_S=Y_S)
}
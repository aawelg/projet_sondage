#
# calage_fun permet de calculer les poids de calage
# 
#=> en entrée :
# ~ dk : l'inverse de la probabilité d'inclusion pour un plan p(.) SAS 
# ~ X_S : la matrice contenant l'information auxiliaire dans S
# ~ X_U : la matrice contenant l'information auxiliaire dans U

calage_fun=function(dk,X_S,X_U){
    
   tx = apply(X_U,2,sum) # vecteur contenant les totaux pour l'info. aux. dans U
   twdx = apply(X_S,2,sum) # vecteur contenant les totaux pour l'info. aux. dans S
   txtwdx = tx - dk*twdx 
   mat = ginv(dk * crossprod(X_S,X_S)) 
   wk = dk + dk*(txtwdx %*% mat) %*% t(X_S) # poids de calage
   list(wk=wk)
   
  }
  
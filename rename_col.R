#
# rename_col creer un nouveau format "JOURS/ HEURE" pour le nom des variables de la table conso
#

rename_col=function(conso,p){
  
v=rep(0,p)
h=0; m=0; j=5
v[1]="5/ 0:0"

for( i in 1:(p-1) ){
  
  if(i%%48!=0){
      m=m+30
      if(m==60){ m=0; h=h+1;}
      }
   else{ j=j+1; h=0; m=0;}
  v[i+1]=paste(c(as.character(j),"/ ",as.character(h),":",as.character(m)),sep=" ",collapse="")
  
  }

  v=c("code",v);  colnames(conso)=v;
  
  return(conso)
}



#
# struct_conso permet de structurer la table "smart.278co.csv" avec "contrat.smartco.csv"
#
#=> en sortie :
# ~ conso : table des conso. des ind. classés par contrat
# ~ nconso : table uniquement avec les conso. des ind.

struct_conso=function(){
  
  source('rename_col.R')

  smart=read.table('smart.278co.csv', header = TRUE, sep=",")
  smart[,1]=1:nrow(smart)
  
  contrat=read.table('contrat.smartco.csv', header = TRUE, sep=",")
  
  # renomme la table contrat, 1:Autres, 2:PM entreprises, 3:Résidentiels
  levels=c("Autres","PM entreprises","Résidentiels")
    for( i in 1:3){ contrat[which(contrat[,2]==i),2]=levels[i]; } 

  tab.conso=merge(smart,contrat, by="X") # fusion de la table conso. des ind. avec la table contrat
  tab.conso[,1]=tab.conso[,ncol(tab.conso)]
  tab.conso=tab.conso[,-ncol(tab.conso)]
  
  # renomme les colonnes de la table conso, nouveau format: JOURS/HEURE
  conso=rename_col(tab.conso,p=(ncol(tab.conso)-1)) 
  nconso=conso[,-1] # table conso sans var. qualitatives

list(conso=conso,nconso=nconso)
}

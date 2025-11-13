
#**Read me*
#*Ce code a été rédigé dans le cadre du projet de modélisation statistique du S7 2025-26. 
#*Groupe : Noé Peyrot, Lucia Dufond-Gonzalez, Mattieu Marchais, Julia Zitouni--Flambard
#*Consigne générale : Expliquer les fluctuations de l'aire de Broca à gauche au cours d'une tâche de production langagière,
#*à l'aide d'autres variables du jeu de données, pour mieux comprendre l'interaction des différentes aires cérébrales entre elles. 


# clean workspace & set up space -----------------------------------

rm(list = ls())
try(dev.off())
cat("\014")

set.seed(123)

library(tibble)
library(tidyr)
library(dplyr)
library(PCAmixdata)

#Get current location
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

wd <- getCurrentFileLocation()
setwd(wd)


activation <- readRDS('activation.Rdata')

# PARTIE 1 : VISUALISATION DES DONNÉES -------------------------------


activation_H <- activation[activation$Sexe=="H",]
activation_F <- activation[activation$Sexe=="F",]




# PARTIE 2 : ACP -------------------------------



resACP <- PCAmix(activation[,-(1:2)], graph=FALSE)


cat('\n\n--------- ACP -----------\n\n\tCalcul des Eingen values :\n\n')
# Pour préparer l'ACP, on regarde les Eingen values
print(round(resACP$eig, digits=2))

# et mtnt on plot
plot(resACP,axes=c(1,2), coloring.ind=activation$Sexe, main="ACP, répartition H-F pour dim 1-2")
plot(resACP,axes=c(1,2),choice="cor", lim.cos2.plot = 0.2, main="ACP dim 1-2")
plot(resACP,axes=c(3,4),choice="cor", lim.cos2.plot = 0.2, main="ACP dim 3-4")
plot(resACP,axes=c(1,3),choice="cor", lim.cos2.plot = 0.2,main="ACP dim 1-3")


plot(activation$PROD_G_Frontal_Inf_Tri_1_L, activation$PROD_G_Rolandic_Oper_1_L)
plot(activation$PROD_G_Frontal_Inf_Tri_1_L, activation$PROD_S_Sup_Temporal_4_L)
plot(activation$PROD_G_Frontal_Inf_Tri_1_L, activation$PROD_G_Frontal_Inf_Tri_1_R)


# et on regarde le détail de quelle variable apparait sur quoi 
print(round(resACP$quanti$cos2, digits=2))

# PARTIE 3 :  -------------------------------



# PARTIE 4 :  -------------------------------



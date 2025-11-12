
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

resACP <- PCAmix(activation[,-(1:2)], graph=FALSE)

cat('\n\n--------- ACP -----------\n\n\tCalcul des Eingen values :\n\n')
# Pour préparer l'ACP, on regarde les Eingen values
print(round(resACP$eig, digits=2))

# on plot jusqu'à la dimension 5. 
plot(resACP,choice="cor", axes=c(1:5)) 





# PARTIE 2 : ACP -------------------------------




# PARTIE 3 :  -------------------------------



# PARTIE 4 :  -------------------------------



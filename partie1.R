# ========== PARTIE 1 ==========

# On importe les données.
donnees <- readRDS("C:/Users/peyro/Desktop/Stat/activation.Rdata")
attach(donnees)

# Aperçu général des données : 
head(donnees)
str(donnees)
summary(donnees)
dim(donnees)

# Nuage de points du volume cérébral en fonction de l'âge : 
plot(Age, Volume_Cerebral,
     main = "Volume cérébral en fonction de l'âge",
     xlab = "Âge (années)",
     ylab = "Volume cérébral",
     pch = 19,
     col = "lightsalmon") 

# Boxplot du volume cérébral en fonction de l'âge : 
boxplot(Volume_Cerebral ~ Sexe, data = donnees,
        main = "Volume cérébral selon le sexe",
        xlab = "Sexe",
        ylab = "Volume cérébral",
        col = c("pink", "lightblue"))

# Données quantitatives du volume cérébral : 
summary(Volume_Cerebral)

# Boxplots des deux gyrus frontaux inférieurs triangulaires : 
boxplot(PROD_G_Frontal_Inf_Tri_1_L, PROD_G_Frontal_Inf_Tri_1_R,
        names = c("Gyrus gauche", "Gyrus droit"),
        main = "Comparaison des deux gyrus frontaux inférieurs triangulaires",
        ylab = "Activation",
        col = c("springgreen4", "navyblue"))

# Histogramme de l'index de latéralisation hémisphérique 
hist(Index_Lateralisation_Hemispherique,
     main = "Distribution de l'index de latéralisation hémisphérique (ILH)",
     xlab = "ILH",
     col = "lavender", breaks = 20)

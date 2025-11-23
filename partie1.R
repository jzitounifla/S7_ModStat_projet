
# On importe les données
activation <- readRDS("C:/Users/peyro/Desktop/Stat/activation.Rdata")
attach(activation)


# ===========================================================================
# Partie 1 : Analyse descriptive des données 
# ===========================================================================

# Aperçu général des données : 
head(activation)
str(activation)
summary(activation)
dim(activation)

# Nuage de points du volume cérébral en fonction de l'âge : 
plot(Age, Volume_Cerebral,
     main = "Volume cérébral en fonction de l'âge",
     xlab = "Âge (années)",
     ylab = "Volume cérébral",
     pch = 19,
     col = "lightsalmon") 

# Boxplot du volume cérébral en fonction de l'âge : 
boxplot(Volume_Cerebral ~ Sexe, data = activation,
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


# ===========================================================================
# Partie 2 : ACP des activations cérébrales
# ===========================================================================

library(PCAmixdata)

# On retire les deux premières colonnes (Sujet et Sexe) pour ne garder que les variables quantitatives
resACP <- PCAmix(activation[,-(1:2)], graph=FALSE)

# Valeurs propres de l'ACP pour identifier le nombre de composantes à retenir
resACP_eig <- round(resACP$eig, digits=2)

# Visualisation des individus selon les axes principaux, colorés par sexe
plot(resACP,axes=c(1,2), coloring.ind=Sexe, main="ACP, Répartition H-F pour dim 1-2")

# Cercles des corrélations pour les dimensions 1-2, 3-4 et 1-3
plot(resACP,axes=c(1,2),choice="cor", lim.cos2.plot = 0.2, main="ACP dim 1-2")
plot(resACP,axes=c(3,4),choice="cor", lim.cos2.plot = 0.2, main="ACP dim 3-4")
plot(resACP,axes=c(1,3),choice="cor", lim.cos2.plot = 0.2,main="ACP dim 1-3")

# Nuages de points pour visualiser les relations entre l'aire de Broca gauche et d'autres régions
plot(PROD_G_Frontal_Inf_Tri_1_L, PROD_G_Rolandic_Oper_1_L)
plot(PROD_G_Frontal_Inf_Tri_1_L, PROD_S_Sup_Temporal_4_L)
plot(PROD_G_Frontal_Inf_Tri_1_L, PROD_G_Frontal_Inf_Tri_1_R)

# Vérification de la qualité de représentation des variables selon les dimensions (cos²) 
resACP_cos2 <- round(resACP$quanti$cos2, digits=2)


# ===========================================================================
# Partie 3 : Régression linéaire multiple
# ===========================================================================

modele <- lm(PROD_G_Frontal_Inf_Tri_1_L ~ . - Sujet, data = activation)
summary(modele)

# Sélection automatique des variables les plus pertinentes via le critère AIC
step(modele)

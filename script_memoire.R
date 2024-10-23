################################################################################
#Script Mémoire
################################################################################

#Package
library(tseries)
library(urca)
library(vars)
library(tsDyn)
library(ARDL)
library(PerformanceAnalytics)
library(EnvStats)
library(ggplot2)
library(outliers)
library(corrplot)
library(lmtest)



#Chargement de la base de données
base<-read.csv2("C:/Users/Admi/Documents/memoire_alimentation/conso_alimentation.csv", sep=";")

#Transformation des variables en variables numériques
base <- as.data.frame(lapply(base, as.numeric))
str(base)

#Création d'une base de données traitant sur la répartition des dépenses.
i<-1
j<-2
base2<-base[,1:2]
base2<-as.data.frame(base2)
str(base2)
for (i in 1:62){
  for (j in 3:14){
    base2[i,j]<-(base[i,j]/base[i,15])*100
  }
}
k<-1
p<-2
for (k in 1:62){
  base2[k,15]<-0
  for(p in 3:14){
    base2[k,15]<-base2[k,15]+base2[k,p]
  }
}

colnames(base2)<-c("Annee","Inflation", "Cereales","Viande","Poisson","Laitage","Huiles","Fruits","Legumes","Sucrerie","Epicerie","Cafe_cacao","Boissons_non_alcoolisees","Boissons_alcoolisees", "Total")
View(base2)
#Transformation des variables en série temporelle
INF<-ts(base2[,2], start=1959,end=2020, frequency=1)
cereales<-ts(base2[,3], start=1959,end=2020, frequency=1)
viande<-ts(base2[,4], start=1959,end=2020, frequency=1)
poisson<-ts(base2[,5], start=1959,end=2020, frequency=1)
laitage<-ts(base2[,6], start=1959,end=2020, frequency=1)
huiles<-ts(base2[,7], start=1959,end=2020, frequency=1)
fruits<-ts(base2[,8], start=1959,end=2020, frequency=1)
legumes<-ts(base2[,9], start=1959,end=2020, frequency=1)
sucrerie<-ts(base2[,10], start=1959,end=2020, frequency=1)
epices<-ts(base2[,11], start=1959,end=2020, frequency=1)
cafe_cacao<-ts(base2[,12], start=1959,end=2020, frequency=1)
boissons_na<-ts(base2[,13], start=1959,end=2020, frequency=1)
boisson_a<-ts(base2[,14], start=1959,end=2020, frequency=1)

################################################################################

###Analyse descriptive des séries

##INF
#graphique
plot(INF, xlab="Date", ylab="INF", col="blue")
#Boîte à moustache
boxplot(INF, main="Répartition de l'inflation", col = "lightblue")
rosnerTest(INF, k = 3, alpha = 0.05) #pas de valeurs atypiques
#Histogramme
hist(INF, col = "lightblue", main="Histogramme de l'inflation")
# Statistiques générales
summary(INF)
#ecart-type:
round(sd(INF),3)
#kurtosis:
INF2<-as.vector(INF)
round(kurtosis(INF2),3)
#skewness:
round(skewness(INF2),3)
#test de normalité:
shapiro.test((INF))
#test de KS
ks.test(INF, "pnorm", mean = 0, sd = 1)

##cereales
#graphique
plot(cereales, xlab="Date", ylab="dépenses céreales", col="blue")
#Boîte à moustache
boxplot(cereales, main="Répartition des dépenses en céreales", col = "lightblue")
rosnerTest(cereales, k = 2, alpha = 0.05) #une valeur atypique
#Histogramme
hist(cereales, col = "lightblue", main="Histogramme des dépenses en céreales")
# Statistiques générales
summary(cereales)
#ecart-type:
round(sd(cereales),3)
#kurtosis:
cereales2<-as.vector(cereales)
round(kurtosis(cereales2),3)
#skewness:
round(skewness(cereales2),3)
#test de normalité:
shapiro.test((cereales))
#test de KS
ks.test(cereales, "pnorm", mean = 0, sd = 1)

##viande
#graphique
plot(viande, xlab="Date", ylab="dépenses viande", col="blue")
#Boîte à moustache
boxplot(viande, main="Répartition des dépenses en viande", col = "lightblue")
#Histogramme
hist(viande, col = "lightblue", main="Histogramme des dépenses en viande")
# Statistiques générales
summary(viande)
#ecart-type:
round(sd(viande),3)
#kurtosis:
viande2<-as.vector(viande)
round(kurtosis(viande2),3)
#skewness:
round(skewness(viande2),3)
#test de normalité:
shapiro.test((viande))
#test de KS
ks.test(viande, "pnorm", mean = 0, sd = 1)

##poisson
#graphique
plot(poisson, xlab="Date", ylab="dépenses poisson", col="blue")
#Boîte à moustache
boxplot(poisson, main="Répartition des dépenses en poisson", col = "lightblue")
#Histogramme
hist(poisson, col = "lightblue", main="Histogramme des dépenses en poisson")
# Statistiques générales
summary(poisson)
#ecart-type:
round(sd(poisson),3)
#kurtosis:
poisson2<-as.vector(poisson)
round(kurtosis(poisson2),3)
#skewness:
round(skewness(poisson2),3)
#test de normalité:
shapiro.test((poisson))
#test de KS
ks.test(poisson, "pnorm", mean = 0, sd = 1)

##laitage
#graphique
plot(laitage, xlab="Date", ylab="dépenses laitages", col="blue")
#Boîte à moustache
boxplot(laitage, main="Répartition des dépenses en laitage", col = "lightblue")
#Histogramme
hist(laitage, col = "lightblue", main="Histogramme des dépenses en laitage")
# Statistiques générales
summary(laitage)
#ecart-type:
round(sd(laitage),3)
#kurtosis:
laitage2<-as.vector(laitage)
round(kurtosis(laitage2),3)
#skewness:
round(skewness(laitage2),3)
#test de normalité:
shapiro.test((laitage))
#test de KS
ks.test(laitage, "pnorm", mean = 0, sd = 1)

##huiles
#graphique
plot(huiles, xlab="Date", ylab="dépenses huiles", col="blue")
#Boîte à moustache
boxplot(huiles, main="Répartition des dépenses en huiles", col = "lightblue")
#Histogramme
hist(huiles, col = "lightblue", main="Histogramme des dépenses en huiles")
# Statistiques générales
summary(huiles)
#ecart-type:
round(sd(huiles),3)
#kurtosis:
huiles2<-as.vector(huiles)
round(kurtosis(huiles2),3)
#skewness:
round(skewness(huiles2),3)
#test de normalité:
shapiro.test((huiles))
#test de KS
ks.test(huiles, "pnorm", mean = 0, sd = 1)

##fruits
#graphique
plot(fruits, xlab="Date", ylab="dépenses fruits", col="blue")
#Boîte à moustache
boxplot(fruits, main="Répartition des dépenses en fruits", col = "lightblue")
rosnerTest(fruits, k = 7, alpha = 0.05) #pas de valeurs atypiques
#Histogramme
hist(fruits, col = "lightblue", main="Histogramme des dépenses en fruits")
# Statistiques générales
summary(fruits)
#ecart-type:
round(sd(fruits),3)
#kurtosis:
fruits2<-as.vector(fruits)
round(kurtosis(fruits2),3)
#skewness:
round(skewness(fruits2),3)
#test de normalité:
shapiro.test((fruits))
#test de KS
ks.test(fruits, "pnorm", mean = 0, sd = 1)

##legumes
#graphique
plot(legumes, xlab="Date", ylab="dépenses légumes", col="blue")
#Boîte à moustache
boxplot(legumes, main="Répartition des dépenses en légumes", col = "lightblue")
#Histogramme
hist(legumes, col = "lightblue", main="Histogramme des dépenses en légumes")
# Statistiques générales
summary(legumes)
#ecart-type:
round(sd(legumes),3)
#kurtosis:
legumes2<-as.vector(legumes)
round(kurtosis(legumes2),3)
#skewness:
round(skewness(legumes2),3)
#test de normalité:
shapiro.test((legumes))
#test de KS
ks.test(legumes, "pnorm", mean = 0, sd = 1)

##sucrerie
#graphique
plot(sucrerie, xlab="Date", ylab="dépenses sucrerie", col="blue")
#Boîte à moustache
boxplot(sucrerie, main="Répartition des dépenses en sucrerie", col = "lightblue")
grubbs.test(sucrerie, type=10)
#Histogramme
hist(sucrerie, col = "lightblue", main="Histogramme des dépenses en sucrerie")
# Statistiques générales
summary(sucrerie)
#ecart-type:
round(sd(sucrerie),3)
#kurtosis:
sucrerie2<-as.vector(sucrerie)
round(kurtosis(sucrerie2),3)
#skewness:
round(skewness(sucrerie2),3)
#test de normalité:
shapiro.test((sucrerie))
#test de KS
ks.test(sucrerie, "pnorm", mean = 0, sd = 1)

##epices
#graphique
plot(epices, xlab="Date", ylab="dépenses en épices", col="blue")
#Boîte à moustache
boxplot(epices, main="Répartition des dépenses en epices", col = "lightblue")
#Histogramme
hist(epices, col = "lightblue", main="Histogramme des dépenses en epices")
# Statistiques générales
summary(epices)
#ecart-type:
round(sd(epices),3)
#kurtosis:
epices2<-as.vector(epices)
round(kurtosis(epices2),3)
#skewness:
round(skewness(epices2),3)
#test de normalité:
shapiro.test((epices))
#test de KS
ks.test(epices, "pnorm", mean = 0, sd = 1)

##cafe_cacao
#graphique
plot(cafe_cacao, xlab="Date", ylab="dépenses café, thé, cacao", col="blue")
#Boîte à moustache
boxplot(cafe_cacao, main="Répartition des dépenses en café, thé, cacao", col = "lightblue")
#Histogramme
hist(cafe_cacao, col = "lightblue", main="Histogramme des dépenses en café, thé, cacao")
# Statistiques générales
summary(cafe_cacao)
#ecart-type:
round(sd(cafe_cacao),3)
#kurtosis:
cafe_cacao2<-as.vector(cafe_cacao)
round(kurtosis(cafe_cacao2),3)
#skewness:
round(skewness(cafe_cacao2),3)
#test de normalité:
shapiro.test((cafe_cacao))
#test de KS
ks.test(cafe_cacao, "pnorm", mean = 0, sd = 1)

##boissons non alcoolisées
#graphique
plot(boissons_na, xlab="Date", ylab="dépenses boissons non alcoolisées", col="blue")
#Boîte à moustache
boxplot(boissons_na, main="Répartition des dépenses en boissons non alcoolisées", col = "lightblue")
#Histogramme
hist(boissons_na, col = "lightblue", main="Histogramme des dépenses en boissons non alcoolisées")
# Statistiques générales
summary(boissons_na)
#ecart-type:
round(sd(boissons_na),3)
#kurtosis:
boissons_na2<-as.vector(boissons_na)
round(kurtosis(boissons_na2),3)
#skewness:
round(skewness(boissons_na2),3)
#test de normalité:
shapiro.test((boissons_na))
#test de KS
ks.test(boissons_na, "pnorm", mean = 0, sd = 1)

##boissons alcoolisées
#graphique
plot(boisson_a, xlab="Date", ylab="dépenses boissons alcoolisées", col="blue")
#Boîte à moustache
boxplot(boisson_a, main="Répartition des dépenses en boissons alcoolisées", col = "lightblue")
#Histogramme
hist(boisson_a, col = "lightblue", main="Histogramme des dépenses en boissons alcoolisées")
# Statistiques générales
summary(boisson_a)
#ecart-type:
round(sd(boisson_a),3)
#kurtosis:
boisson_a2<-as.vector(boisson_a)
round(kurtosis(boisson_a2),3)
#skewness:
round(skewness(boisson_a2),3)
#test de normalité:
shapiro.test((boisson_a))
#test de KS
ks.test(boisson_a, "pnorm", mean = 0, sd = 1)


#Graphique de l'ensemble des séries
autoplot(INF)+
  geom_line(aes(y= INF, color="INF"))+
  geom_line(aes(y= viande, color="viande"))+
  geom_line(aes(y= poisson, color="poisson"))+
  geom_line(aes(y= cereales, color="cereales"))+
  geom_line(aes(y= laitage, color="laitage"))+
  geom_line(aes(y= huiles, color="huiles"))+
  geom_line(aes(y= fruits, col="fruits"))+
  geom_line(aes(y= legumes, col="legumes"))+
  geom_line(aes(y= sucrerie , col="sucrerie"))+
  geom_line(aes(y= epices, col="epices"))+
  geom_line(aes(y= cafe_cacao, col="cafe_cacao"))+
  geom_line(aes(y= boissons_na, col="boissons_na"))+
  geom_line(aes(y= boisson_a, col="boisson_a"))+
  labs(title = "Graphqiques des différentes séries temporelles ",
     x = "Année", y = "Pourcentage") +
  theme_minimal()

################################################################################

###Analyse économétrique

##Corrélation de Spearman de l'inflation avec les autres séries
s<-cor(base2[,2], base2[,3:14], method="spearman")
corrplot(s, type="full", method="number")

##Stationnarité avec le test de Dickey-Fuller augmenté ainsi que FAC et FACP

#INF
adf.test(INF)
adf.test(diff(INF)) #I(1)

#cereales
adf.test(cereales)
adf.test(diff(cereales)) #I(1)

#viande
adf.test(viande)
adf.test(diff(viande))#I(1)

#poisson
adf.test(poisson)
adf.test(diff(poisson))
adf.test(diff(diff(poisson)))#I(2)

#laitage
adf.test(laitage)
adf.test(diff(laitage))
adf.test(diff(diff(laitage)))#I(2)

#huiles
adf.test(huiles)
adf.test(diff(huiles))
adf.test(diff(diff(huiles)))#I(2)

#fruits
adf.test(fruits)
adf.test(diff(fruits))
adf.test(diff(diff(fruits)))#I(2)

#legumes
adf.test(legumes)
adf.test(diff(legumes))#I(1)

#sucrerie
adf.test(sucrerie)
adf.test(diff(sucrerie))#I(1)

#epices
adf.test(epices)
adf.test(diff(epices))#I(1)

#cafe_cacao
adf.test(cafe_cacao)
adf.test(diff(cafe_cacao))
adf.test(diff(diff(cafe_cacao)))#I(2)

#boissons_na
adf.test(boissons_na)
adf.test(diff(boissons_na))
adf.test(diff(diff(boissons_na)))#I(2)

#boissons_na
adf.test(boisson_a)
adf.test(diff(boisson_a))
adf.test(diff(diff(boisson_a)))#I(2)

##cointégration

#cereales
cereales_INF<-cbind(cereales, INF)
VARselect(cereales_INF) #AIC=2
coint_cereales<-ca.jo(cereales_INF, ecdet ="const", type="trace", K=2)
summary(coint_cereales)#cointégration seuil de 10%


#viande
viande_INF<-cbind(viande, INF)
VARselect(viande_INF) #AIC=1
coint_viande<-ca.jo(viande_INF, ecdet ="const", type="trace", K=2)
summary(coint_viande)#cointégration au seuil de 5%

#legumes
legumes_INF<-cbind(legumes, INF)
VARselect(legumes_INF) #AIC=10
coint_legumes<-ca.jo(legumes_INF, ecdet ="const", type="trace", K=10)
summary(coint_legumes)#cointégration au seuil de 1%

#sucrerie
sucrerie_INF<-cbind(sucrerie, INF)
VARselect(sucrerie_INF) #AIC=2
coint_sucrerie<-ca.jo(sucrerie_INF, ecdet ="const", type="trace", K=2)
summary(coint_sucrerie)#cointégration au seuil de 5%

#epicerie
epices_INF<-cbind(epices, INF)
VARselect(epices_INF) #AIC=2
coint_epices<-ca.jo(epices_INF, ecdet ="const", type="trace", K=2)
summary(coint_epices)#cointégration au seuil de 5%

##VECM

#céréales
modele1<-lm(cereales~INF)
summary(modele1)
ECT<-as.vector(modele1$residuals)[-1]
dcereales<-diff(cereales)
dINF<-diff(INF)
modele_d1<-lm(dcereales~dINF+ECT-1)
summary(modele_d1)#rééquilibrage à long terme

#test sur les résidus
adf.test(modele1$residuals)#résidus non stationnaire
acf(modele1$residuals)#résidus non stationnaire
bptest(modele1)# homoscédasticité
shapiro.test(modele1$residuals) # normalité

#viande
modele2<-lm(viande~INF)
summary(modele2)
ECT2<-as.vector(modele2$residuals)[-1]
dviande<-diff(viande)
dINF<-diff(INF)
modele_d2<-lm(dviande~dINF+ECT2-1)
summary(modele_d2)#pas de rééquilibrage

#test sur les résidus
adf.test(modele2$residuals)#résidus non stationnaire
acf(modele2$residuals)#résidus non stationnaire
bptest(modele2)# hétéroscédasticité
shapiro.test(modele2$residuals) # non normalité


#légumes
modele3<-lm(legumes~INF)
summary(modele3)
ECT3<-as.vector(modele3$residuals)[-1]
dlegumes<-diff(legumes)
dINF<-diff(INF)
modele_d3<-lm(dlegumes~dINF+ECT3-1)
summary(modele_d3)#pas de rééquilibrage

#test sur les résidus
adf.test(modele3$residuals)#résidus non stationnaire
acf(modele3$residuals)#résidus non stationnaire
bptest(modele3)# homoscédasticité
shapiro.test(modele3$residuals) # normalité


#sucrerie
modele4<-lm(sucrerie~INF)
summary(modele4)
ECT4<-as.vector(modele4$residuals)[-1]
dsucrerie<-diff(sucrerie)
dINF<-diff(INF)
modele_d4<-lm(dsucrerie~dINF+ECT4-1)
summary(modele_d4)#pas de rééquilibrage pour cause de significativité du modèle

#test sur les résidus
adf.test(modele4$residuals)#résidus non stationnaire
acf(modele4$residuals)#résidus non stationnaire
bptest(modele4)# homoscédasticité
shapiro.test(modele4$residuals) # non normalité


#épices
modele5<-lm(epices~INF)
summary(modele5)
ECT5<-as.vector(modele5$residuals)[-1]
depices<-diff(epices)
dINF<-diff(INF)
modele_d5<-lm(depices~dINF+ECT5-1)
summary(modele_d5)#pas de rééquilibrage

#test sur les résidus
adf.test(modele5$residuals)#résidus non stationnaire
acf(modele5$residuals)#résidus non stationnaire
bptest(modele5)# homoscédasticité
shapiro.test(modele5$residuals) # normalité


##VAR

#cereales
mod1<-VAR(cereales_INF, p=2, type = "const")
summary(mod1)
#testing for serial correlation
mod.serial1<-serial.test(mod1, lags.pt=12, type="PT.asymptotic")
mod.serial1
plot(mod.serial1, names = "cereales")
plot(mod.serial1, names = "INF")
#heteroscedasticity
mode.arch1<-arch.test(mod1, lags.multi=12, multivariate.only=TRUE)
mode.arch1
#normalité
mode.norm1<-normality.test(mod1, multivariate.only=TRUE)
mode.norm1
#test for the structural break in the residuals
mode.cusum1<-stability(mod1, type="OLS-CUSUM")
plot(mode.cusum1)
#Granger causality
cause.cereales<-causality(mod1, cause="INF")
cause.cereales
cause.INF1<-causality(mod1, cause="cereales")
cause.cereales
#IRF, INF shock
irf1<-irf(mod1, impulse="INF", response="cereales", n.ahead = 60, boot=TRUE)
plot(irf1, ylab="output", main="Shock from inflation")

#viande
mod2<-VAR(viande_INF, p=1, type = "const")
summary(mod2)
#testing for serial correlation
mod.serial2<-serial.test(mod2, lags.pt=12, type="PT.asymptotic")
mod.serial2
plot(mod.serial2, names = "cereales")
plot(mod.serial2, names = "INF")
#heteroscedasticity
mode.arch2<-arch.test(mod2, lags.multi=12, multivariate.only=TRUE)
mode.arch2
#normalité
mode.norm2<-normality.test(mod2, multivariate.only=TRUE)
mode.norm2
#test for the structural break in the residuals
mode.cusum2<-stability(mod2, type="OLS-CUSUM")
plot(mode.cusum2)
#Granger causality
cause.viande<-causality(mod2, cause="INF")
cause.viande
cause.INF2<-causality(mod2, cause="viande")
cause.viande
#IRF, INF shock
irf2<-irf(mod2, impulse="INF", response="viande", n.ahead = 60, boot=TRUE)
plot(irf2, ylab="output", main="Shock from inflation")


#legumes
mod3<-VAR(legumes_INF, p=10, type = "const")
summary(mod3)
#testing for serial correlation
mod.serial3<-serial.test(mod3, lags.pt=12, type="PT.asymptotic")
mod.serial3
plot(mod.serial3, names = "legumes")
plot(mod.serial3, names = "INF")
#heteroscedasticity
mode.arch3<-arch.test(mod3, lags.multi=12, multivariate.only=TRUE)
mode.arch3
#normalité
mode.norm3<-normality.test(mod3, multivariate.only=TRUE)
mode.norm3
#test for the structural break in the residuals
mode.cusum3<-stability(mod3, type="OLS-CUSUM")
plot(mode.cusum3)
#Granger causality
cause.legumes<-causality(mod3, cause="INF")
cause.legumes
cause.INF3<-causality(mod3, cause="legumes")
cause.legumes
#IRF, INF shock
irf3<-irf(mod3, impulse="INF", response="legumes", n.ahead = 60, boot=TRUE)
plot(irf3, ylab="output", main="Shock from inflation")

#sucrerie
mod4<-VAR(sucrerie_INF, p=2, type = "const")
summary(mod4)
#testing for serial correlation
mod.serial4<-serial.test(mod4, lags.pt=12, type="PT.asymptotic")
mod.serial4
plot(mod.serial4, names = "sucrerie")
plot(mod.serial4, names = "INF")
#heteroscedasticity
mode.arch4<-arch.test(mod4, lags.multi=12, multivariate.only=TRUE)
mode.arch4
#normalité
mode.norm4<-normality.test(mod4, multivariate.only=TRUE)
mode.norm4
#test for the structural break in the residuals
mode.cusum4<-stability(mod4, type="OLS-CUSUM")
plot(mode.cusum4)
#Granger causality
cause.sucrerie<-causality(mod4, cause="INF")
cause.sucrerie
cause.INF4<-causality(mod4, cause="sucrerie")
cause.sucrerie
#IRF, INF shock
irf4<-irf(mod4, impulse="INF", response="sucrerie", n.ahead = 60, boot=TRUE)
plot(irf4, ylab="output", main="Shock from inflation")

#epices
mod5<-VAR(epices_INF, p=2, type = "const")
summary(mod5)
#testing for serial correlation
mod.serial5<-serial.test(mod5, lags.pt=12, type="PT.asymptotic")
mod.serial5
plot(mod.serial5, names = "epices")
plot(mod.serial5, names = "INF")
#heteroscedasticity
mode.arch5<-arch.test(mod5, lags.multi=12, multivariate.only=TRUE)
mode.arch5
#normalité
mode.norm5<-normality.test(mod5, multivariate.only=TRUE)
mode.norm5
#test for the structural break in the residuals
mode.cusum5<-stability(mod5, type="OLS-CUSUM")
plot(mode.cusum5)
#Granger causality
cause.epices<-causality(mod5, cause="INF")
cause.epices
cause.INF5<-causality(mod5, cause="epices")
cause.epices
#IRF, INF shock
irf5<-irf(mod5, impulse="INF", response="epices", n.ahead = 60, boot=TRUE)
plot(irf5, ylab="output", main="Shock from inflation")


rm(list=ls())
library(dplyr)
library(magrittr)
library(cluster)
library(Rtsne)
library(ggplot2)
library(factoextra)

df14 = read.csv2("data_2014_HA.csv") # Carico il dataset

# Creo un dataset avente come unità le singole squadre quando giocano in casa 
# e come valori delle variabili le medie dei valori riferiti a quella determinata squadra
df_Home_14 = df14 %>% group_by(Home.Team) %>% 
  summarise(mean_HGFT = mean(Home.Goals.FT),
            mean_HGHT = mean(Home.Goals.HT),
            mean_Poss = mean(Home.Team.Possession..),
            mean_OffT = mean(Home.Team.Off.Target.Shots),
            mean_OnT = mean(Home.Team.On.Target.Shots),
            mean_TotS = mean(Home.Team.Total.Shots),
            mean_BlS = mean(Home.Team.Blocked.Shots),
            mean_Corn = mean(Home.Team.Corners),
            mean_ThIn = mean(Home.Team.Throw.Ins),
            mean_PaSu = mean(Home.Team.Pass.Success..),
            mean_AeWo = mean(Home.Team.Aerials.Won),
            mean_Clea = mean(Home.Team.Clearances),
            mean_Foul = mean(Home.Team.Fouls),
            mean_YeCa = mean(Home.Team.Yellow.Cards),
            mean_SeYC = mean(Home.Team.Second.Yellow.Cards),
            mean_ReCa = mean(Home.Team.Red.Cards),
            .groups = 'drop')

# Medesimo dataset ma con le squadre quando giocano fuori casa
df_Away_14 = df14 %>% group_by(Away.Team) %>%
  summarise(mean_GoFT = mean(Away.goals.FT),
            mean_GoHT = mean(Away.Goals.HT),
            mean_Poss = mean(Away.Team.Possession..),
            mean_OffT = mean(Away.Team.Off.Target.Shots),
            mean_OnT = mean(Away.Team.On.Target.Shots),
            mean_TotS = mean(Away.Team.Total.Shots),
            mean_BlS = mean(Away.Team.Blocked.Shots),
            mean_Corn = mean(Away.Team.Corners),
            mean_ThIn = mean(Away.Team.Throw.Ins),
            mean_PaSu = mean(Away.Team.Pass.Success..),
            mean_AeWo = mean(Away.Team.Aerials.Won),
            mean_Clea = mean(Away.Team.Clearances),
            mean_Foul = mean(Away.Team.Fouls),
            mean_YeCa = mean(Away.Team.Yellow.Cards),
            mean_SeYC = mean(Away.Team.Second.Yellow.Cards),
            mean_ReCa = mean(Away.Team.Red.Cards),
            .groups = 'drop')

colnames(df_Away_14)=colnames(df_Home_14)
df_HA_14 = rbind(df_Home_14,df_Away_14)

row.names(df_HA_14)=df_HA_14$Home.Team

# Creo la matrice di dissimilarità con distanza euclidea
dist<-dist(df_HA_14[-1],method="euclidean")
mat <- as.matrix(dist)

sil_width <- c(NA)
sil_width=NULL

for(i in 2:10){
  
  pam_fit <- pam(dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Cerco il numero migliore di cluster mediante il metodo del gomito
dev.new()
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# Controllo la silhouette risultante
pam_fit <- pam(dist,diss = TRUE,k = 2)
plot(silhouette(pam_fit))

# Eseguo la cluster non gerarchica mediante t-sne
tsne_obj <- Rtsne(dist, perplexity = 50 ,is_distance = TRUE, theta = 0.5, eta = 700, max_iter = 5000)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Offensività")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = df_HA_14$Home.Team)
# Studio le correlazioni delle due variabili nascoste con le variabili del dataset originale
mat_df = cbind(df_HA_14[-1], tsne_data$X, tsne_data$Offensività)
mat_cor = as.data.frame(cor(mat_df))
cor_XY = mat_cor[1:18, 17:18]
View(cor_XY)
# Osservo la distribuzione delle unità e i cluster risultanti
ggplot(aes(x = X, y = Offensività), data = tsne_data) + geom_text(label = tsne_data$name, size = 2.5, aes(color = cluster), check_overlap = T)
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))

tsne_data_fil = tsne_data %>%
  filter(X < 1 & X > -1,
         Offensività > 3)

ggplot(aes(x = X, y = Offensività), data = tsne_data_fil) + geom_text(label = tsne_data_fil$name, size = 3.5, check_overlap = T)

g1 = subset(tsne_data, tsne_data$cluster == 1)$name
g2 = subset(tsne_data, tsne_data$cluster == 2)$name

# Eseguo la PCA mediante princomp
pca_ds<-princomp(df_HA_14[-1], cor=TRUE, scores=TRUE)
rownames(pca_ds$scores)=df_HA_14$Home.Team

n_club = length(pca_ds$scores[,2])
# Studio la numerosità delle squadre nei gruppi creatisi mediante la PCA
n_fallose = nrow(subset(pca_ds$score, pca_ds$scores[1:n_club,2]>0))/(n_club/100)
n_offensive = nrow(subset(pca_ds$score, pca_ds$scores[1:n_club,1]>0))/(n_club/100)
n_pulite = 100-n_fallose
n_difensive = 100-n_offensive

dev.new()
# Osservo come le componenti spiegano la varianza
fviz_eig(pca_ds)

fviz_pca_ind(pca_ds,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   
)

fviz_pca_var(pca_ds,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE   
)

# Osservo direzione delle variabili e unità statistiche contemporaneamente
fviz_pca_biplot(pca_ds, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969",
                label = 'all'
)

pc1=pca_ds$scores[1:n_club,1]
pc2=pca_ds$scores[1:n_club,2]

pca_ds_fil = pca_ds$scores %>%
  data.frame() %>%
  filter(pc1 > 2.5,
         pc2 > -3 & pc2 < 3)

ggplot(aes(x = Comp.1, y = Comp.2), data = pca_ds_fil) + geom_text(label = rownames(pca_ds_fil), size = 3.5, check_overlap=T)

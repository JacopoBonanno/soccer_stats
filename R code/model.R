rm(list=ls())
df = read.csv2("data_v2.csv") #data_Home.csv data_Away.csv

summary(df)
str(df)
# Trasformo le variabili in numeriche
for (i in seq(1, ncol(df))) {
  df[,i]=as.numeric(df[,i])
}

# Testo la regressione logistica, prendendo un campione dell'80% del dataset come sample
training_index_sample = sample(nrow(df)-1, nrow(df)*0.8) 
training_sample = df[training_index_sample,]       
test_sample = df[-training_index_sample,]
df=training_sample

out = glm(output ~ . , family = binomial("logit"), data = df)
summary.glm(out)
# Studio le collinearità
library(car)
vif(out)
# Analizzo la robustezza del modello mediante probit e lineare
outp = glm(output ~ . , family = binomial("probit"), data = df)
summary.glm(outp)

outl = lm(output ~ . , data = df)
summary(outl)
# Studio la significatività totale
Chi2_oss=out$null.deviance-out$deviance
gradi_lib=out$df.null-out$df.residual
alfa_oss=1-pchisq(Chi2_oss, df=gradi_lib)   
print(alfa_oss)
# Calcolo l'R quadro di McFadden
mf_r2=1-out$deviance/out$null.deviance
print(mf_r2)
# Effettuo il testing sul modello utilizzando il sample di test del 20% ottenuto in precedenza
betalogit = out$coefficients
X = cbind(1, test_sample[-19])
xb = as.matrix(X) %*% betalogit
phat = exp(xb)/(1 + exp(xb))
yhat = as.numeric(phat>0.4956)

c0 = sum((yhat==0&test_sample$output==0))
c1 = sum((yhat==1&test_sample$output==1))
w1 = sum((yhat==0&test_sample$output==1))
w0 = sum((yhat==1&test_sample$output==0))
tabella_predictions = rbind(cbind(c1,w1),cbind(w0,c0))
print(tabella_predictions)
print(cbind(c1,w1))
print(cbind(w0,c0))
# Studio accurarezza, sensitività e specificità, anche mediante curva ROC
perc_correct = (c0 + c1) / length(test_sample$output)
print(perc_correct) 

sensitivity=c1/(c1+w1)
print(sensitivity) 

specificity=c0/(c0+w0)
print(specificity) 

library(pROC)
roc=roc(test_sample$output ~ phat)
plot(roc, xlab='1-specificità',ylab='sensitività' )   
auc(roc) 

# Eseguo i precedenti passaggi ma per 1000 run, per ottenere risultati più attendibili e robusti
library(pROC)

n_camp = 1000
matrice_beta = matrix(rep(0,19), ncol = 1)
matrice_predizioni = matrix(rep(0, 4), ncol = 1)
matrice_dev_res = matrix(rep(0,5), ncol = 1)

for (i in 1:n_camp){
  training_index_sample = sample(nrow(df)-1, nrow(df)*0.15) 
  training_sample = df[training_index_sample,]       
  test_sample = df[-training_index_sample,]
  
  out_t  = glm(output ~ . , family = binomial("logit"), data = training_sample)
  sum_t=summary.glm(out_t)
  dev_res = c(min(sum_t$deviance.resid), quantile(sum_t$deviance.resid)[2],
              median(sum_t$deviance.resid), quantile(sum_t$deviance.resid)[4],
              max(sum_t$deviance.resid))
  matrice_dev_res = cbind(matrice_dev_res, as.vector(dev_res))
  
  matrice_beta = cbind(matrice_beta, out_t$coefficients)
  
  betalogit = out_t$coefficients 
  X = cbind(1, test_sample[-19]) 
  xb = as.matrix(X) %*% betalogit
  phat = exp(xb)/(1 + exp(xb)) 
  yhat = as.numeric(phat>0.4956)
  
  c0 = sum((yhat==0&test_sample$output==0)) 
  c1 = sum((yhat==1&test_sample$output==1)) 
  w1 = sum((yhat==0&test_sample$output==1))
  w0 = sum((yhat==1&test_sample$output==0)) 
  
  
  perc_correct = (c0 + c1) / length(test_sample$output) 
  
  sensitivity=c1/(c1+w1)
  
  specificity=c0/(c0+w0)
  
  roc=roc(test_sample$output ~ phat)
  area_roc = auc(roc)
  
  matrice_predizioni = cbind(matrice_predizioni, c(perc_correct, sensitivity, specificity, area_roc))
}

matrice_beta = matrice_beta[,-1]
matrice_predizioni = matrice_predizioni[,-1]
matrice_dev_res = matrice_dev_res[,-1]
# Tengo traccia dei residui, dei coefficienti e delle statistiche relative alle predizioni 
# per ogni run e ne calcolo media e deviazione standard
lista_dev_res = rep(0,5)
for (o in 1:5){
  lista_dev_res[o] = mean(matrice_dev_res[o,])
}
lista_dev_res_sd = rep(0,5)
for (p in 1:5){
  lista_dev_res_sd[p] = sqrt(mean((matrice_dev_res[p,]-mean(matrice_dev_res[p,]))^2))
}

matrice_recap_dev_res = cbind(lista_dev_res, lista_dev_res_sd)

lista_beta = rep(0, 19)
for (k in 1:19){
  lista_beta[k] = mean(matrice_beta[k,])
}

lista_beta_sd = rep(0, 19)
for (j in 1:19){
  lista_beta_sd[j] = sqrt(mean((matrice_beta[j,]-mean(matrice_beta[j,]))^2))
}

matrice_recap = cbind(lista_beta, lista_beta_sd)

lista_stat_predizioni = rep(0, 4)
for (q in 1:4){
  lista_stat_predizioni[q] = mean(matrice_predizioni[q,])
}

lista_stat_predizioni_sd = rep(0, 4)
for (w in 1:4){
  lista_stat_predizioni_sd[w] = sqrt(mean((matrice_predizioni[w,]-mean(matrice_predizioni[w,]))^2))
}

matrice_recap_stat_predizioni = cbind(lista_stat_predizioni, lista_stat_predizioni_sd)
# Osservo i risultati
View(matrice_recap_stat_predizioni)
View(matrice_recap)
View(matrice_recap_dev_res)






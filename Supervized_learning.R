library(car)  # para bases de dados e gráficos
library(lmtest)  # para testes de hipótese sobre os pressupostos dos resíduos
library(olsrr)  # para regressão (e testes de hipótese)
library(tseries)  # teste de Jarque-Bera
library(ggplot2)   # gráficos
library(tidyr)   # data-frames
library(MASS) 
library(sandwich)
library(caTools)# estatística, bases de dados, regressão

df <- read.csv("listings.csv", header = TRUE)

#ANALISE GERAL DO DF
head(df)
str(df)
summary(df)

length(df$neighbourhood_group)
table(is.na(df$neighbourhood_group))

#REMOVER COLUNAS FULL WITH NA
df_improved <- subset(df,select =-c(neighbourhood_group,license))


summary(df_improved)


nrow(df_improved)


#CHECKAR SE OS NAS NO REVIEW POR MONTH CORREPONDEM A 0 REVIEW COMO CORREPONDEM DAR SUB POR 0
nrow(df_improved)-nrow(na.omit(df_improved))

df_improved[which(is.na(df_improved$reviews_per_month)&df_improved$number_of_reviews==0),1]==df_improved[which(is.na(df_improved$reviews_per_month)) ,1]


df_improved$reviews_per_month[is.na(df_improved$reviews_per_month)] <-0



summary(df_improved)


#VERIFICAMOS QUE EXISTE UM PRICE COM 0 PORTANTO REMOVEMOS


df_improved[which(df_improved$price==0),]

df_improved <- df_improved[-(which(df_improved$price==0)),]

summary(df_improved)

#reviews per month > 31 impossivel ??

df_improved[which(df_improved$reviews_per_month> 31),]
df_improved <- df_improved[-(which(df_improved$reviews_per_month>31)),]


summary(df_improved)

#minimum nights > 365

df_improved[which(df_improved$minimum_nights> 365),]

df_improved <- df_improved[-(which(df_improved$minimum_nights>365)),]


summary(df_improved)




df_improved_no_quali <- subset(df_improved,select = -c(name,host_name,neighbourhood,last_review,room_type))

head(df_improved_no_quali)

cor(df_improved_no_quali)


df_improved_model <- subset(df_improved, select = -c(name,id,host_name)) 
colnames(df_improved_model)

df_improved_model <- na.omit(df_improved_model)


#modeling comeca aqui

fit <- lm(price ~., data=df_improved_model)
ols_step_both_p(fit)

fit <- lm(price ~., data=df_improved_model)
ols_step_both_aic(fit)


df_model <- subset(df_improved_model, select=c(neighbourhood,reviews_per_month,minimum_nights,room_type,availability_365,host_id,number_of_reviews,price))

# neighbourhood,review_per_month,minimum_night,room_type,availabilty_365,host_id,number_of_reviews ---- concordam os dois metodos

fit <- lm(price~., data = df_model)
summary(fit)

#analises dos pressupostos


mean(fit$residuals)  # media dos erros muito proxima de 1
bptest(fit) # nao passa o bp test logo erros não são homocedasticos
bgtest(fit) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit$residuals) # não se aproxima da normalidade 



options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit)

outlierTest(fit, data=df_model)

df_model_1 <- df_model[-c(4582,5562,7033,7034,7032,7035,2553,2554,6973,2509),]

fit1 <- lm(price ~., data=df_model_1)
summary(fit1)




-# neighbourhood,review_per_month,minimum_night,room_type,availabilty_365,host_id,number_of_reviews,calculated_host_listings_count ---- concordam os dois metodos
  
  
  
  #analises dos pressupostos
  
  
  mean(fit1$residuals)  # media dos erros muito proxima de 1
bptest(fit1) # p value > 0.05 logo existe homocedatiscos
bgtest(fit1) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit1$residuals) # não se aproxima da normalidade 


options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit1)


influenceIndexPlot (fit1, id=list(n=3)) # podemos ver alguns elementos extremos influenciadores

AIC(fit, fit1)


cooksd <- cooks.distance(fit1)


influential <- as.numeric(names((cooksd)[cooksd > (100* mean(cooksd,na.rm = TRUE))]))




influential <- na.omit(influential)

df_model_1[influential, ]






df_model_2 <- df_model_1[-c(influential),]


fit2 <- lm(price ~., data=df_model_2)
summary(fit2)


AIC(fit1,fit2)


mean(fit2$residuals)  
bptest(fit2) 
bgtest(fit2)  
jarque.bera.test(fit2$residuals)  


options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit2)


influenceIndexPlot (fit2, id=list) # podemos ver alguns elementos extremos influenciadores


outlierTest(fit2,df_model_2)



df_model_3 <- df_model_2[-c(4582,5562,7032,7033,7034,7035,6973,7668,1041),]



fit3 <- lm(log(price) ~., data=df_model_3)
summary(fit3)


AIC(fit,fit1,fit2,fit3)


mean(fit3$residuals) 
bptest(fit3) 
bgtest(fit3) 
jarque.bera.test(fit3$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit3)



crPlots(fit3)

fit4 <- lm(log(price)~neighbourhood + reviews_per_month + minimum_nights+ I(minimum_nights^2)+I(minimum_nights^3)+room_type+availability_365+host_id+number_of_reviews,data = df_model_3)
summary(fit4)

AIC(fit,fit1,fit2,fit3,fit4)

mean(fit4$residuals)  
bptest(fit4) 
bgtest(fit4)  
jarque.bera.test(fit4$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit4)


crPlots(fit4)



fit5 <- lm(log(price)~neighbourhood + reviews_per_month + I(1/minimum_nights)+room_type+availability_365+host_id+number_of_reviews,data = df_model_3)
summary(fit5)

AIC(fit,fit1,fit2,fit3,fit4,fit5)

mean(fit5$residuals)  
bptest(fit5) 
bgtest(fit5)  
jarque.bera.test(fit5$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit5)

#fit5 no goooddd
crPlots(fit5)



w = df_model_3$price - mean(df_model_3$price)-sd(df_model_3$price)

fit6 <- lm(log(price)~neighbourhood+room_type+number_of_reviews+  reviews_per_month +minimum_nights+ I(minimum_nights^2)+I(minimum_nights^3)+availability_365+ I(availability_365^2)+host_id,data = df_model_3,weights = w**2)
summary(fit6)

AIC(fit,fit1,fit2,fit3,fit4,fit5,fit6)

mean(fit6$residuals)  
bptest(fit6) 
bgtest(fit6) # 
jarque.bera.test(fit6$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit6)

#fit5 no goooddd
crPlots(fit6)

df_model_3[2757,]
df_model_3[5562,]

outlierTest(fit6,data = df_model_3)


df_model_3[c(7519,6494,4582,7032,7033,7034,7035,1768,5562,166),]

#nada fora do normal


N<-length(df_model_3$price)

#Fit 7 melhor



fit7 <- lm(log(price)~neighbourhood+room_type+number_of_reviews+I(minimum_nights^2)+minimum_nights+I(minimum_nights^3) + reviews_per_month+ availability_365+host_id,data = df_model_3, weights = 1/((1:N)^0.5)**2)

#+I(minimum_nights^2)+minimum_nights+I(minimum_nights^3)
summary(fit7)


AIC(fit,fit1,fit2,fit3,fit4,fit5,fit6,fit7)



vif(fit7)
mean(fit7$residuals)  # media dos erros muito proxima de 1
bptest(fit7) # p value > 0.05 logo é homocedatiscos
bgtest(fit7) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit7$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit7)



yhat<- predict(fit7,data=df_model_3)
w <- 1/sqrt(yhat)

fit8 <- rlm(log(log(price))~neighbourhood+room_type+number_of_reviews+  reviews_per_month +minimum_nights+ I(minimum_nights^2)+I(minimum_nights^3)+availability_365+host_id,data = df_model_3, weights = 1/w**2)
summary(fit8)

AIC(fit8)
mean(fit8$residuals)  # media dos erros muito proxima de 1
bptest(fit8) # p value > 0.05 logo existe homocedatiscos
bgtest(fit8) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit8$residuals) 







h <- sqrt(fit4$residuals^2)
fit9 <- lm(log(log(price))~neighbourhood+room_type+number_of_reviews+  reviews_per_month +minimum_nights+ I(minimum_nights^2)+I(minimum_nights^3)+availability_365+host_id,data = df_model_3, weights = 1/h**2)
summary(fit9)


AIC(fit9)
mean(fit9$residuals)  # media dos erros muito proxima de 1
bptest(fit9) # p value > 0.05 logo existe homocedatiscos
bgtest(fit9) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit9$residuals) 


outlierTest(fit9, data= df_model_3)




df_model_4 <- df_model_3[-c(7519)]




h1 <- sqrt(fit7$residuals)
fit10 <- lm(log(log(price))~neighbourhood+room_type+number_of_reviews + reviews_per_month +minimum_nights+ I(minimum_nights^2)+I(minimum_nights^3)+availability_365+host_id,data = df_model_3, weights = 1/h1)
summary(fit10)


AIC(fit10)
mean(fit10$residuals)

AIC(fit10)
mean(fit10$residuals)  # media dos erros muito proxima de 1
bptest(fit10) # p value > 0.05 logo existe homocedatiscos
bgtest(fit10) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit10$residuals)




influenceIndexPlot (fit2, id=list)

cooksd <- cooks.distance(fit10)


influential <- as.numeric(names((cooksd)[cooksd > (100* mean(cooksd,na.rm = TRUE))]))




influential <- na.omit(influential)

df_model_4[influential, ]



df_model_5 <- df_model_4[-c(influential),]
























#melohr modelo fit 7

set.seed(69000000)
df_fit_7 <- df_model_3

df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 11'] <- "Neighbourhood NR"
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 17'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 18'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 19']<- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 2'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 21'] <- 'Neighbourhood NR'

df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 22'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 24'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 25'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 3'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 5'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 20'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 23'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 28'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 6'] <- 'Neighbourhood NR'

df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 26'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 27'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 32'] <- 'Neighbourhood NR'
df_fit_7$neighbourhood [df_fit_7$neighbourhood == 'District 34'] <- 'Neighbourhood NR'

df_fit_7$room_type [df_fit_7$room_type == 'Hotel room'] <- 'room room'
df_fit_7$room_type [df_fit_7$room_type == 'Shared room'] <- 'room room'



df_fit_7

training_testing_1 <- sample.split(Y = df_fit_7$price, SplitRatio=0.7)

train_1<-  df_fit_7[training_testing_1,]

test_1 <-  df_fit_7[!training_testing_1,] 

dim(train_1)
dim(test_1)




#Fit 7 melhor





N<-length(train_1$price)
fit_train_7 <- lm(log(price)~neighbourhood+room_type+number_of_reviews+minimum_nights+ reviews_per_month+ availability_365+host_id,data = train_1, weights = 1/((1:N)^0.5)**1.5)



vif(fit_train_7)


AIC(fit_train_7)

summary(fit_train_7)

mean(fit_train_7$residuals)  # media dos erros muito proxima de 0
bptest(fit7) # p value > 0.05 logo é homocedatiscos
bgtest(fit7) # não passa o bgtest o que significa que existe autocorelacao 
jarque.bera.test(fit7$residuals) 

options(repr.plot.width=5, repr.plot.height=5)
par(mfrow=c(2,2)) 
plot(fit_train_7)


prev_1 <- exp(predict(fit_train_7,test_1))





plot((prev_1), type = "b", frame = FALSE, pch = 19, col = "red", xlab = "x", ylab = "y")

lines(test_1$price, pch = 18, col = "blue", type = "b", lty = 5)

legend("topleft", legend=c("Prediction", "True value"), col=c("red", "blue"), lty = 1:2, cex=0.8)


preco_real<-test_1$price

n<-length(test_1$price)

# o MAPE é uma das melhores métricas, pois é dada por uma percentagem (não é dependente de escala)
MAPE_fit7 <- (1/n) * sum(abs((preco_real - prev_1)/preco_real))
MAPE_fit7




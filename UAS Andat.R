data = read.csv("C:/Data Andat.csv", sep = ";",header = T)
head(data)

# Atur layout tampilan: 2 baris x 3 kolom
par(mfrow = c(2, 3))
# Histogram X1 sampai X6 
hist(data$X1, 
     main = "Penerapan Efisiensi", 
     xlab = "Skor", 
     col = "orange", 
     border = "black")

hist(data$X2, 
     main = "Fasilitas Kampus", 
     xlab = "Skor", 
     col = "mediumpurple", 
     border = "black")

hist(data$X3, 
     main = "Program Kampus", 
     xlab = "Skor", 
     col = "darkseagreen", 
     border = "black")

hist(data$X4, 
     main = "Kualitas layanan", 
     xlab = "Skor", 
     col = "coral", 
     border = "black")

hist(data$X5, 
     main = "Kegiatan Perkuliahan", 
     xlab = "Skor", 
     col = "darkcyan", 
     border = "black")

hist(data$Y, 
     main = "Efisiensi Anggaran", 
     xlab = "Skor", 
     col = "steelblue", 
     border = "black")
# Kembalikan layout ke default
par(mfrow = c(1, 1))

# Model regresi logistik
model_logit = glm(Y ~ X1 + X2 + X3 + X4 + X5, data = data, family = binomial)
model_logit
library(car)
Anova(model_logit)
summary(model_logit)
anova(model_logit, test="LRT") # likelihood-ratio test comparing models

# Prediksi probabilitas pada berbagai skenario
# Skenario 1: X2 = 1, lainnya pada nilai rata-rata
predict(model_logit, data.frame(X1 = mean(data$X1), X2 = 1, X3 = mean(data$X3),
                                X4 = mean(data$X4), X5 = mean(data$X5)), type = "response")

# Skenario 2: X2 = 4, bandingkan dengan skenario 1
predict(model_logit, data.frame(X1 = mean(data$X1), X2 = 4, X3 = mean(data$X3),
                                X4 = mean(data$X4), X5 = mean(data$X5)), type = "response")

# Prediksi probabilitas dengan variasi pada Q2 (jam tanggap)
predict(model_logit, data.frame(X1 = quantile(data$X1), X2 = mean(data$X2), 
                                X3 = mean(data$X3),X4 = mean(data$X4), X5 = mean(data$X5)), type = "response")

library(mfx)
logitmfx(model_logit, atmean=FALSE, data=data) # with atmean = TRUE, finds

# Hitung proporsi Q1 = 1 (responden yang puas)
prop = sum(data$Y)/nrow(data) # sample proportion of 1's for y variable
prop
predicted = as.numeric(fitted(model_logit) > prop)
xtabs(~data$Y+predicted)

library(pROC)
rocplot = roc(Y ~ fitted(model_logit), data = data)
plot.roc(rocplot, legacy.axes = TRUE) # Specficity on x axis if legacy.axes = F
auc(rocplot) # auc = area under ROC curve = concordance index

cor(data$Y, fitted(model_logit))

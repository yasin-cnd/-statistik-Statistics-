install.packages("ggplot2")
library(ggplot2)
# Bagimsiz veri
data_independent <- data.frame(
  A = c(2.00, 12.00, 5.00, 4.00, 26.00, 8.00, 17.00, 4.00, 25.00, 6.00, 21.00, 6.00),
  B = c(17.00, 15.00, 3.00, 19.00, 5.00, 14.00, 5.00, 6.00, 19.00, 4.00, 9.00, 7.00),
  C = c(29.00, 3.00, 25.00, 28.00, 11.00, 7.00, 5.00, 25.00, 32.00, 24.00, 36.00, 20.00)
)

print(data_independent)
#özet istatistikler
summary_statistics <- summary(data_independent)
print(summary_statistics)
#grafik
boxplot(data_independent, col = c("red", "green", "blue"), main = "Box Plot", xlab = "Variable", ylab = "Value")
#normallik testi
alpha <- 0.05

for (column in colnames(data_independent)) {
  shapiro_test <- shapiro.test(data_independent[[column]])
  
  cat("\nShapiro-Wilk test for column '", column, "':\n",
      "   Null Hipotezi (H0): Veri seti normal olarak dagilmistir.\n",
      "   Alternatif Hipotezi (H1): Veri seti normal olarak dagilmamistir.\n")
  
  if (shapiro_test$p.value < alpha) {
    cat("   Sonuç: p-value =", shapiro_test$p.value, ". Null hipotezi reddedilir. Veri seti normal olarak dagilmamistir.\n")
  } else {
    cat("   Sonuç: p-value =", shapiro_test$p.value, ". Null hipotezi reddedilemez. Veri seti normal olarak dagilmistir.\n")
  }
}
# Bagimli Veri
data_dependent <- data.frame(
  A = c(5.00, 1.00, 16.00, 5.00, 10.00, 19.00, 10.00),
  B = c(4.00, 3.00, 12.00, 4.00, 9.00, 18.00, 7.00),
  C = c(7.00, 1.00, 22.00, 3.00, 7.00, 28.00, 6.00),
  D = c(10.00, 0.00, 22.00, 5.00, 13.00, 37.00, 8.00),
  E = c(12.00, 2.00, 35.00, 4.00, 10.00, 58.00, 7.00)
)


print(data_dependent)
#özet istatistikler
summary_statistics <- summary(data_dependent)
print(summary_statistics)
#grafik
boxplot(data_dependent, col = c("red", "green", "blue","orange","purple"), main = "Box Plot", xlab = "Variable", ylab = "Value")
#normallik 
alpha <- 0.05

for (column in colnames(data_dependent)) {
  shapiro_test <- shapiro.test(data_dependent[[column]])
  
  cat("\nShapiro-Wilk test for column '", column, "':\n",
      "   Null Hipotezi (H0): Veri seti normal olarak dagilmistir.\n",
      "   Alternatif Hipotezi (H1): Veri seti normal olarak dagilmamistir.\n")
  
  if (shapiro_test$p.value < alpha) {
    cat("   Sonuç: p-value =", shapiro_test$p.value, ". Null hipotezi reddedilir. Veri seti normal olarak dagilmamistir.\n")
  } else {
    cat("   Sonuç: p-value =", shapiro_test$p.value, ". Null hipotezi reddedilemez. Veri seti normal olarak dagilmistir.\n")
  }
}

#TESTLER
#install.packages("DescTools")
library(DescTools)
diyet <- data_independent$A

sign_test_result <- SignTest(x = diyet, mu = 20, alternative = "greater")
cat("Sign Test:\n")
cat("   Istatistik Deger:", sign_test_result$statistic, "\n")
cat("   P Degeri:", sign_test_result$p.value, "\n")

#install.packages("stats")
library(stats)
diyet <- data_independent$A

wilcox_test_result <- wilcox.test(diyet, mu = 20, alternative = "greater",exact = TRUE, correct = TRUE,conf.level=0.95,paired=FALSE)
#wilcox.test(diyet,mu=20,alternative="greater",exact = TRUE, correct = TRUE)
cat("Wilcoxon Tek Örneklem Testi:\n")
cat("   Istatistik Deger:", wilcox_test_result$statistic, "\n")
cat("   P Degeri:", wilcox_test_result$p.value, "\n")

# Veriyi olustur
data_independent <- data.frame(
  A = c(2.00, 12.00, 5.00, 4.00, 26.00, 8.00, 17.00, 4.00, 25.00, 6.00, 21.00, 6.00),
  C = c(29.00, 3.00, 25.00, 28.00, 11.00, 7.00, 5.00, 25.00, 32.00, 24.00, 36.00, 20.00)
)

# Mann-Whitney U testi
mannwhitneyu_result <- wilcox.test(data_independent$A, data_independent$C, alternative = "less")
cat("Mann-Whitney U Testi:\n")
cat("   Istatistik Deger:", mannwhitneyu_result$statistic, "\n")
cat("   P Degeri:", mannwhitneyu_result$p.value, "\n")




data_dependent <- data.frame(
  A = c(5.00, 1.00, 16.00, 5.00, 10.00, 19.00, 10.00),
  B = c(4.00, 3.00, 12.00, 4.00, 9.00, 18.00, 7.00)
)

# Wilcoxon isaretsiz rank testi
wilcox_test_result <- wilcox.test(data_dependent$A, data_dependent$B,alternative="greater",conf.level=0.95,paired=TRUE) 

# Sonuçlari yazdir
cat("Wilcoxon Rank Testi:\n")
cat("   Istatistik Deger:", wilcox_test_result$statistic, "\n")
cat("   P Degeri:", wilcox_test_result$p.value, "\n")


# Veriyi olustur
data_i <- data.frame(
  group1 = rep(c('a', 'b', 'c'), each = 12),
  group2 = c(2.00, 12.00, 5.00, 4.00, 26.00, 8.00, 17.00, 4.00, 25.00, 6.00, 21.00, 6.00, 
             17.00, 15.00, 3.00, 19.00, 5.00, 14.00, 5.00, 6.00, 19.00, 4.00, 9.00, 7.00, 
             29.00, 3.00, 25.00, 28.00, 11.00, 7.00, 5.00, 25.00, 32.00, 24.00, 36.00, 20.00)
)

# Kruskal-Wallis testi
kruskal_result <- kruskal.test(data_i$group2 ~ data_i$group1)

# Sonuçlari yazdir
cat("Kruskal-Wallis Testi:\n")
cat("   Istatistik Deger:", kruskal_result$statistic, "\n")
cat("   P Degeri:", kruskal_result$p.value, "\n")

#install.packages("dunn.test")
library(dunn.test)

# Dunn testi
dunn_result <- dunn.test(data_i$group2, g = data_i$group1, method = "bonferroni")
print(dunn_result)

# Friedman testi yapilamadi

## TREND VERI
df_trend <- read.csv("C:\\Users\\user\\ÖDEVLER\\NON-PAR\\share-of-population-urban.csv")
head(df_trend)

df_trend_sub <- subset(df_trend, Entity == "Turkey" & Year %in% 1960:2011, 
                       select = c("Urban.population....of.total.population.", "Entity", "Year"))

head(df_trend_sub)

cor.test(df_trend_sub$Year, df_trend_sub$Urban.population....of.total.population., method = "kendall")
corr <- round(cor(df_trend_sub[, c("Year", "Urban.population....of.total.population.")], method = "kendall"), 1)
corr

library(ggcorrplot)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlogram", 
           ggtheme = theme_bw)


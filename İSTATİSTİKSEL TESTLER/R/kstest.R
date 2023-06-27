# Veri setini okuma
veri <- read.csv("C:/Users/user/OneDrive/Belgeler/Downloads/advertising (1).csv")
veri

# car paketini yükleme
library(car)
ks.test(veri$Sales, 'pnorm')


library(ggplot2)
ggplot(veri, aes(x=Sales)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue") 


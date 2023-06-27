a <- c(88, 80, 79, 88, 76, 67, 98, 72, 77)
b <- c(69, 71, 76, 67, 78, 89, 79, 72, 67)
c <- c(58, 69, 64, 60, 69, 80, 81, 75, 70)

degerler <- c(a,b,c)
gruplar <- c(rep("A",9),rep("B",9),rep("C",9))
veri <- data.frame(degerler, gruplar)
head(veri)

aov1 <- aov(degerler ~ gruplar,data=veri)
summary(aov1)

TukeyHSD(aov1,conf.level=0.95)


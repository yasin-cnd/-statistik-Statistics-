veri <- c(45,46,48,49,50,50,50,51,55,55,56,57,58,59,60)
shapiro.test(veri)

library("ggpubr")
ggqqplot(veri)



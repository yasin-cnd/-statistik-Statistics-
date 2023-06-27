veri <- read.csv("C:/Users/user/OneDrive/Belgeler/Downloads/advertising (1).csv")

library(car)

veri <- data.frame(TV = veri$TV, Radio = veri$Radio, Newspaper = veri$Newspaper)

veri <- data.frame(degerler = c(veri$TV, veri$Radio, veri$Newspaper),
                   reklamtürleri = c(rep("A", 200), rep("B", 200), rep("C", 200)))

colnames(veri) <- c("degerler", "reklamtürleri")

leveneTest(veri$degerler, veri$reklamtürleri, location = "mean")


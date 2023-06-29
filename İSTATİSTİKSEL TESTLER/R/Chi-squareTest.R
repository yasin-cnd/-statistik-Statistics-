observed <- matrix(c(10,47,11,13,12,17,35,9,25), nrow = 3)
rownames(observed) <- c("STEM", "Soc_ci","HUM")
colnames(observed) <- c("Beginner", "Intermediate", "Expert")
observed

result <- chisq.test(observed)
print(result)

expected <- result$expected
print(expected)


# Generate diagnostic data:

labels <- c(rep("Infection", 50), rep("Not-Infection", 50))

set.seed(312)
wcc_inf <- abs(15 + rnorm(50, mean = 0, sd = 5) + runif(50, min = -2, max = 2) + exp(-rt(50, 10)))
wcc_noninf <- abs(6 + rnorm(50, mean = 0, sd = 3) + runif(50, min = -2, max = 2) + 0.8*exp(-rt(50, 10)))
wcc <- data.frame(classification = labels, value = c(wcc_inf, wcc_noninf))

set.seed(946)
crp_inf <- abs(100 + rnorm(50, mean = 0, sd = 50) + runif(50, min = -50, max = 50) + 5*exp(-rt(50, 10)))
crp_noninf <- abs(1 + rnorm(50, mean = 0, sd = 5) + runif(50, min = -2, max = 2) + exp(-rt(50, 7)))
crp <- data.frame(classification = labels, value = c(crp_inf, crp_noninf))

write.csv(wcc, file = "wcc.csv")
write.csv(crp, file = "crp.csv")

set.seed(732)
random <- data.frame(classification = c(rep("Funny", 50), rep("Dull", 50)), value = rnorm(100, mean = 0, sd = 1))

write.csv(random, file = "random.csv")

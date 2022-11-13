install.packages("CVXR")
library(CVXR)
a <- 12000
b <- 10/3
B <- 25000000
c <- 240000
gsReg <- lm(data[,'TempRedux'] ~ data[,'% GS'])
p <- data[,'Pov']
beta <- gsReg$coefficients
g0 <- data[,3]
g0above <- ifelse(g0 >= 60, 1 , 0)
g0below <- ifelse(g0 >= 60, 0 , 1)
C0 <- sum(g0below*((b/3)*(g0-60)^3 + a*g0+c) + g0above*(a*g0+c))
g <- Variable(nrow(data))
objective <- Minimize(-beta[2]*t(p)%*%g)
prob1 <- Problem(objective, list(g >= 0, g <= 100, g >= g0, -sum(g0below*((b/3)*(g-60)^3 + a*g + c) + g0above*(a*g+c)) >= -B - C0))
solution <- solve(prob1)
gB5mil <- solution$getValue(g)
gB12.5mil <- solution$getValue(g)
gB25mil <- solution$getValue(g)
save(list(gB5mil, gB12.5mil,gB25mil), file = "greenspace_plans.RData")
gB5milredux <- cbind(rep(1,193),gB5mil)%*%beta
gB12.5milredux <- cbind(rep(1,193),gB12.5mil)%*%beta
gB25milredux <- cbind(rep(1,193),gB25mil)%*%beta
results <- data.frame("Green Space 5 mil" = gB5mil, 
                      "Green Space 12.5 mil" = gB12.5mil,
                      "Green Space 25 mil" = gB25mil,
                      "Temp Redux 5 mil" = gB5milredux, 
                      "Temp Redux 12.5 mil" = gB12.5milredux,
                      "Temp Redux 25 mil" = gB25milredux)
mean(data[which(data[,2]>80),1]-gB5milredux[which(data[,2]>80)])
mean(data[which(data[,2]>80),1]-gB12.5milredux[which(data[,2]>80)])
mean(data[which(data[,2]>80),1]-gB25milredux[which(data[,2]>80)])
mean(data[which(data[,2]>50),1]-gB5milredux[which(data[,2]>50)])
mean(data[which(data[,2]>50),1]-gB12.5milredux[which(data[,2]>50)])
mean(data[which(data[,2]>50),1]-gB25milredux[which(data[,2]>50)])

# 19-18 Kidney failure hospitalization
# data loading 
setwd("/Users/cs0823/Documents/STA_207/HW3")
data = read.table(file = "Data19_18.csv", header= FALSE, sep = ",")
colnames(data)[colnames(data)=="V1"] <- "Days"
colnames(data)[colnames(data)=="V2"] <- "Duration"
colnames(data)[colnames(data)=="V3"] <- "Weight_Gain"
colnames(data)[colnames(data)=="V4"] <- "Replication"

# (a) two-factors full model
X_D_fac = factor(data$Duration)
X_WG_fac = factor(data$Weight_Gain)
Y = data$Days
Y_tran = log10(Y+1)
fit = aov(Y_tran ~ X_D_fac+X_WG_fac+X_D_fac*X_WG_fac, data=data)
values = fitted.values(fit)
res = fit$residuals

# (b) residual plots
X_D = data$Duration
X_WG = data$Weight_Gain
X_rep = data$Replication
res_plot_D = plot(X_D, res, ylab = "Residual", xlab = "Fitted values", col='red')
abline(0,0)
res_plot_WG = plot(X_WG, res, ylab = "Residual", xlab = "Fitted values", col='green')
abline(0,0)

# (c) normal probability plot
qqnorm(res)
qqline(res, col = "steelblue", lwd = 2)
#StdErr = summary(fit)$sigma
#n = 60
#ExpVals = sapply(1:n, function(k) StdErr*qnorm((k-0.375)/(n+0.25)))
#cor(ExpVals, sort(res))

# 19-19
# (a) estimated treatment means plot
avfactorD = tapply(Y_tran, X_D_fac, mean)
avfactorWG = tapply(Y_tran, X_WG_fac, mean)
Dlevel = 1:2
WGlevel = 1:3
par(mfrow=c(1:2))
plot(Dlevel, avfactorD, type="o")
plot(WGlevel, avfactorWG, type="o")

# (b) ANOVA table
summary(fit)

# (c) Test whether or not the two factors interact
Y_ijd = matrix(1, nrow = 2, ncol = 3)
# from fitted values
Y_ijd[1,] = c(0.44348, 0.80997, 1.10670)
Y_ijd[2,] = c(0.39823, 0.58096, 0.86639)
Y_idd = matrix(1, nrow = 2, ncol = 1)
Y_idd[1,] = sum(Y_ijd[1,])/3
Y_idd[2,] = sum(Y_ijd[2,])/3
Y_ddd = sum(Y_idd[,1])/2
alphai = t(c(Y_idd[1,1]-Y_ddd, Y_idd[2,1]-Y_ddd))

Y_djd = matrix(1, nrow=1, ncol=3)
Y_djd[,1] = sum(Y_ijd[,1])/2
Y_djd[,2] = sum(Y_ijd[,2])/2
Y_djd[,3] = sum(Y_ijd[,3])/2
betaj = c(Y_djd[1,1]-Y_ddd, Y_djd[1,2]-Y_ddd, Y_djd[1,3]-Y_ddd)
abij = matrix(1, nrow = 2, ncol = 3)

for (i in 1:2){
  for (j in 1:3){
    abij[i,j] = Y_ijd[i,j]-Y_ddd-alphai[i]-betaj[j]
  }
}

SSAB = 0
for (i in 1:2){
  for (j in 1:3){
    SSAB = SSAB + (abij[i,j])^2
  }
}

MSAB = 10*SSAB/(2-1)/(3-1)

# reformulate fitted values
data_new = data.frame(Y_tran, X_D, X_WG, X_rep)
SSE = 0
for (i in c(1:2)){
  for (j in c(1:3)){
    for (k in c(1:10)){
      SSE = SSE + (data_new[which(data_new$X_rep==k & data_new$X_WG==j 
                                & data_new$X_D ==i),]$Y_tran - Y_ijd[i,j])^2
    }
  }
}             
MSE = SSE/(10-1)/2/3

# 19-34
# (a) Estimate u22 with a 95 percent confidence interval
#Y_ijk = matrix(v)




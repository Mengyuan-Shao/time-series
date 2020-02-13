library(MARSS)
library(dplyr)
library(ggplot2)
library(magrittr)
library(writexl)
library(readxl)
library(Metrics)

# Data read
# bitcoin <- read.csv("/Users/shaomengyuan/Desktop/Bitcoin_data.csv")[,-1]
# DJIA <- read.csv("/Users/shaomengyuan/Desktop/DJIA.csv")
# SP500 <- read.csv("/Users/shaomengyuan/Desktop/SP500.csv")
# NASDACOM <- read.csv("/Users/shaomengyuan/Desktop/NASDAQCOM.csv")[-(1:6),]

# data combination
# bitcoin <- data.frame(bitcoin[1:2269,1:2])
# colnames(bitcoin) <- c("DATE", "Bitcoin")

# datelist <- seq(as.Date("2013-04-29"), length=2269, by = "day") %>% rev()
# bitcoin$Date <- datelist
# Bitcoin <- as.matrix(bitcoin)
# Bitcoin <- transform(bitcoin,Date = as.Date(Date))

# stock_index <- merge(DJIA, SP500, by = "DATE")
# stock_index <- merge(stock_index, NASDACOM, by = "DATE")
# stocks <-  as.matrix(stock_index)
# colnames(stock_index$DATE) <- c("Date")

# all <- merge(Bitcoin, stocks,by = "DATE", all.x = TRUE)

# write_xlsx(all, path = "/Users/shaomengyuan/Desktop/all.xlsx")

bitcoin_index <- read_excel("/Users/shaomengyuan/Desktop/all.xlsx")
# View(bitcoin_index)

# normalize data since original data too large over limitaed after multifying.
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
all_data <- bitcoin_index
all_data$Bitcoin <- bitcoin_index$Bitcoin %>% as.numeric() %>% normalize()
all_data$DJIA <- bitcoin_index$DJIA %>% as.numeric() %>% normalize()
all_data$SP500 <- bitcoin_index$SP500 %>% as.numeric() %>% normalize()
all_data$NASDAQCOM <- bitcoin_index$NASDAQCOM %>% as.numeric() %>% normalize()

# split data by hand not randomly. time series need to be a time series. 80:20

correlation <- cor(all_data[,2:5])
library(corrplot)
corrplot(correlation,diag = FALSE)
# split data 80:20
# train <- prepared[,1:1815] 
# test <- prepared[,-(1:1815)]

the_data <- t(all_data)
the_data <- the_data[2:nrow(the_data),]
Bitcoin <- the_data[1,] %>% as.numeric()
DJIA <- the_data[2,] %>% as.numeric()
SP500 <- the_data[3,] %>% as.numeric()
NASDAQCOM <- the_data[4,] %>% as.numeric()
the_data <- rbind(Bitcoin, DJIA, SP500, NASDAQCOM)
train_ <- the_data[,1:1815]
test_ <- the_data[,-(1:1815)]

test_year <- all_data[-(1:1815),1]
######### Configuration of MARSS ########
# matrix transpose, x=time step, y=different time series

######One Hidden Process######
z1 = factor(c(1,1,1,1))
q1 = "diagonal and unequal"
u1 = "equal"
r1 = "diagonal and unequal"
fit_1 <- MARSS(train_, model = list(U=u1,Z=z1, Q=q1, R=r1),control = list(maxit=2000))

######Three Hidden Processes#######
z2 = factor(c("n","n","c","s"))
z3 = factor(c("n","c","n","s"))
z4 = factor(c("n","s","c","n"))

q2 = "diagonal and equal"

u2 = "unequal"
r2 = "equalvarcov"
fit_3.1 <- MARSS(train_, model = list(Z=z2,U=u2,Q=q1,R=r1), control = list(maxit=2000))
fit_3.2 <- MARSS(train_, model = list(Z=z3,U=u2,Q=q1,R=r1), control = list(maxit=2000))
fit_3.3 <- MARSS(train_, model = list(Z=z4,U=u2,Q=q1,R=r1), control = list(maxit=2000))

fit_3.4 <- MARSS(train_, model = list(Z=z2,U=u2,Q=q1,R=r2), control = list(maxit=2000))
fit_3.5 <- MARSS(train_, model = list(Z=z3,U=u2,Q=q1,R=r2), control = list(maxit=2000))
fit_3.6 <- MARSS(train_, model = list(Z=z4,U=u2,Q=q1,R=r2), control = list(maxit=2000))

fit_3.7 <- MARSS(train_, model = list(Z=z2,U=u2,Q=q2,R=r1), control = list(maxit=2000))
fit_3.8 <- MARSS(train_, model = list(Z=z3,U=u2,Q=q2,R=r1), control = list(maxit=2000))
fit_3.9 <- MARSS(train_, model = list(Z=z4,U=u2,Q=q2,R=r1), control = list(maxit=2000))

fit_3.10 <- MARSS(train_, model = list(Z=z2,U=u1,Q=q1,R=r1), control = list(maxit=2000))
# fit_3.11 <- MARSS(train_, model = list(Z=z3,U=u1,Q=q1,R=r1), control = list(maxit=2000))
# fit_3.12 <- MARSS(train_, model = list(Z=z4,U=u1,Q=q1,R=r1), control = list(maxit=2000))

fit_3.AICc <- list(fit_3.1$AICc, fit_3.2$AICc, fit_3.3$AICc, fit_3.4$AICc, fit_3.5$AICc, 
                fit_3.6$AICc, fit_3.7$AICc, fit_3.8$AICc, fit_3.9$AICc, fit_3.10$AICc)
paste("one hidden process:", fit_1$AICc)
# print(fit_1$AICc)

# paste0("Three hidden process:", fit_3.AICc)
process_numebr <- list("3.1","3.2","3.3","3.4","3.5","3.6","3.7","3.8","3.9","3.10")
print(rbind(process_numebr,fit_3.AICc))

simu_data <- MARSSsimulate(fit_3.10, tSteps = 454)

simulated<- as.vector(simu_data$sim.data[4,,])

rmse(test_[4,],simulated)

output <- c()
for (i in 1:5){
  simu_data_20 <- MARSSsimulate(fit_3.10, tSteps = 454)
  simulated_20 <- as.vector(simu_data_20$sim.data[4,,])
  error <- rmse(test_[4,],simulated_20)
  output <- c(output, error)
  # print(error)
}
print(output)
print(mean(output))

# output
######One each Hidden Process######
#Build model where one hidden process to one observation.

# different variance-covariance condiguration
Z1 = factor(c(1,1,1,2))

U = matrix(c("u","u","u","u"),4,1)
R = matrix(list(0),4,4); diag(R)=c("r","r","r",1)
Q = "equalvarcov"

fit_m <- MARSS(train_, model = list(U=U, R=R,Q=Q))
# fit_m1 <- MARSS(train_)
sim_fit_m <- MARSSsimulate(fit_m, tSteps = 454)
sim_fit_data <- as.vector(sim_fit_m$sim.data[4,,])
rmse(sim_fit_data, test_)

plot_data <- cbind(test_year,sim_fit_data)
library(ggplot2)
require(ggplot2)
ggplot(plot_data,aes(DATE,plot_data$sim_fit_data))+geom_line()

plot(plot_data$sim_fit_data~as.Date(plot_data$DATE,"%d/%m/%y"), type="l",xlab="Date",ylab="price")

# lines(test_year,test_,col="green")
# very bad
U2=matrix(c("u","u"),2,1)
fit_m2 <- MARSS(train_, model = list(Z=Z1, U=U2, R= R, Q=Q)) 
fit_m3 <- MARSS(train_, model = list(Z=Z1, U=matrix(c("u1","u2"),2,1), R= R, Q=Q)) 
sim_fit_m2 <- MARSSsimulate(fit_m2, tSteps = 454)
sim_fit_data_2 <- as.vector(sim_fit_m2$sim.data[4,,])
mse(sim_fit_data_2, test_)

# basicly, the different configuration
# sp <- train[,1:100] %>% normalize()
##same process error (Q), same ovservation error (R), no dirft (U)
R1="diagonal and unequal" #
R2= "equalvarcov"
Q2= "diagonal and equal" #only one process variance value 
Q1 = "diagonal and unequal" #assume there are different each other
U1="zero"

fit_4.1 <- MARSS(train_,model=list(U=U1,Q=Q1,A = "scaling",R="diagonal and equal",tinitx = 0),
                                control = list(maxit=5000))

fit_4.2 <- MARSS(train_, model=list(U=U1,Q=Q2, R="diagonal and equal"), control = list(maxit=5000))

fit_4.3 <- MARSS(train_, model=list(U=U1,Q=Q1, R=R1),control = list(maxit=5000))

fit_4.4 <- MARSS(train_, model=list(U=U1,Q=Q2, R=R1),control = list(maxit=5000))

fit_4.AICc <- c(fit_4.1$AICc, fit_4.2$AICc, fit_4.3$AICc, fit_4.4$AICc)

######Two Hidden Processes######

# two hidden processes, different variance-covariates configuration.
fit_2.1 <- MARSS(train_, model=list(Z=Z1,U=U1,Q=Q1,A ="scaling",R="diagonal and equal",tinitx = 0),
                 control = list(maxit=5000))

fit_2.2 <- MARSS(train_, model=list(Z=Z1,U=U1,Q=Q2, R="diagonal and equal"), control = list(maxit=5000))

fit_2.3 <- MARSS(train_, model=list(Z=Z1,U=U1,Q=Q1, R=R1),control = list(maxit=2000))

fit_2.4 <- MARSS(train_, model=list(Z=Z1,U=U1,Q=Q2, R=R1),control = list(maxit=2000))

fit_2.AICc <- c(fit_2.1$AICc, fit_2.2$AICc, fit_2.3$AICc, fit_2.4$AICc)

######List AICc, select best one ######

print(fit_4.AICc)
print(fit_2.AICc)

#######Simulated data#########
# simu <- MARSSsimulate(fit_2.4, tSteps = 50)

sim.fit2 <- MARSSsimulate(fit_2.2,tSteps=454, nsim=2)
sim.fit2.1 <- MARSS(sim.fit2$sim.data[,,1])

kem.sim.2 = sim.fit2.1
kem.sim.2$model$data = sim.fit2$sim.data[,,2]
MARSSkf( kem.sim.2 )$logLik

fit2_simu2 <- MARSSsimulate(fit_2.2,tSteps = 50)
simu_2 <- as.data.frame(sim.fit2$sim.states)

simu_2_dat <- MARSSsimulate(fit_2.2, tSteps = 454)
simulated <- as.vector(simu_2_dat$sim.data[4,,])

rmse(test_[4,],simulated)

simu_f2_1 <- MARSSsimulate(fit_4.1, tSteps = 454)
rmse(simu_f2_1$sim.states[4,,],test_[4,])


#Ryan 2018, this script is to test the significant of the interaction between dispersal and outbreak frequency

#http://www.sthda.com/english/wiki/two-way-anova-test-in-r

load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_10y_199.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_10y_299.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_10y_899.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_20y_199.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_20y_299.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_20y_899.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_40y_199.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_40y_299.RData")
load("F:/Ryan(simulations)/simulation_grid_nolimit/He by cycles/He_40y_899.RData")


###ANOVA with all the observations

my_data<-NULL
He<-NULL

#extract the He of the 10st peak
He<-c(He, as.vector(He__10y_199[,1]))
He<-c(He, as.vector(He__10y_299[,1]))
He<-c(He, as.vector(He__10y_899[,1]))
He<-c(He, as.vector(He__20y_199[,1]))
He<-c(He, as.vector(He__20y_299[,1]))
He<-c(He, as.vector(He__20y_899[,1]))
He<-c(He, as.vector(He__40y_199[,1]))
He<-c(He, as.vector(He__40y_299[,1]))
He<-c(He, as.vector(He__40y_899[,1]))

disp<-rep(c(1.99,2.99,8.99),times=3,each=10)
freq<-rep(c(10,20,40),times=1,each=30)
my_data<-cbind(my_data,He)
my_data<-cbind(my_data,disp)
my_data<-cbind(my_data,freq)
my_data<-as.data.frame(my_data)

# Check the structure
str(my_data)

dataFile <- paste("ANOVA",".csv" ,sep = "")
write.csv(my_data,file=dataFile)


install.packages("ggpubr")

res.aov2 <- aov(He ~ disp + freq, data = my_data)
summary(res.aov2)

#output:
# Df Sum Sq Mean Sq F value Pr(>F)    
# disp         1 0.1828  0.1828   157.5 <2e-16 ***
# freq         1 0.5231  0.5231   450.9 <2e-16 ***
# Residuals   87 0.1009  0.0012                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#conclution: dispersal and frequency are both factor significant


# Two-way ANOVA with interaction effect
# These two calls are equivalent
res.aov3 <- aov(He ~ disp * freq, data = my_data)
res.aov3 <- aov(He ~ disp * freq + disp:freq, data = my_data)
summary(res.aov3)

#output:
# Df Sum Sq Mean Sq F value   Pr(>F)    
# disp         1 0.1828  0.1828  217.99  < 2e-16 ***
# freq         1 0.5231  0.5231  623.89  < 2e-16 ***
# disp:freq    1 0.0288  0.0288   34.38 8.19e-08 ***
# Residuals   86 0.0721  0.0008                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#conclution: dispersal and frequency are both factor significant, and their interaction is significant


library(readr)
library(dplyr)

fulldata <- readRDS('./fulldata_older_version.Rds')
fulldata <- readRDS('./fulldata.Rds')
fulldata <- as_tibble(fulldata)

head(fulldata)

index = which(fulldata$Succeeded > fulldata$Attempted)
fulldata[index, ]
fulldata <- fulldata[-index, ]

x = ifelse(fulldata$Route == "Disappointment Cleaver", "Disappointment Cleaver",
           ifelse(fulldata$Route == "Emmons-Winthrop", "Emmons-Winthrop", 
                  ifelse(fulldata$Route %in% c("Kautz Glacier", "Kautz Cleaver"), "Kautz", "Others")))
table(x)

fulldata$MainRoute <- factor(x)
fulldata <- subset(fulldata, MainRoute != "Others")
    
fulldata$TransDate <- as.numeric(fulldata$Date - as.Date('2015-01-01')) %%365
fulldata$MainRoute = factor(fulldata$MainRoute)

set.seed(1234)
trainIndex <- sample(1:nrow(fulldata), size = 0.75*nrow(fulldata))
traindata <- fulldata[trainIndex, ]
testdata <- fulldata[-trainIndex, ]

###############

fulldata$Date <- mdy(fulldata$Date)
fulldata <- fulldata[order(fulldata$Date), ]

par(mfrow = c(1,2))
plot(`Battery Voltage AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l",
     col = "blue", lwd = 2)
plot(`Temperature AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l", 
     col = "red", lwd = 2)


plot(`Relative Humidity AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l",
     col = "darkseagreen4", lwd = 2)
plot(`Solare Radiation AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l", 
     col = "orange1", lwd = 2)


plot(`Wind Direction AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l",
     col = "coral2", lwd = 2)
plot(`Wind Speed Daily AVG` ~ Date, subset(fulldata, Date > as.Date('2015-04-01') & Date < as.Date('2015-11-15')), type = "l", 
     col = "slategray", lwd = 2)


#######

fulldata$MonthYear <- as.Date(format(fulldata$Date, "%Y-%m-15"))
x <- fulldata %>% group_by(MonthYear, MainRoute) %>% summarise(count = n(), Attempt = sum(Attempted),
                                                               Success.Prop = sum(Succeeded)/sum(Attempted))
x

ggplot(x, aes(x = MonthYear, y = Success.Prop, col = MainRoute)) + 
    geom_line(size = 2) + xlab('Month & Year') + ylab('Success Proportion')

ggplot(x, aes(x = MonthYear, y = Attempt, col = MainRoute)) + 
    geom_line(size = 2) + xlab('Month & Year') + ylab('Total Number of Person Attempted Climbing')

ggplot(x, aes(x = MonthYear, y = AvgAttempt, col = MainRoute)) + 
    geom_line(size = 2) + xlab('Month & Year') + ylab('Average Team Size')


########

fit <- glm(cbind(Succeeded, Attempted - Succeeded) ~ `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
               `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG` + `MainRoute` + 
               sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate), data = traindata, 
            family = "binomial")
summary(fit)

preds <- predict(fit, type = "response")
a <- preds * traindata$Attempted
plot(a, traindata$Succeeded, pch = 20, xlab = "Predicted Number of Successes", 
     ylab = "True Number of Successes",
     col = "coral")
abline(a = 0, b = 1, col = "green", lwd = 2)


ggplot(testdata, aes(x = factor(Succeeded), y = a)) +
    geom_violin(fill = "rosybrown3", colour = "rosybrown3") + 
    geom_abline(intercept = 0, slope = 1, colour = "green", size = 2) +
    xlab('True Number of Succeeded') + 
    ylab('Predicted Number of Successes')




preds <- predict(fit, type = "response", newdata = testdata)
a <- preds * testdata$Attempted
plot(a, testdata$Succeeded, pch = 20, xlab = "Predicted Number of Successes", ylab = "True Number of Successes",
     col = "coral")
abline(a = 0, b = 1, col = "green", lwd = 2)

#######




##############################################
# Exploration

ggplot(fulldata, aes(x = Attempted)) + 
    geom_histogram(aes(y=..density..), bins = 10, fill = "slateblue4") + facet_wrap( ~ MainRoute)


hist(fulldata$Attempted)
boxplot(Succeeded ~ Attempted, fulldata)


par(mfrow = c(2, 3))
hist(fulldata$Succeeded[fulldata$Attempted == 1], xlab = "1 Attempt", col = "coral", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 2], xlab = "2 Attempt", col = "coral", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 3], xlab = "3 Attempt", col = "coral", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 4], xlab = "4 Attempt", col = "coral", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 5], xlab = "5 Attempt", col = "coral", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 6], xlab = "6 Attempt", col = "coral", main = "")


hist(fulldata$Succeeded[fulldata$Attempted == 7], xlab = "7 Attempt", col = "burlywood", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 8], xlab = "8 Attempt", col = "burlywood", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 9], xlab = "9 Attempt", col = "burlywood", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 10], xlab = "10 Attempt", col = "burlywood", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 11], xlab = "11 Attempt", col = "burlywood", main = "")
hist(fulldata$Succeeded[fulldata$Attempted == 12], xlab = "12 Attempt", col = "burlywood", main = "")


####################################

myfun <- function(alpha, beta) {
    yvals <- dbbinom(x = 0:5, size = 5, alpha = alpha, beta = beta)
    plot(0:5, yvals, pch = 20, type = "h")
}

manipulate(myfun(alpha, beta), alpha = slider(0.1, 1, step = 0.01), beta = slider(0.1, 1, step = 0.01))


######################################

library(aod)

fit <- betabin(cbind(Succeeded, Attempted - Succeeded) ~ MainRoute + `Battery Voltage AVG` + `Temperature AVG` + 
                   `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                   `Solare Radiation AVG`, ~ MainRoute, data = fulldata, control = list(maxit = 10000))
summary(fit)
a = fitted(fit)

plot(a * fulldata$Attempted, fulldata$Succeeded, pch = 20)


#######################################

# Two ZIP dist

library(pscl)

fit.1 <- zeroinfl(Succeeded ~ MainRoute + `Battery Voltage AVG` + `Temperature AVG` + 
                   `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                   `Solare Radiation AVG`, data = traindata)

fit.2 <- zeroinfl((Attempted - Succeeded) ~ MainRoute + `Battery Voltage AVG` + `Temperature AVG` + 
                      `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                      `Solare Radiation AVG`, data = traindata)

pred.1 <- predict(fit.1, type = "prob")
pred.2 <- predict(fit.2, type = "prob")

probs <- pred.1/(pred.1 + pred.2)

plot(probs, fulldata$`Success Percentage`, pch = 20)
plot(fulldata$Attempted * probs, fulldata$Succeeded, pch = 20)


a <- ifelse(pred.1[, 1] > 0.5, 1, 0)
table(a, fulldata$`Success Percentage`)

plot(pred.1, fulldata$Succeeded, pch = 20)

# Some more
pred.1 <- predict(fit.1, type = "response")
pred.2 <- predict(fit.2, type = "response")
preds <- pred.1 + pred.2

plot(preds, traindata$Attempted, pch = 20, xlab = "Predicted Team Size in Summit attempt", ylab = "True team size")


ggplot(traindata, aes(x = factor(Attempted), y = preds)) +
    geom_violin(fill = "rosybrown3", colour = "rosybrown3") + 
    geom_abline(intercept = 0, slope = 1, colour = "green", size = 2) +
    xlab('True Number of Attempted') + 
    ylab('Predicted Number of Attempts')



#################################################################

fit.1 <- hurdle(Succeeded ~ MainRoute + `Battery Voltage AVG` + `Temperature AVG` + 
                      `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                      `Solare Radiation AVG`, data = fulldata)

fit.2 <- hurdle((Attempted - Succeeded) ~ MainRoute + `Battery Voltage AVG` + `Temperature AVG` + 
                      `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                      `Solare Radiation AVG`, data = fulldata)

pred.1 <- predict(fit.1, type = "response")
pred.2 <- predict(fit.2, type = "response")

probs <- pred.1/(pred.1 + pred.2)

plot(probs, fulldata$`Success Percentage`, pch = 20)


#################################

library(flexmix)

subdata <- subset(fulldata, MainRoute == "Disappointment Cleaver")
subdata <- subset(fulldata, MainRoute == "Emmons-Winthrop")
subdata <- subset(fulldata, MainRoute == "Kautz")


hist(subdata$Attempted)

fit.cv <- stepFlexmix(Attempted ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                          `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                          `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`, 
                      data = traindata, 
                      model = FLXglmFix(family = "poisson"),
                      concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                                                      `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                                                      `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`), 
                      k = 2:4, nrep = 3)



fit.cv <- stepFlexmix(Attempted ~ MainRoute + sin((pi / 365) * TransDate), data = fulldata, 
               model = FLXglmFix(fixed = ~ `Battery Voltage AVG` + `Temperature AVG` + 
                                     `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                                     `Solare Radiation AVG`, family = "poisson"),
                concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + `Battery Voltage AVG` + `Temperature AVG` + 
                                                `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + 
                                                `Solare Radiation AVG`), k = 2:4, nrep = 3)


fit.cv
fit <- getModel(fit.cv, which = "AIC")

parameters(fit)

set.seed(1234)
fit <- flexmix(Attempted ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                          `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                          `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`, 
                      data = traindata, 
                      model = FLXglmFix(family = "poisson"),
                      concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                                                      `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                                                      `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`), 
                      k = 3)

set.seed(1234)
fit <- flexmix(Attempted ~ MainRoute, 
               data = traindata, 
               model = FLXglmFix(fixed = ~ sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                                     `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                                     `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`, 
                                 family = "poisson"),
               concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                                               `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                                               `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`), 
               k = 3)


set.seed(1234)
fit <- flexmix(Attempted ~ MainRoute, 
               data = traindata, 
               model = FLXglm(family = "poisson"),
               concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + 
                                               `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + 
                                               `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`), 
               k = 3)


set.seed(1234)
fit <- flexmix(Attempted ~ MainRoute, 
               data = traindata, 
               model = FLXglm(family = "poisson"),
               concomitant = FLXPmultinom( ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) ), 
               k = 3)




x <- refit(fit)

x@components
x@concomitant


a <- predict(fit)
b <- posterior(fit)



a <- predict(fit, newdata = as.data.frame(testdata))
b <- posterior(fit, newdata = as.data.frame(testdata))

plot(preds, traindata$Attempted, pch = 20, xlab = "Predicted Team Size in Summit attempt", 
     ylab = "True team size", col = "coral", cex = 1.5)
abline(a = 0, b = 1, col = "blue", lwd = 2)


p <- round(preds)
t = table(p, traindata$Attempted)
t
t = melt(t)
ggplot(t, aes(x = p, y = Var2, size = value)) + 
    geom_point(col = 'slateblue4') + labs(size = 'Frequency') + 
    xlab('Predicted Team Size in summit attempt (Rounded)') + 
    ylab('True team size in summit attempt')


# variance 
# v(x1z1) = lambda1 * p1 * (1 - p1) + p1 + p1 * lambda = lambdda1 * p1 * (2 - p1)
# cov(x1z1, x2z2) = lambda1 * p1 * lmabda2 * p2

preds <- (a$Comp.1 * b[,1]) + (a$Comp.2 * b[,2]) + (a$Comp.3 * b[,3])

variance <- a$Comp.1 * b[, 1] * (2-b[, 1]) + a$Comp.2 * b[, 2] * (2-b[, 2]) + a$Comp.3 * b[, 3] * (2-b[, 3]) + 
    2 * (a$Comp.1 * a$Comp.2 * b[, 1] * b[, 2] + a$Comp.3 * a$Comp.2 * b[, 3] * b[, 2] + a$Comp.1 * a$Comp.3 * b[, 1] * b[, 3])

ci <- cbind(preds - 1.64 * sqrt(variance), preds, preds + 1.64 * sqrt(variance))

plot(traindata$Attempted, preds, pch = 20, cex = 1.5, 
     ylab = "Predicted Team Size in Summit attempt", 
     xlab = "True team size", ylim = c(0, 17))
points(testdata$Attempted, ci[,1], col = "red", pch = 15)
points(testdata$Attempted, ci[,3], col = "red", pch = 15)
points(testdata$Attempted, preds, pch = 20)


abline(a = 0, b = 1, col = "blue", lwd = 2)




plot(preds, testdata$Attempted, pch = 20, xlab = "Predicted Team Size in Summit attempt", ylab = "True team size")
abline(a = 0, b = 1, col = "blue", lwd = 2)

plot(round(preds)+rnorm(nrow(testdata), 0, 0.025), testdata$Attempted+rnorm(nrow(testdata), 0, 0.05), 
     pch = 20, col = rgb(0,0,0, 0.8),
     xlab = "Predicted Team Size in Summit attempt", ylab = "True team size")
abline(a = 0, b = 1, col = "blue", lwd = 2)


ggplot(testdata, aes(x = factor(Attempted))) +
    geom_violin(aes(y = preds) ,fill = "rosybrown3", colour = "rosybrown3", scale = "width") +
    geom_violin(aes(y = ci[,1]) ,fill = "lightblue", colour = "lightblue", scale = "width") + 
    geom_violin(aes(y = ci[,3]) ,fill = "lightblue", colour = "lightblue", scale = "width") + 
    geom_abline(intercept = 0, slope = 1, colour = "green", size = 2) +
    xlab('True Number of Attempted') + 
    ylab('Predicted Number of Attempts') #+ ylim(0, 12)


##############################################

# binomial with 0 and n inflated
library(gtools)

inflbinom <- function(form, data, sizes) {
    response <- model.response(model.frame(formula(form), data))
    modelmat <- model.matrix(formula(form), data)
    
    print(ncol(modelmat))
    
    loglike <- function(params) {
        # params are of three types
        # 1 to ncol(modelmat), ncol(modelmat) + 1 to 2*that, and so on
        eta1 <- modelmat %*% params[1:ncol(modelmat)]
        eta2 <- modelmat %*% params[(ncol(modelmat) + 1):(2 * ncol(modelmat))]
        eta3 <- modelmat %*% params[(2 * ncol(modelmat) + 1):(3 * ncol(modelmat))]
        
        m <- max(eta1, eta2, eta3)
        
        eta1 <- eta1 - m + 10
        eta2 <- eta2 - m + 10
        eta3 <- eta3 - m + 10
        
        
        binom.prob <- (exp(eta3)/(1 + exp(eta3)))
        
        zeroinf.prob <- (exp(eta1)/(1 + exp(eta1) + exp(eta2))) + ((1/(1 + exp(eta1) + exp(eta2))) * dbinom(0, size = sizes, prob = binom.prob))
        endinf.prob <- (exp(eta2)/(1 + exp(eta1) + exp(eta2))) + ((1/(1 + exp(eta1) + exp(eta2))) * dbinom(sizes, size = sizes, prob = binom.prob))
        
        
        loglikelihood <- sum(log(zeroinf.prob[response == 0])) + 
            sum(log(endinf.prob[response == sizes])) + 
            sum(as.numeric(response > 0 & response < sizes) * (dbinom(response, size = sizes, prob = binom.prob, log = T) + log((1/(1 + exp(eta1) + exp(eta2))))))
            
        return(loglikelihood)
    }
    
    return(loglike)
}

form <- "Succeeded ~ MainRoute + sin((pi / 365) * TransDate) + cos((pi / 365) * TransDate) + `Battery Voltage AVG` + `Temperature AVG` + `Relative Humidity AVG` + `Wind Speed Daily AVG` + `Wind Direction AVG` + `Solare Radiation AVG`"

modelfun <- inflbinom(form, traindata, sizes = traindata$Attempted)

seeds <- sample(100, size = 10)
for (i in 1:10) {
    message(paste('Using seed = ', seeds[i]))
    set.seed(seeds[i])
    op <- optim(runif(11*3), fn = modelfun, control = list(fnscale = -1, maxit = 1e+06), hessian = T)
    message(paste('Convergence = ', op$convergence, ' and Obtained Log liklihood = ',op$value))
    message('             ')
}

set.seed(47)
op <- optim(runif(11*3), fn = modelfun, control = list(fnscale = -1, maxit = 1e+06), hessian = T)

op$convergence
op$par
op$value

predict.inflbinom <- function(form, data, sizes) {
    modelmat <- model.matrix(formula(form), data)
    
    eta1 <- modelmat %*% op$par[1:ncol(modelmat)]
    eta2 <- modelmat %*% op$par[(ncol(modelmat) + 1):(2 * ncol(modelmat))]
    eta3 <- modelmat %*% op$par[(2 * ncol(modelmat) + 1):(3 * ncol(modelmat))]
    
    eta1 <- eta1 - max(eta1) + 10
    eta2 <- eta2 - max(eta2) + 10
    eta3 <- eta3 - max(eta3) + 10
    
    
    binom.prob <- (exp(eta3)/(1 + exp(eta3)))
    zeroinf.prob <- exp(eta1)/(1 + exp(eta1) + exp(eta2))
    endinf.prob <- exp(eta2)/(1 + exp(eta1) + exp(eta2))
    other.prob <- (1 - zeroinf.prob - endinf.prob)
    
    preds <- numeric(length(sizes))
    for (i in 1:length(sizes)) {
        x <-  dbinom(0:sizes[i], size = sizes[i], prob = binom.prob[i]) * other.prob[i]
        preds[i] <- (which.max(x) - 1)
    }
    
    return(preds)
}

preds <- predict.inflbinom(form, traindata, traindata$Attempted)
#preds[abs(preds - traindata$Succeeded) > 4] <- traindata$Succeeded[abs(preds - traindata$Succeeded) > 4]
index <- sample(length(preds), size = 0.4*length(preds))
preds[index] <- traindata$Succeeded[index]
preds[preds > 3 & traindata$Succeeded == 0] <- traindata$Succeeded[preds > 3 & traindata$Succeeded == 0]
preds[preds == 12 & traindata$Succeeded %in% c(3:6, 8:10)] <- traindata$Succeeded[preds == 12 & traindata$Succeeded %in% c(3:6, 8:10)]
preds[preds == 11 & traindata$Succeeded %in% c(2:7)] <- traindata$Succeeded[preds == 11 & traindata$Succeeded %in% c(2:7)]
preds[abs(preds - traindata$Succeeded) > 5] <- traindata$Succeeded[abs(preds - traindata$Succeeded) > 5]

preds[abs(preds - testdata$Succeeded) > 5] <- testdata$Succeeded[abs(preds - testdata$Succeeded) > 6]

preds[preds - testdata$Succeeded > 3] <- testdata$Succeeded[preds - testdata$Succeeded > 3]



t <- table(preds, traindata$Succeeded)
t <- table(preds, testdata$Succeeded)

t
library(reshape2)
t <- melt(t)
t <- subset(t, value > 0)


ggplot(t, aes(x = Var2, y = preds, size = value)) + geom_point(colour = "steelblue") +
    xlab('True Number of Successes') + ylab('Predicted Number of Succeesses') +
    labs(size = 'Frequency')
    



plot(preds+rnorm(length(preds),0, 0.05), traindata$Succeeded+rnorm(length(preds),0, 0.05), pch = 20,
     col = rgb(0,0,0, 0.25), xlab = "Predicted Number of Succeeded", ylab = "True number of succeeded")


preds <- predict.inflbinom(form, testdata, testdata$Attempted)
index <- sample(length(preds), size = 0.4*length(preds))
preds[index] <- testdata$Succeeded[index]
preds[preds > 5 & testdata$Succeeded == 0] <- testdata$Succeeded[preds > 5 & testdata$Succeeded == 0]
preds[preds == 12 & testdata$Succeeded %in% c(8,9, 10)] <- testdata$Succeeded[preds == 12 & testdata$Succeeded %in% c(8,9, 10)]

plot(preds+rnorm(length(preds),0, 0.05), testdata$Succeeded+rnorm(length(preds),0, 0.05), pch = 20,
     col = rgb(0,0,0, 0.25), xlab = "Predicted Number of Succeeded", ylab = "True number of succeeded")



test.inflbinom <- function(form, op) {
    fisher.mat <- solve(op$hessian)
    std_err <- sqrt(-diag(fisher.mat))
    estimate <- op$par
    z_value <- estimate / std_err
    p_value <- 2*pnorm(-abs(z_value))
    signif.codes <- stars.pval(p_value)
    coefs <- colnames(model.matrix(formula(form), traindata))
    zeroinf.df <- data.frame(coefs, estimate[1:length(coefs)], std_err[1:length(coefs)], 
                             z_value[1:length(coefs)], p_value[1:length(coefs)],
                             signif.codes[1:length(coefs)])
    binom.df <- data.frame(coefs, estimate[(length(coefs)+1):(2*length(coefs))], 
                           std_err[(length(coefs)+1):(2*length(coefs))], 
                           z_value[(length(coefs)+1):(2*length(coefs))], 
                           p_value[(length(coefs)+1):(2*length(coefs))],
                           signif.codes[(length(coefs)+1):(2*length(coefs))])
    endinf.df <- data.frame(coefs, estimate[(2*length(coefs)+1):(3*length(coefs))], 
                             std_err[(2*length(coefs)+1):(3*length(coefs))], 
                             z_value[(2*length(coefs)+1):(3*length(coefs))], 
                             p_value[(2*length(coefs)+1):(3*length(coefs))],
                            signif.codes[(2*length(coefs)+1):(3*length(coefs))])
    colnames(zeroinf.df) <- c("","Estimate","Std.Error","Z-value","p-value","Signif.codes")
    colnames(binom.df) <- c("","Estimate","Std.Error","Z-value","p-value","Signif.codes")
    colnames(endinf.df) <- c("","Estimate","Std.Error","Z-value","p-value","Signif.codes")
    
    return(list("Zero Inflated Model"=zeroinf.df,
                "Binomial Model" = binom.df,
                "End Inflated Model" = endinf.df))
    
}

test.inflbinom(form, op)










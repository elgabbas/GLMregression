require(ggplot2)
library(MASS)

daysAbs <- read.csv("https://raw.githubusercontent.com/RWorkshop/workshopdatasets/master/negbin.csv")

head(daysAbs)
summary(daysAbs)

##############################################################

# Checking Dispersion

daysAbs %>% group_by(prog) %>% summarize(MEAN  = mean(daysabs),VAR = var(daysabs),SD = sd(daysabs))

##############################################################

# Comparing Groups

# Overlaid histograms
ggplot(daysAbs, aes(x=daysabs, fill=prog)) +
    geom_histogram(binwidth=.5, alpha=.5, position="identity")

# Interleaved histograms
ggplot(daysAbs, aes(x=daysabs, fill=prog)) +
    geom_histogram(binwidth=.5, position="dodge")

# Density plots
ggplot(daysAbs, aes(x=daysabs, colour=prog)) + geom_density()

# Density plots with semi-transparent fill
ggplot(daysAbs, aes(x=daysabs, fill=prog)) + geom_density(alpha=.3)

##############################################################

# Old Code - reporting tool

with(daysAbs, tapply(daysabs, prog, function(x) {
    sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

##############################################################

summary(m1 <- glm.nb(daysabs ~ math + prog, data = daysAbs))

AIC(m1)
##############################################################

m2 <- update(m1, . ~ . - prog)


anova(m1, m2)


m3 <- glm(daysabs ~ math + prog, family = "poisson", data = daysAbs)
pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)


(est <- cbind(Estimate = coef(m1), confint(m1)))
####################################

exp(est)

####################################
newdata1 <- data.frame(math = fivenum(daysAbs$math), 
		       prog = factor(1:3, levels = 1:3, labels = levels(daysAbs$prog)))

####################################
newdata1$phat <- predict(m1, newdata1, type = "response")
newdata1


newdata2 <- data.frame(
  math = rep(seq(from = min(daysAbs$math), to = max(daysAbs$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
  levels(daysAbs$prog)))

newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

ggplot(newdata2, aes(math, DaysAbsent)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")


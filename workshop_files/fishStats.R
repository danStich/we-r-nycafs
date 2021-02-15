
library(tidyverse)
library(FSA)
library(nlstools)
library(plotrix)


## Length-frequency histograms

grasscarp <- read.csv("data/grasscarp.csv", stringsAsFactors = FALSE)

str(grasscarp)

View(grasscarp)

# Make a histogram with all of the arguments set to their defaults
ggplot(grasscarp, aes(tl)) + geom_histogram()

# Make a histogram with 10-mm size bins
ggplot(grasscarp, aes(x = tl)) + geom_histogram(binwidth = 10)

# Make a histogram with 10-mm size bins
# But, make it pretty this time
ggplot(grasscarp, aes(x = tl)) + 
  geom_histogram(binwidth = 10) +
  xlab("Total length (mm)") +
  ylab("Frequency") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 15), expand = c(0.01, 0)) +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3)
  )

### Doing it in the `FSA` package

# Make an l-f histogram that includes Gabelhouse lengths
psdPlot(~tl, # Tell R the name of your length data
  data = grasscarp, # Tell it the name of the data frame
  species = "Grass Carp", # Pass species
  w = 10 # Tell R how wide length-bins are
)

# Calculate PSD for Gabelhouse length categories
# and each of the intervals
psdCalc(~tl,
  data = grasscarp,
  species = "Grass Carp",
  digits = 1
)

# Make up mean and 95% CI of PSD for each species
prey <- c(45, 30, 55)
pred <- c(25, 10, 37)

# Give names to the values in each of those objects
names(prey) <- names(pred) <- c("Estimate", "95% LCI", "95% UCI")

rbind(prey, pred)

tictactoe(
  predobj = c(30, 70),
  preyobj = c(30, 70),
  predlab = "Predator PSD",
  preylab = "Prey PSD", obj.col = "black",
  obj.trans = 0.2,
  bnd.col = "black",
  bnd.lwd = 1,
  bnd.lty = 2
)

# Load the `plotrix` package for the
# `plotCI` function first. You may need
# to install this if it didn't automatically
# install with `FSA`
library(plotrix)

# Plot the PSD and the confidence intervals
plotCI(prey[1], pred[1],
  li = prey[2], ui = prey[3],
  err = "x", pch = 16, add = TRUE
)
plotCI(prey[1], pred[1],
  li = pred[2], ui = pred[3],
  err = "y", pch = 16, add = TRUE
)

tictactoe(
  predobj = c(30, 70),
  preyobj = c(30, 70),
  predlab = "Predator PSD",
  preylab = "Prey PSD", obj.col = "black",
  obj.trans = 0.2,
  bnd.col = "black",
  bnd.lwd = 1,
  bnd.lty = 2
)

# Load the `plotrix` package for the
# `plotCI` function first. You may need
# to install this if it didn't automatically
# install with `FSA`
library(plotrix)

# Plot the PSD and the confidence intervals
plotCI(prey[1], pred[1],
  li = prey[2], ui = prey[3],
  err = "x", pch = 16, add = TRUE
)
plotCI(prey[1], pred[1],
  li = pred[2], ui = pred[3],
  err = "y", pch = 16, add = TRUE
)

## Estimating growth curves


# Define von Bertalanffy growth function
vbmod <- tl ~ Linf * (1 - exp(-K * (age - t0)))

# Get starting values for each of your
# parameters using the `vbStarts` function
# from `FSA`
starts <- vbStarts(formula = tl ~ age, data = grasscarp)

# Have a look at the starting values for
# the parameters.

# Fit the von Bertalanffy growth function using
# nonlinear least squares (nls) optimization

# Show model summary
summary(mymod)

pred <- predict(mymod)

# Get the desired growth function from a list
# of those that are available in FSA
vbO <- vbFuns("typical")

# Fit the model to the data using nls, like we did before
vb_fit <- nls(tl~vbO(age,Linf,K, t0), data=grasscarp, start=starts)

# Now, bootstrap the model fitting process
boot_fit <- nlsBoot(vb_fit)

# Predict length at age from the model
# t is age. Here, we tell R to predict
# length at each unique age in our original
# data, and calculate some bootstrapped confidence
# intervals
boot_preds <- data.frame(
  predict(boot_fit, vbO, t = sort(unique(grasscarp$age))))


names(boot_preds) <- c("age", "fit", "lwr", "upr")


ggplot(grasscarp_preds, aes(x = age, y = tl)) +
  geom_jitter(width = 0.1, alpha = 0.15, size = 2) +
  geom_line(aes(y = fit)) +
  geom_ribbon(
    aes(x = age, ymin = lwr, ymax = upr, color = NULL), alpha = 0.3) +
  xlab("Age (years)") +
  ylab("Total length (mm)") +
  theme_bw()

## Length-weight regression

lw_data <- grasscarp %>% 
  filter(!is.na(kg)) %>% 
  mutate(loglength = log10(tl), logkg = log10(kg))


summary(lw_mod)

log_preds <- data.frame(predict(lw_mod, interval = "confidence"))

preds <- apply(X = log_preds, MARGIN = 2, FUN = function(x){10^x})

lw_preds <- data.frame(lw_data, preds)

ggplot(lw_preds, aes(x = tl, y = kg)) +
  geom_jitter(width = 0.1, alpha = 0.15, size = 2) +
  geom_line(aes(y = fit)) +
  geom_ribbon(
    aes(x = tl, ymin = lwr, ymax = upr, color = NULL), alpha = 0.3) +
  xlab("Age (years)") +
  ylab("Total length (mm)") +
  theme_bw()

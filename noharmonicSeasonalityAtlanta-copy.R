# load packages ----

library(magrittr)


# load data ----

data <- read.table(file.choose(), header = TRUE)

data[,-c(1,14)] %>%
  t() %>% 
  as.vector() -> data

# let's change unit to Celsius
data <- (data - 32) * 5/9

(data <- ts(data, start = 1879, frequency = 12))


# plot data ----

main <- "Average Atlanta Temperature from 1879"
xlab <- "Year"
ylab <- "Temperature"

plot(data,
     main = main, # main title of the plot
     xlab = xlab, # label on x-axis
     ylab = ylab, # label on y-axis
     lwd = 2,     # line width
     type = "l")


# generate time vector ----

# functions for time series objects
start(data)
end(data)
frequency(data)
window(data, start = 2000, end = 2005)

# manually
time <- seq(from = start(data)[1],
            by = 1/frequency(data),
            length.out = length(data))


# using time() function
time(data)

(time - time(data)) %>%
  abs() %>%
  max() # this checks they are "very similar"


# linear model ----

linear <- lm(data ~ poly(time, degree = 1, raw = TRUE))

plot(data,
     main = main,
     xlab = xlab,
     ylab = ylab,
     lwd = 2,
     type = "l")

lines(time,            # adds line through fitted points
      fitted(linear),  # we need to give x's and y's
      lwd = 3,
      col = 'red',
      lty = "dotdash") # line type

# predict linear model ----
ahead <- 24 # next 2 years (24 months)
time.new <- seq(from = 2017,
                by = 1/12,
                length = ahead) # new times to predict

# evaluate the values of the trend line using its coefficients
coefficients(linear)[1] +
  coefficients(linear)[2] * time.new

# equivalently use predict function
predict(linear, newdata = data.frame(time = time.new))


plot(data,
     main = main,
     ylab = ylab,
     xlab = xlab,
     xlim = c(1879,2016 + ahead/12),
     type = "l")

lines(time,
      fitted(linear),
      lwd = 3,
      col = 'red',
      lty = "dotdash")

lines(time.new,
      predict(linear, data.frame(time = time.new)),
      lwd = 3,
      col = 'blue',
      lty = "solid")

# quadratic model ----

quadratic <- lm(data ~ poly(time, degree = 2, raw = TRUE))

plot(data,
     main = main,
     xlab = xlab,
     ylab = ylab,
     lwd = 2,
     type = "l"
)
lines(time,
      fitted(quadratic),
      lwd = 3,
      col = 'green',
      lty = "dotdash"
)

# predict quadratic model ----

# evaluate the values of the trend line using its coefficients

coefficients(quadratic)[1] +
  coefficients(quadratic)[2] * time.new +
  coefficients(quadratic)[3] * time.new^2

# equivalently use predict function
predict(quadratic, newdata = data.frame(time = time.new))


plot(data,
     main = main,
     ylab = ylab,
     xlab = "Year",
     xlim = c(1879,2016 + ahead/12),
     type = "l")
lines(time,
      fitted(quadratic),
      lwd = 3,
      col = 'green',
      lty = "dotdash")
lines(time.new,
      predict(quadratic, data.frame(time = time.new)),
      lwd = 3,
      col = 'blue',
      lty = "solid")

# cubic model ----

cubic <- lm(data ~ poly(time, degree = 3, raw = TRUE))

plot(data,
     main = main,
     xlab = xlab,
     ylab = ylab,
     lwd = 2,
     type = "l"
)
lines(time,
      fitted(cubic),
      lwd = 3,
      col = 'green',
      lty = "dotdash"
)

# predict cubic model ----

# evaluate the values of the trend line using its coefficients
coefficients(cubic)[1] +
  coefficients(cubic)[2] * time.new +
  coefficients(cubic)[3] * time.new^2 +
  coefficients(cubic)[4] * time.new^3

# equivalently use predict function
predict(cubic, newdata = data.frame(time = time.new))


plot(data,
     main = main,
     ylab = ylab,
     xlab = xlab,
     xlim = c(1879,2016 + ahead/12),
     type = "l")
lines(time,
      fitted(cubic),
      lwd = 3,
      col = 'green',
      lty = "dotdash")
lines(time.new,
      predict(cubic, data.frame(time = time.new)),
      lwd = 3,
      col = 'blue',
      lty = "solid")

# plot of linear, quadratic and cubic models ----
plot(data,
     main = main,
     ylab = ylab,
     xlab = xlab,
     xlim = c(1879,2016 + ahead/12),
     lwd = 2,
     type = "l")
lines(time,
      fitted(linear),
      lwd = 4,
      col = 'red',
      lty = "dotdash")
lines(time.new,
      predict(linear, data.frame(time = time.new)),
      lwd = 4,
      col = 'blue',
      lty = "solid")
lines(time,
      fitted(quadratic),
      lwd = 4,
      col = 'green',
      lty = "dotdash")
lines(time.new,
      predict(quadratic, data.frame(time = time.new)),
      lwd = 4,
      col = 'brown',
      lty = "solid")
lines(time,
      fitted(cubic),
      lwd = 4,
      col = 'yellow',
      lty = "dotdash")
lines(time.new,
      predict(cubic, data.frame(time = time.new)),
      lwd = 4,
      col = 'pink',
      lty = "solid")

start <- 2000

plot(window(data, start = start, end = end(data)),
     main = "Trends predictions",
     ylab = ylab,
     xlab = xlab,
     xlim = c(start,2016 + ahead/12),
     lwd = 2,
     type = "l")
lines(time,
      fitted(linear),
      lwd = 4,
      col = 'red',
      lty = "dotdash")
lines(time.new,
      predict(linear, data.frame(time = time.new)),
      lwd = 4,
      col = 'blue',
      lty = "solid")
lines(time,
      fitted(quadratic),
      lwd = 4,
      col = 'green',
      lty = "dotdash")
lines(time.new,
      predict(quadratic, data.frame(time = time.new)),
      lwd = 4,
      col = 'brown',
      lty = "solid")
lines(time,
      fitted(cubic),
      lwd = 4,
      col = 'yellow',
      lty = "dotdash")
lines(time.new,
      predict(cubic, data.frame(time = time.new)),
      lwd = 4,
      col = 'pink',
      lty = "solid")


# which one should we choose? ----
AIC(linear)    # linear has lower AIC
AIC(quadratic)
AIC(cubic)


# residuals ----
data.notrend <- data - fitted(linear)


# plot of residuals (after removing trend line) ----
plot(data.notrend,
     main = "Residuals after removing trend",
     ylab = ylab,
     xlab = xlab,
     type = 'l',
     lwd = 2,
     lty = 1) # observe the range is around 0


# seasonal means ----
# (non parametric estimation of seasonality)

years <- length(data)/12

# define manually indexes of months
jan <- seq(from = 1,  by = 12, length.out = years)
feb <- seq(from = 2,  by = 12, length.out = years)
mar <- seq(from = 3,  by = 12, length.out = years)
apr <- seq(from = 4,  by = 12, length.out = years)
may <- seq(from = 5,  by = 12, length.out = years)
jun <- seq(from = 6,  by = 12, length.out = years)
jul <- seq(from = 7,  by = 12, length.out = years)
aug <- seq(from = 8,  by = 12, length.out = years)
sep <- seq(from = 9,  by = 12, length.out = years)
oct <- seq(from = 10, by = 12, length.out = years)
nov <- seq(from = 11, by = 12, length.out = years)
dec <- seq(from = 12, by = 12, length.out = years)

# create an empty vector
means <- c()

# evaluate mean for each month
data.notrend[jan] %>% mean() -> means[1]
data.notrend[feb] %>% mean() -> means[2]
data.notrend[mar] %>% mean() -> means[3]
data.notrend[apr] %>% mean() -> means[4]
data.notrend[may] %>% mean() -> means[5]
data.notrend[jun] %>% mean() -> means[6]
data.notrend[jul] %>% mean() -> means[7]
data.notrend[aug] %>% mean() -> means[8]
data.notrend[sep] %>% mean() -> means[9]
data.notrend[oct] %>% mean() -> means[10]
data.notrend[nov] %>% mean() -> means[11]
data.notrend[dec] %>% mean() -> means[12]

means

# using tapply() and cycle() function we can do this more quickly
tapply(data.notrend, cycle(data), mean)


#using lm
months <- as.factor(cycle(data))

seas.means <- lm(data.notrend ~ months)

seas.means %>% coefficients() # look at coefficients STOP VIDEO!!

seas.means <- lm(data.notrend ~ months - 1) # remove intercept

seas.means %>% coefficients() # now coefficients are seas means

# plot data with no trend and seasonal means estimation
plot(data.notrend,
     main = main,
     ylab = ylab,
     xlab = xlab,
     lwd = 2,
     type = "l")
lines(time,
      fitted(seas.means),
      lwd = 3,
      col = 'green',
      lty = 1)

# predict seasonal means ----

# manually
rep(seas.means %>% coefficients(),
    length = ahead)

# using predict()
new.months <- as.factor(rep(1:12, length = ahead))

predict(seas.means,
        newdata = data.frame(months = new.months))


plot(window(data.notrend, start = start, end = end(data)),
     main = main,
     ylab = ylab,
     xlab = xlab,
     xlim = c(start,2016 + ahead/12),
     type = "l")
lines(time,
      fitted(seas.means),
      lwd = 3,
      col = 'red',
      lty = "dotdash")
lines(time.new,
      predict(seas.means, data.frame(months = new.months)),
      lwd = 3,
      col = 'blue',
      lty = "solid")





# we can combine the predictions of the two models
predict(linear, data.frame(time = time.new)) +
  predict(seas.means, data.frame(months = new.months))

# and also combine the two models on the graph with the predictions
plot(window(data, start = start, end = end(data)),
     main = main,
     ylab = ylab,
     xlab = xlab,
     xlim = c(start,2016 + ahead/12),
     type = "l")
lines(time,
      fitted(linear) + fitted(seas.means),
      lwd = 3,
      col = 'red',
      lty = "dotdash")
lines(time.new,
      predict(linear, data.frame(time = time.new)) +
        predict(seas.means, data.frame(months = new.months)),
      lwd = 3,
      col = 'blue',
      lty = "solid")

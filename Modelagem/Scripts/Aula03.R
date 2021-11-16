# Libraries and datasets --------------------------------------------------
library(ggplot2)
library(cowplot)

# Analytical vs Numerical Solutions -------------------------------------------------

Solutions = data.frame()
list_lag = list()
k = .25

{
  for (j in c(.001, .01, .1, 1)) {
    if (j != .001) {
      sol = rbind(sol, Solutions)
    }
    for (i in 1:10000) {
      Solutions[i, "Time"] = i - 1
      Solutions[i, "Numerical"] = Solutions[i, "Analytical"] = 300
      if (i != 1) {
        Solutions[i, "Time"] = Solutions[i - 1, "Time"] + j
        delta = Solutions[i, "Time"] - Solutions[i - 1, "Time"]
        Solutions[i, "Numerical"] = Solutions[i - 1, "Numerical"] - k * Solutions[i -
                                                                                    1, "Numerical"] * delta
        Solutions[i, "Analytical"] = Solutions[1, "Analytical"] * exp(-k * Solutions[i, "Time"])
      }
      Solutions$lag = j
      if (j == .001) {
        sol = Solutions[1,]
      }
    }
  }
  sol = rbind(sol, Solutions)
}
#check
tapply(sol$Time, sol$lag, length) #remove first line
sol = sol[-1, ]
tapply(sol$Time, sol$lag, length) #OK!


# Graph ------------------------------------------------------------------

ggplot(sol, aes(group = lag)) +
  geom_line(aes(x = Time, y = Analytical)) +
  #geom_line(aes(x = Time, y = Numerical),
       #     linetype = "dashed",
        #    color = "red") +
  facet_grid( ~ lag, scales = "free_x")
par(mfrow = c(2, 2))
for (i in c(.001, .01, .1, 1)) {
  a = Solutions[Solutions$Lag == i, ]
  plot(
    x = a$Time,
    y = a$Analytical,
    type = "l",
    lwd = 2.5,
    ylim = c(0, 350),
    xlim = c(0, 50),
    lty = 2,
    ylab = "BOD (mg/L)",
    xlab = "Time (Day)",
    main = i,
    sub = ifelse(i == .001, "(a)",
                 ifelse(
                   i == .01, "(b)",
                   ifelse(i == .1, "(c)",
                          "(d)")
                 ))
  )
  lines(
    x = a$Time,
    y = a$Numerical,
    col = 2,
    lwd = 1
  )
  if (i == 0.001) {
    legend(
      x = 1,
      y = 400,
      cex = .75,
      legend = c("Analytical", "Numerical"),
      lwd = c(2, 1),
      col = c(1, 2),
      lty = c(2, 1),
      ncol = 2,
      bty = "n"
    )
  }
}

# Root mean Square error --------------------------------------------------
library(forecast)

for (i in c(.001, .01, .1, 1)) {
  Q  = as.data.frame(round(accuracy(sol$Numerical[sol$lag == i],
                                    sol$Analytical[sol$lag == i]), 4))
  if (i == .001) {
    Q1 = Q[1, ]
  }
  Q1 = rbind(Q1, Q)
}
Q1 = Q1[-1,]
rownames(Q1) = c(.001,.01,.1,1)
Q1


# Method 2 ----------------------------------------------------------------

for (i in c(.001, .01, .1, 1)) {
  sols1 = data.frame(
    "Time" = seq(0, 50, i),
    "Analytical" = 300 * exp(-.25 * seq(0, 50, i)),
    "Lag" = i
  )
  if (i == .001) {
    sols = sols1[1,]
  }
  sols = rbind(sols1, sols)
}
Solutions = Solutions1 = data.frame()
for (j in c(.001, .01, .1, 1)) {
  Solutions1  =  sols[sols$Lag == j, ]

  for (i in 1:length(Solutions1$Time)) {
    if (i == 1) {
      Solutions1[i, "Numerical"] = 300
    } else {
      delta = Solutions1[i, "Time"] - Solutions1[i - 1, "Time"]
      Solutions1[i, "Numerical"] = Solutions1[i - 1, "Numerical"] - Solutions1[i -
                                                                                 1, "Numerical"] * .25 * delta
    }
  }
  if (i == .001) {
    Solutions = Solutions1[1, ]
  }
  Solutions = rbind(Solutions, Solutions1)
}

Solutions[Solutions$Lag==.01,]

tapply(Solutions$Time, Solutions$Lag, length) #OK!
Solutions = Solutions[-50002,]
tapply(Solutions$Time, Solutions$Lag, length) #OK!



ggplot(Solutions, aes(group = Lag)) +
  geom_line(aes(x = Time, y = Analytical)) +
  geom_line(aes(x = Time, y = Numerical),
       linetype = "dashed",
       color = "red") +
  labs(x = "Time (Day)", y = "BOD (mg/L)")+
  facet_grid( ~ Lag, scales = "free_x")+
  theme_bw()

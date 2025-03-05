################################################################################
# Lab 7
# Caroline Devine
################################################################################

################################################################################
# Task 1: Describe The Population Distribution
################################################################################
library(tidyverse)
library(patchwork)
library(xtable)

# Goal: Plot 4 distributions, compute mean, variance, skewness, and kurtosis
# for all 4 cases
# Write a function for efficiency

beta.pop.distribution <- function(alpha, beta){
  beta.distribution <- data.frame(
    Alpha = alpha,
    Beta = beta,
    mean = alpha/(alpha+beta),
    variance = (alpha*beta)/((alpha + beta)^2 * (alpha + beta + 1)),
    skewness = (2*(beta - alpha)*sqrt(alpha + beta +1))/((alpha + beta + 2)*sqrt(alpha*beta)),
    kurtosis = ( 6 * ((alpha-beta)^2 * (alpha + beta + 1) - ((alpha*beta)*(alpha+beta+2))))/
      ((alpha*beta)*(alpha + beta + 2)*(alpha + beta +3))
  )
  return(beta.distribution)
}

table.results <- bind_rows(
  beta.pop.distribution(2,5),
  beta.pop.distribution(5,5),
  beta.pop.distribution(5,2),
  beta.pop.distribution(0.5,0.5)
)
view(table.results)

# Table with Values
table <- xtable(table.results) # for latex

# Plot the 4 Distributions

# Beta(2,5)
figure.data.1 <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
    mutate(beta.pdf = dbeta(x, 2, 5))
  
plot1 <- ggplot(data = figure.data.1)+
          geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +      
          geom_hline(yintercept=0)+                                            # plot x axis
          theme_bw()+                                                          # change theme
          xlab("x")+                                                           # label x axis
          ylab("Density")+                                                     # label y axis
          scale_color_manual("", values = c("black", "grey"))+                 # change colors
          theme(legend.position = "bottom") 

# Beta(5,5)
figure.data.2 <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
  mutate(beta.pdf = dbeta(x, 5, 5))

plot2 <- ggplot(data = figure.data.2)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +      
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") 

# Beta(5,2)
figure.data.3 <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
  mutate(beta.pdf = dbeta(x, 5, 2))

plot3 <- ggplot(data = figure.data.3)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +      
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") 

# Beta(0.5,0.5)
figure.data.4 <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>
  mutate(beta.pdf = dbeta(x, 0.5, 0.5))

plot4 <- ggplot(data = figure.data.4)+
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.5,0.5)")) +      
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") 

# Combined Plot of 4 Distributions
four.case.plot <- plot1+plot2+plot3+plot4
four.case.plot

################################################################################
# Task 2: Compute the Moments
################################################################################

#
beta.moment <- function(alpha, beta, k, centered){

  if (centered == T){
    mean <- alpha/(alpha+beta)
    centered.moment <- integrate(function(x) ((x-mean)^k)*dbeta(x, alpha, beta), 0, 1)$value
    return(centered.moment)
  }else{
    uncentered.moment <- integrate(function(x) ((x)^k)*dbeta(x, alpha, beta), 0, 1)$value
    return(uncentered.moment)
  }
}

# Moments Tested
beta.moment1 <- tibble(
    Alpha = 2,
    Beta = 5,
    mean = beta.moment(2,5,1,F),
    variance = beta.moment(2,5,2,T),
    skewness = (beta.moment(2,5,3,T) / 
              ((beta.moment(2,5,2,T))^(3/2))),
    excess.kurtosis = (beta.moment(2,5,4,T)/(beta.moment(2,5,2,T))^2)-3,
  )

beta.moment2 <- tibble(
  Alpha = 5,
  Beta = 5,
  mean = beta.moment(5,5,1,F),
  variance = beta.moment(5,5,2,T),
  skewness = (beta.moment(5,5,3,T) / 
                ((beta.moment(5,5,2,T))^(3/2))),
  excess.kurtosis = (beta.moment(5,5,4,T)/
                       (beta.moment(5,5,2,T))^2)-3,
)

beta.moment3 <- tibble(
  Alpha = 5,
  Beta = 2,
  mean = beta.moment(5,2,1,F),
  variance = beta.moment(5,2,2,T),
  skewness = (beta.moment(5,2,3,T) / 
                ((beta.moment(5,2,2,T))^(3/2))),
  excess.kurtosis = (beta.moment(5,2,4,T)/
                       (beta.moment(5,2,2,T))^2)-3,
)

beta.moment4 <- tibble(
  Alpha = 0.5,
  Beta = 0.5,
  mean = beta.moment(0.5,0.5,1,F),
  variance = beta.moment(0.5,0.5,2,T),
  skewness = (beta.moment(0.5,0.5,3,T) / 
                ((beta.moment(0.5,0.5,2,T))^(3/2))),
  excess.kurtosis = (beta.moment(0.5,0.5,4,T)/
                       (beta.moment(0.5,0.5,2,T))^2)-3,
)

confirmed.results <- bind_rows(
  beta.moment1,
  beta.moment2,
  beta.moment3,
  beta.moment4
)
view(confirmed.results)
# They match, the skewness gives us a value instead of 0, but it is essentially 0

################################################################################
# Task 3: Do Data Summaries Help?
################################################################################
library(e1071)

# Beta(2,5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample.1 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
fig1.data <-  tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta))

beta.sample.data.1 <- tibble(data = beta.sample.1)

sample.summary.1 <- beta.sample.data.1 |>
  summarize(
    Alpha = alpha,
    Beta = beta,
    mean = mean(data),
    variance = var(data),
    skewness = e1071::skewness(data),
    excess.kurtosis =  e1071::kurtosis(data)
  )

hist.1 <- ggplot()+
  geom_histogram(data = beta.sample.data.1,
                 aes(x = beta.sample.1, y = after_stat(density)),
                 breaks = seq(-0.5,1.2,0.1),
                 fill = "grey30",
                 color = "lightgray"
  ) + 
  geom_density(data = beta.sample.data.1,
               aes(x = beta.sample.1, color = "Sample Density"), show.legend = T)+
  geom_line(data = fig1.data,
            aes(x = x, y = beta.pdf, color = "Beta Distribution"), show.legend = T)+
  geom_hline(yintercept = 0) + 
  labs(color = "Line", title = "Beta(2,5)")
hist.1

# Beta(5,5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 5
beta.sample.2 <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
fig2.data <-  tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta))

beta.sample.data.2 <- tibble(data = beta.sample.2)

sample.summary.2 <- beta.sample.data.2 |>
  summarize(
    Alpha = alpha,
    Beta = beta,
    mean = mean(data),
    variance = var(data),
    skewness = e1071::skewness(data),
    excess.kurtosis =  e1071::kurtosis(data)
  )

hist.2 <- ggplot()+
  geom_histogram(data = beta.sample.data.2,
                 aes(x = beta.sample.2, y = after_stat(density)),
                 breaks = seq(-0.5,1.2,0.1),
                 fill = "grey30",
                 color = "lightgray"
  ) + 
  geom_density(data = beta.sample.data.2,
               aes(x = beta.sample.2, color = "Sample Density"), show.legend = T)+
  geom_line(data = fig2.data,
            aes(x = x, y = beta.pdf, color = "Beta Distribution"), show.legend = T)+
  geom_hline(yintercept = 0) + 
  labs(color = "Line", title = "Beta(5,5)")
hist.2

# Beta(5,2)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 5
beta <- 2
beta.sample.3 <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
fig3.data <-  tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta))

beta.sample.data.3 <- tibble(data = beta.sample.3)

sample.summary.3 <- beta.sample.data.3 |>
  summarize(
    Alpha = alpha,
    Beta = beta,
    mean = mean(data),
    variance = var(data),
    skewness = e1071::skewness(data),
    excess.kurtosis =  e1071::kurtosis(data)
  )

hist.3 <- ggplot()+
  geom_histogram(data = beta.sample.data.3,
                 aes(x = beta.sample.3, y = after_stat(density)),
                 breaks = seq(-0.5,1.2,0.1),
                 fill = "grey30",
                 color = "lightgray"
  ) + 
  geom_density(data = beta.sample.data.3,
               aes(x = beta.sample.3, color = "Sample Density"), show.legend = T)+
  geom_line(data = fig3.data,
            aes(x = x, y = beta.pdf, color = "Beta Distribution"), show.legend = T)+
  geom_hline(yintercept = 0) + 
  labs(color = "Line", title = "Beta(5,2)")
hist.3

# Beta(0.5,0.5)
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 0.5
beta <- 0.5
beta.sample.4 <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
fig4.data <-  tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta))

beta.sample.data.4 <- tibble(data = beta.sample.4)

sample.summary.4 <- beta.sample.data.4 |>
  summarize(
    Alpha = alpha,
    Beta = beta,
    mean = mean(data),
    variance = var(data),
    skewness = e1071::skewness(data),
    excess.kurtosis =  e1071::kurtosis(data)
  )

hist.4 <- ggplot()+
  geom_histogram(data = beta.sample.data.4,
                 aes(x = beta.sample.4, y = after_stat(density)),
                 breaks = seq(-0.5,1.2,0.1),
                 fill = "grey30",
                 color = "lightgray"
  ) + 
  geom_density(data = beta.sample.data.4,
               aes(x = beta.sample.4, color = "Sample Density"), show.legend = T)+
  geom_line(data = fig4.data,
            aes(x = x, y = beta.pdf, color = "Beta Distribution"), show.legend = T)+
  geom_hline(yintercept = 0) + 
  labs(color = "Line", title = "Beta(0.5,0.5)")
hist.4


# Combined Plot
sample.combined <- hist.1 + hist.2 + hist.3 + hist.4+ plot_layout(guides = "collect")
sample.combined

# Combined Summaries
sample.summary.results <- bind_rows(
  sample.summary.1,
  sample.summary.2,
  sample.summary.3,
  sample.summary.4
)
view(sample.summary.results)

################################################################################
# Task 4: Is Sample Size Important?
################################################################################
library(cumstats)
#help("cumstats")

set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample.1 <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha,   # alpha parameter
                       shape2 = beta)    # beta parameter
# fig1.data <-  tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
#   mutate(beta.pdf = dbeta(x, alpha, beta))
true.mean <- alpha/(alpha+beta)
true.variance <- (alpha*beta)/((alpha + beta)^2 * (alpha + beta + 1))
true.skewness <- (2*(beta - alpha)*sqrt(alpha + beta +1))/((alpha + beta + 2)*sqrt(alpha*beta))
true.kurtosis <- ( 6 * ((alpha-beta)^2 * (alpha + beta + 1) - ((alpha*beta)*(alpha+beta+2))))/
  ((alpha*beta)*(alpha + beta + 2)*(alpha + beta +3))

cumulative.sum <- data.frame(
    Alpha = alpha,
    Beta = beta,
    mean =cummean(beta.sample.1),
    variance = cumvar(beta.sample.1),
    skewness = cumskew(beta.sample.1),
    kurtosis = cumkurt(beta.sample.1)-3 # regular kurtosis
  )
cumulative.sum <- cumulative.sum|>
  mutate(observation = 1:n())


cs.mean1 <- ggplot(data = cumulative.sum)+
        geom_line(aes(x=observation, y = mean, color = "Cumulative Mean"), show.legend = F) + 
        geom_hline(yintercept = true.mean)+
        labs(title = "Cumulative Mean Across Sample Sizes",
             x = "Sample Size",
             y = "Mean",
             color = "")
  
cs.var1 <- ggplot(data = cumulative.sum)+
  geom_line(aes(x=observation, y = variance, color = "Cumulative Variance"), show.legend = F) + 
  geom_hline(yintercept = true.variance)+
  labs(title = "Cumulative Variance Across Sample Sizes",
       x = "Sample Size",
       y = "Variance",
       color = "")

cs.skew1 <- ggplot(data = cumulative.sum)+
  geom_line(aes(x=observation, y = skewness, color = "Cumulative Skewness"), show.legend = F) + 
  geom_hline(yintercept = true.skewness)+
  labs(title = "Cumulative Skewness Across Sample Sizes",
       x = "Sample Size",
       y = "Skewness",
       color = "")

cs.kurt1 <- ggplot(data = cumulative.sum)+
  geom_line(aes(x=observation, y = kurtosis, color = "Cumulative Regular Kurtosis"), show.legend = F) + 
  geom_hline(yintercept = true.kurtosis)+
  labs(title = "Cumulative Kurtosis Across Sample Sizes",
       x = "Sample Size",
       y = "Kurtosis",
       color = "")
        
cs.1 <- cs.mean1 + cs.var1 + cs.skew1 + cs.kurt1
  

# New Data 

for (i in (2:50)){
  set.seed(7272+i) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  alpha <- 2
  beta <- 5
  beta.sample.1 <- rbeta(n = sample.size,  # sample size
                         shape1 = alpha,   # alpha parameter
                         shape2 = beta)    # beta parameter

  
  cumulative.sum <- data.frame(
    Alpha = alpha,
    Beta = beta,
    mean =cummean(beta.sample.1),
    variance = cumvar(beta.sample.1),
    skewness = cumskew(beta.sample.1),
    kurtosis = cumkurt(beta.sample.1)-3 # regular kurtosis
  )
  cumulative.sum <- cumulative.sum|>
    mutate(observation = 1:n())
  
  cs.mean1 <- cs.mean1 +
    geom_line(data = cumulative.sum, aes(x = observation, y = mean), color = i)
  
  cs.var1 <- cs.var1 +
    geom_line(data = cumulative.sum, aes(x = observation, y = variance), color = i)
  
  cs.skew1 <- cs.skew1 +
    geom_line(data = cumulative.sum, aes(x = observation, y = skewness), color = i)
  
  cs.kurt1 <- cs.kurt1 +
    geom_line(data = cumulative.sum, aes(x = observation, y = kurtosis), color = i)
  
  cs.1 <- cs.mean1 + cs.var1 + cs.skew1 + cs.kurt1
  
}
cs.1

################################################################################
# Task 5: How can we model the variable?
################################################################################
new.results <- data.frame()

for (i in 1:1000){
  set.seed(7272+i) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  alpha <- 2
  beta <- 5
  beta.sample.1 <- rbeta(n = sample.size,  # sample size
                         shape1 = alpha,   # alpha parameter
                         shape2 = beta)    # beta parameter
  
  results <- data.frame(
    Alpha = alpha,
    Beta = beta,
    mean =mean(beta.sample.1),
    variance = var(beta.sample.1),
    skewness = e1071::skewness(beta.sample.1),
    kurtosis = e1071::kurtosis(beta.sample.1)-3 
  )
  
  new.results <- bind_rows(new.results, results)
  
}
view(new.results)


# Plot Histograms for Each Statistic with Estimated Density
# Mean Plot
mean.plot <- ggplot() +
  geom_histogram(data = new.results, aes(x = mean, y = after_stat(density)), 
                 bins = 40,
                 fill = "grey30",
                 color = "lightgray")+
  geom_density(data = new.results,
               aes(x = mean,
                   color = "Density"))+
  labs(title = "Histogram of Means", 
       x = "Mean", 
       y = "Density",
       color = "Lines")

# Variance Plot
var.plot <- ggplot() +
  geom_histogram(data = new.results, aes(x = variance, y = after_stat(density)), 
                 bins = 40,
                 fill = "grey30",
                 color = "lightgray")+
  geom_density(data = new.results,
               aes(x = variance,
                   color = "Density"))+
  labs(title = "Histogram of Variance", 
       x = "Variance", 
       y = "Density",
       color = "Lines")

# Skewness Plot
skew.plot <- ggplot() +
  geom_histogram(data = new.results, aes(x = skewness, y = after_stat(density)), 
                 bins = 40,
                 fill = "grey30",
                 color = "lightgray")+
  geom_density(data = new.results,
               aes(x = skewness,
                   color = "Density"))+
  labs(title = "Histogram of Skewness", 
       x = "Skewness", 
       y = "Density",
       color = "Lines")

# Excess Kurtosis Plot
kurt.plot <- ggplot() +
  geom_histogram(data = new.results, aes(x = kurtosis, y = after_stat(density)), 
                 bins = 40,
                 fill = "grey30",
                 color = "lightgray")+
  geom_density(data = new.results,
               aes(x = kurtosis,
                   color = "Density"))+
  labs(title = "Histogram of Excess Kurtosis", 
       x = "Excess Kurtosis", 
       y = "Density",
       color = "Lines")

new.data.histogram <- mean.plot + var.plot + skew.plot + kurt.plot + plot_layout(guides = "collect")
new.data.histogram
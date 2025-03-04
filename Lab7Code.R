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

beta.moment1 <- tibble(
    Alpha = 2,
    Beta = 5,
    mean = beta.moment(2,5,1,F),
    variance = beta.moment(2,5,2,T),
    skewness = (beta.moment(2,5,3,T)/((beta.moment(2,5,2,T))^(2/3))),
    excess.kurtosis = (beta.moment(2,5,4,T)/(beta.moment(2,5,2,T))^2)-3,
  )
view(beta.moment1)

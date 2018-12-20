# Daniel Eubanks
# December 2018
# WorksOfR.wordpress.com

library(data.table)
library(ggplot2)

########## BIRTHDAY PROBLEM, UNIFORMLY DISTRIBUTED

n <- 1000

# Run simulation for 1 through 65 people
n_ppl <- 1:65
prob_same_bday <- rep(NA,length(n_ppl))
for(ppl in n_ppl) {
    # Generate a matrix of birthdays: one row for each of the n simulations, one column for each person
    birthdays <- matrix(sample(1:365,ppl*n,replace=TRUE),nrow=n)
    # For each row in the matrix, determine if there are shared birthdays
    # The probability that at least two people share a birthday is the mean of the boolean vector
    prob_same_bday[ppl] <- mean(apply(birthdays,1,function(row) length(unique(row)) < ppl))
}

# Plot the results: probability of a shared birthday with confidence intervals
#   for different numbers of people
p_df <- data.frame(n_ppl,
                   p = prob_same_bday,
                   sd = sqrt(prob_same_bday*(1-prob_same_bday)),
                   n = n)
p_df$se <- p_df$sd/sqrt(p_df$n)
p_df$lo <- p_df$p - qt(0.975,p_df$n-1)*p_df$se
p_df$hi <- p_df$p + qt(0.975,p_df$n-1)*p_df$se

# The true probability is 1 minus the probability that no birthdays are shared
# The probability that one pair of people do not share a birthday is 364/365
# The probability that no pair have the same birthday is the probability that
#   one pair of people have different birthdays raised to the number of pairs of people
true_p <- function(ppl) {
    1 - (364/365)^choose(ppl,2)
}

ggplot(p_df,aes(n_ppl,p)) +
    geom_line(aes(color="Simulated Value")) +
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.25) +
    geom_hline(yintercept=0.5) +
    stat_function(fun=true_p,aes(color="True Probability")) +
    scale_color_manual(name=NULL,
                       values=c("black","red")) +
    labs(title="Birthday Problem",
         subtitle="Probability that any two people share a birthday") +
    xlab("Number of People") +
    ylab("Probability") +
    theme_bw()

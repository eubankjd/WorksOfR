# Daniel Eubanks
# December 2018
# WorksOfR.wordpress.com

library(data.table)
library(ggplot2)

########## BIRTHDAY PROBLEM, DATA

# Read data
bday_table <- fread("https://raw.githubusercontent.com/fivethirtyeight/data/master/births/US_births_2000-2014_SSA.csv")
bday_table[, month_date := paste(sprintf("%02d",month),sprintf("%02d",date_of_month))]
bday_table <- bday_table[, .(births = sum(births)), by=month_date]
bday_table[, day_num := 1:.N]
bday_table[, prob := births/sum(births)]

# Plot distribution
ggplot(bday_table,aes(day_num,prob)) +
    geom_hline(yintercept=1/365) +
    geom_area(fill="blue",alpha=0.25) +
    ggtitle("Empirical Distribution of Birthdays") +
    labs(caption="Source: https://raw.githubusercontent.com/fivethirtyeight/data/master/births") +
    xlab("Day of Year") +
    ylab("Probability") +
    theme_bw()

# Simulation

n <- 3000

# Run simulation for 1 through 65 people
n_ppl <- 1:65
prob_same_bday <- rep(NA,length(n_ppl))
for(ppl in n_ppl) {
    # Generate a matrix of birthdays: one row for each of the n simulations, one column for each person
    birthdays <- matrix(sample(1:366,ppl*n,prob=bday_table$prob,replace=TRUE),nrow=n)
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

ggplot(p_df,aes(n_ppl,p)) +
    geom_line(color="black") +
    geom_ribbon(aes(ymin=lo,ymax=hi),alpha=0.25) +
    geom_hline(yintercept=0.5) +
    labs(title="Birthday Problem with Data",
         subtitle="Probability that any two people share a birthday") +
    xlab("Number of People") +
    ylab("Probability") +
    theme_bw()

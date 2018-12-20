# Daniel Eubanks
# December 2018
# WorksOfR.wordpress.com

library(ggplot2)

########## MONTY HALL SIMULATION

n <- 10000
doors <- 3

# Generate n outcomes: randomly pick prize door, randomly make first pick
prize_door <- sample(1:doors,n,replace=TRUE)
first_pick <- sample(1:doors,n,replace=TRUE)

# Without switching, it's a win if prize door is the first pick
wins_noswitch <- prize_door ==  first_pick
# With switching, it's a win if prize door is not first pick
wins_switch <- prize_door != first_pick

# Probabilities
p_noswitch <- mean(wins_noswitch)
p_switch <- mean(wins_switch)

# Plot the results: probability of winning by srategy with confidence intervals
p_df <- data.frame(strategy = c("Switch","Don't Switch"),
                   p_win = c(p_switch,p_noswitch),
                   sd = c(sqrt(p_switch*(1-p_switch)),sqrt(p_noswitch*(1-p_noswitch))),
                   n = n)
p_df$se <- p_df$sd/sqrt(p_df$n)
p_df$lo <- p_df$p_win - qt(0.975,p_df$n-1)*p_df$se
p_df$hi <- p_df$p_win + qt(0.975,p_df$n-1)*p_df$se

ggplot(p_df,aes(strategy,p_win)) +
    geom_bar(stat="identity",fill="#4472c0",width=0.5) +
    geom_errorbar(aes(ymin=lo,ymax=hi),width=0.15) +
    geom_hline(yintercept=0) +
    labs(title="Monty Hall Problem",
         subtitle=paste("Probability of winning by strategy given",doors,"doors")) +
    xlab(NULL) +
    ylab("Probability") +
    theme_bw()



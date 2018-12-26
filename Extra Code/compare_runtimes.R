library(ggplot2)
library(data.table)
setwd("~/R Scripts/WorksOfR/")

N = 10
puzzle_id = "evil"

enhanced_times = rep(NA,N)
simple_times = rep(NA,N)
simple_times = 40*60
for(t in 1:N) {
    source('sudoku_enhanced.R')
    enhanced_times[t] = end - start 
    
    source('simple_sudoku.R')
    simple_times[t] = end-start
}

df = data.table(enhanced_times,
                simple_times,
                puzzle=puzzle_id)
df = melt(df,id.vars="puzzle", variable.name = "Method")
df = df[,.(mean=mean(value),
           sd=sd(value),
           N=.N), by=list(puzzle,Method)]

saveRDS(df,paste0("puzzle_results_",puzzle_id,".RData"))

##########
df1 = readRDS("puzzle_results_easy.RData")
df2 = readRDS("puzzle_results_medium.RData")
df3 = readRDS("puzzle_results_hard.RData")
df4 = readRDS("puzzle_results_evil.RData")
df5 = readRDS("puzzle_results_worst.RData")

df = rbind(df1,df2,df3,df4,df5)


ggplot(df,aes(puzzle,mean,fill=Method)) +
    geom_bar(stat="identity",position=position_dodge(.9)) +
    geom_errorbar(aes(puzzle,ymin=mean-qt(0.975,N-1)*sd/sqrt(N),ymax=mean+qt(0.975,N-1)*sd/sqrt(N)),width=0.25,position=position_dodge(0.9)) +
    scale_x_discrete(limits=c("easy","medium","hard","evil","worst"),
                     labels=c("Easy","Medium","Hard","Evil","Worst Case")) +
    scale_fill_brewer(palette="Set2",name=NULL,
                      labels=c("Improved Version","Simple Version")) +
    coord_cartesian(ylim=c(0,10), expand=FALSE) +
    xlab(NULL) +
    ylab("Seconds") +
    theme_bw()

ggsave("Graphics/sudoku_run_times.png")



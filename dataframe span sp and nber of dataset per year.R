library(ggplot2)
library(ggalt)
library(dplyr)
library(export)

load("ram4.41_for_analysis_NP.Rdata")


df_sp_length <- data.frame(matrix(NA, nrow = length(sp_stock), ncol= 3))
colnames(df_sp_length) <- c("Sp","start","end")
df_sp_length$Sp <- names(sp_stock)

for(species in 1:length(df_sp_length$Sp)){
  df_sp_length$start[species] <- min(sp_stock[[species]]$start_y)
  df_sp_length$end[species] <- max(sp_stock[[species]]$end_y)
}
rm(species)
df_sp_length <- df_sp_length[order(df_sp_length$start),]
df_sp_length$Sp <- factor(df_sp_length$Sp, levels = df_sp_length$Sp)

gg_sp <- ggplot(df_sp_length, aes(x=start, xend=end, 
                                  y=df_sp_length$Sp)) + 
  geom_dumbbell(size=0.7) + 
  # scale_x_continuous(label=start) +
  labs(x=NULL, 
       y=NULL, 
       title="Dumbbell Chart") +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        # panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(colour = "grey",size=0.5),
        # panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())+
  geom_vline(aes(xintercept = 1950), color='red')+
  geom_vline(aes(xintercept =1970), color='red')+
  geom_vline(aes(xintercept =2015), color='red')
  # geom_line(aes(y = xxx), inherit.aes=TRUE) 
  plot(gg_sp)
  graph2ppt(file="Dumbell.pptx", width=dev.size()[1], height=dev.size()[2])

#nber of dataset per year
span=min(df_sp_length$start):max(df_sp_length$end)
xxx <- data.frame('number'=NA,'span'=span)

for(i in 1:dim(xxx)[1]){
  xxx[i,1] <- nrow(df_sp_length[(df_sp_length$start<=span[i] & df_sp_length$end >=span[i]),])
}
plot(xxx~span, ylab='nber of dataset', xlab='year')

ggplot(xxx,aes(span, number))+
  geom_line()+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        # panel.grid.minor=element_blank(),
        panel.grid.major.y=element_line(colour = "grey",size=0.5),
        # panel.grid.major.x=element_blank(),
        axis.ticks=element_blank(),
        legend.position="top",
        panel.border=element_blank())+
  geom_vline(aes(xintercept = 1950), color='red')+
  geom_vline(aes(xintercept =1970), color='red')+
  geom_vline(aes(xintercept =2015), color='red')+
  scale_y_continuous(position = "right")
graph2ppt(file="Dumbell.pptx", width=dev.size()[1], height=dev.size()[2], append=TRUE)


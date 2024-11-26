
library(ggplot2)

outputdir <-file.path(getwd(),"Output")

results1 <- read.csv(file.path(outputdir, "WA_2021_imputed_07_2024-09-20.csv"))

# boxplot(Prob_Crash ~ Hour, data = results1, main = 'Boxplot of Crash Probability (for Each Hour of the Day)', ylab = 'Crash Probability', col = 'green')

save_charts <- function(results_df, # the dataframe object with the results
                        name_of_results # some name that will help distinguish from other results - will be used in filename for outputs
                        ){
  
  unfaceted_plot = ggplot(data=results_df, mapping=aes(x=Hour, y=Prob_Crash, group = Hour)) + 
    geom_boxplot(fill = "green") + 
    labs(title = "Boxplot of Crash Probability by Hour",
         y = "Crash Probability",
         x = "Hour")
  
  ggsave(plot = unfaceted_plot, 
         filename = paste0("unfaceted_boxplot", name_of_results, ".png"),
         path = outputdir,
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
  faceted_plot = ggplot(data=results_df, mapping=aes(x=Hour, y=Prob_Crash, group = Hour)) + 
    geom_boxplot(fill = "green") + 
    facet_wrap(~DayOfWeek) + 
    labs(title = "Boxplot of Crash Probability by Hour (Faceted by Day)",
         y = "Crash Probability",
         x = "Hour")
  
  ggsave(plot = faceted_plot, 
         filename = paste0("faceted_boxplot", name_of_results, ".png"),
         path = outputdir,
         device = "png",
         create.dir = T,
         height = 6, width = 5, units = "in")
  
}

save_charts(results_df = results1, name_of_results = "v1_50-50downsample")


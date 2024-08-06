library("ggplot2")
#---

results_aware <- readRDS("intermediate/results_aware.Rda")
results_fairadd <- readRDS("intermediate/results_fairadd.Rda")
results_fair <- readRDS("intermediate/results_fair.Rda")
results_ftu <- readRDS("intermediate/results_ftu.Rda")


generate_barplot <- function(results_list) {
  
  # Filter the results to only include "true" results
  true_results <- results_list[grepl("^res_true", names(results_list))]
  
  # Create a data frame from the filtered results
  df <- data.frame(
    Dataset = rep(c("no_confounding", "confounding_s"), each = 2),
    Learner = rep(c("lg", "rf"), times = 2),
    EBM = sapply(true_results, function(x) x[[2]])
  )
  df$Dataset <- factor(df$Dataset, levels = c("no_confounding", "confounding_s"), 
                       labels = c("DATA1", "DATA2"))
  
  
  # Generate the bar plot
  ggplot(df, aes(x = Dataset, y = EBM, fill = Learner)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(x = "Data",
         y = "MBE",
         fill = "Learner") +
    scale_fill_manual(values = c("lg" = "#00B2A9", "rf" = "#9B59B6"), 
                      labels = c("lg" = "Logistic Regression", "rf" = "Random Forest")) +
    theme_bw() +
    ylim(0,0.4) +
    theme(
      # Title and axis labels
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      
      # Axis text
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      
      # Legend
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.position = "bottom",
      legend.key = element_rect(size = 2), 
      legend.key.size = unit(0.4, "cm")
    )
}

# Example usage:
learner_aware <- generate_barplot(results_aware)
print(learner_aware)
pdf("plots/learner_aware.pdf", width=4, height=3)
learner_aware
dev.off()

learner_ftu <- generate_barplot(results_ftu)
print(learner_ftu)
pdf("plots/learner_ftu.pdf", width=4, height=3)
learner_ftu
dev.off()

learner_fairadd <- generate_barplot(results_fairadd)
print(learner_fairadd)
pdf("plots/learner_fairadd.pdf", width=4, height=3)
learner_fairadd
dev.off()

learner_fair <- generate_barplot(results_fair)
print(learner_fair)
pdf("plots/learner_fair.pdf", width=4, height=3)
learner_fair
dev.off()


### This code creates the results from chapter 5.2.1 for the comparison of the learners ### 
library("ggplot2")

# Read lists of results
results_aware <- readRDS("intermediate/results_aware.Rda")
results_fairadd <- readRDS("intermediate/results_fairadd.Rda")
results_fairdata <- readRDS("intermediate/results_fairdata.Rda")
results_ftu <- readRDS("intermediate/results_ftu.Rda")

# Function to make barplots for MBE values over the true counterfactuals
generate_barplot <- function(results_list) {
  true_results <- results_list[grepl("^res_true", names(results_list))]
  df <- data.frame(
    Dataset = rep(c("no_confounding", "confounding_s"), each = 2),
    Learner = rep(c("lg", "rf"), times = 2),
    EBM = sapply(true_results, function(x) x[[2]])
  )
  df$Dataset <- factor(df$Dataset, levels = c("no_confounding", "confounding_s"), 
                       labels = c("DATA1", "DATA2"))
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
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      legend.position = "bottom",
      legend.key = element_rect(size = 2), 
      legend.key.size = unit(0.4, "cm")
    )
}

# Generate and save plots
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

learner_fairdata <- generate_barplot(results_fairdata)
print(learner_fairdata)
pdf("plots/learner_fairdata.pdf", width=4, height=3)
learner_fairdata
dev.off()


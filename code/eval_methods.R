library("ggplot2")

results_aware <- readRDS("intermediate/results_aware.Rda")
results_ftu <- readRDS("intermediate/results_ftu.Rda")
results_fairadd <- readRDS("intermediate/results_fairadd.Rda")
results_fair <- readRDS("intermediate/results_fair.Rda")


# rf
# Extrahieren der relevanten Daten
extract_relevant_data <- function(results, method) {
  true_results_rf <- results[grepl("^res_true", names(results)) & grepl("rf$", names(results))]
  data.frame(
    Dataset = sub("res_true_", "", sub("_lg$", "", names(true_results_rf))),
    Method = method,
    EBM = sapply(true_results_rf, function(x) x[[2]])
  )
}

# Erstellen des Dataframes für jeden Datensatz und Methode
df_aware <- extract_relevant_data(results_aware, "Aware")
df_ftu <- extract_relevant_data(results_ftu, "Unaware")
df_fairadd <- extract_relevant_data(results_fairadd, "Fair Add")
df_fair <- extract_relevant_data(results_fair, "Fair Data")


# Zusammenfügen aller Dataframes
df_all <- rbind(df_aware, df_ftu, df_fairadd, df_fair)
df_all$Method <- factor(df_all$Method, levels = c("Aware", "Unaware", "Fair Add", "Fair Data"))
df_all$Dataset <- factor(df_all$Dataset, levels = c("no_confounding_rf", "confounding_s_rf"),
                         labels = c("DATA1", "DATA2"))

palette <- c("#4A90E2", "#F5A623", "#7ED321", "#9B9B9B")

# Erzeugen des Barplots
methods_rf <- ggplot(df_all, aes(x = Dataset, y = EBM, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
       x = "Data",
       y = "MBE",
       fill = "Method") +
  theme_bw() +
  ylim(0,0.4) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  scale_fill_manual(values = palette) + 
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
    legend.key = element_rect(size =2), 
    legend.key.size = unit(0.4, "cm")
  )

# lg
# rf
# Extrahieren der relevanten Daten
extract_relevant_data <- function(results, method) {
  true_results_lg <- results[grepl("^res_true", names(results)) & grepl("lg$", names(results))]
  data.frame(
    Dataset = sub("res_true_", "", sub("_rf$", "", names(true_results_lg))),
    Method = method,
    EBM = sapply(true_results_lg, function(x) x[[2]])
  )
}

# Erstellen des Dataframes für jeden Datensatz und Methode
df_aware <- extract_relevant_data(results_aware, "Aware")
df_ftu <- extract_relevant_data(results_ftu, "Unaware")
df_fairadd <- extract_relevant_data(results_fairadd, "Fair Add")
df_fair <- extract_relevant_data(results_fair, "Fair Data")


# Zusammenfügen aller Dataframes
df_all <- rbind(df_aware, df_ftu, df_fairadd, df_fair)
df_all$Method <- factor(df_all$Method, levels = c("Aware", "Unaware", "Fair Add", "Fair Data"))
df_all$Dataset <- factor(df_all$Dataset, levels = c("no_confounding_lg", "confounding_s_lg"),
                         labels = c("DATA1", "DATA2"))



palette <- c("#4A90E2", "#F5A623", "#7ED321", "#9B9B9B")
# Erzeugen des Barplots
methods_lg <- ggplot(df_all, aes(x = Dataset, y = EBM, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Data",
    y = "MBE",
    fill = "Method") +
  theme_bw() +
  ylim(0,0.4) +
  guides(color = guide_legend(
    override.aes = list(size = 3)  # Größe der Punkte in der Legende anpassen
  )) +
  scale_fill_manual(values = palette) + 
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


pdf("plots/methods_lg.pdf", width=4, height=3)
print(methods_lg)
dev.off()

pdf("plots/methods_rf.pdf",  width=4, height=3)
print(methods_rf)
dev.off()



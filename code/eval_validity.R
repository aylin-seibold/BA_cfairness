library("xtable")
library("ggplot2")

###------------------------------###
# Tables for differences
###------------------------------###

results_aware <- readRDS("intermediate/results_aware.Rda")
results_ftu <- readRDS("intermediate/results_ftu.Rda")
results_fairadd <- readRDS("intermediate/results_fairadd.Rda")
results_fair <- readRDS("intermediate/results_fair.Rda")



create_difference_table <- function(results_gen, caption = "Differences Between Generated and True Results") {
  
  # Berechne die Differenzen
  difference_list <- list(
    no_confounding_lg = abs(results_gen$res_gen_no_confounding_lg[[2]] - results_gen$res_true_no_confounding_lg[[2]]),
    no_confounding_rf = abs(results_gen$res_gen_no_confounding_rf[[2]] - results_gen$res_true_no_confounding_rf[[2]]),
    confounding_s_lg = abs(results_gen$res_gen_confounding_s_lg[[2]] - results_gen$res_true_confounding_s_lg[[2]]),
    confounding_s_rf = abs(results_gen$res_gen_confounding_s_rf[[2]] - results_gen$res_true_confounding_s_rf[[2]])
  )
  
  # Umstrukturierung der Daten in ein Data Frame
  difference_df <- do.call(rbind, lapply(c("lg", "rf"), function(learner) {
    data.frame(
      Dataset = c("no_confounding", "confounding_s"),
      Difference = c(
        difference_list[[paste0("no_confounding_", learner)]],
        difference_list[[paste0("confounding_s_", learner)]]
      ),
      Learner = learner
    )
  }))
  
  # Pivotieren, um die Data Frame-Spalten für Datensätze zu erstellen
  difference_df_wide <- reshape(difference_df, 
                                timevar = "Dataset", 
                                idvar = "Learner", 
                                direction = "wide")
  
  # Formatieren der Data Frame-Spaltennamen
  names(difference_df_wide) <- gsub("Difference\\.", "", names(difference_df_wide))
  
  # Erstelle die LaTeX-Tabelle
  xtable_obj <- xtable(difference_df_wide, caption = caption, digits = 3)
  
  # Drucke die xtable
  print(xtable_obj, 
        include.rownames = FALSE, 
        caption.placement = "top", 
        hline.after = c(-1, 0, nrow(difference_df_wide)))
}

# Beispielaufrufe
table_aware <- create_difference_table(results_aware, "Differences Between Generated and True Results (Fair)")
table_unaware <- create_difference_table(results_ftu, "Differences Between Generated and True Results (FTU)")
table_fairadd <-  create_difference_table(results_fairadd, "Differences Between Generated and True Results (Fair Add)")
table_fair <- create_difference_table(results_fair, "Differences Between Generated and True Results (Fair)")









# AMPD FTU
true_data_lg <- results_ftu$res_true_confounding_s_lg[[1]]
gen_data_lg <- results_ftu$res_gen_confounding_s_lg[[1]]


# Erstellen des Data Frames für die Violinplots
df_lg <- data.frame(
  Dataset = rep("No Confounding LG", length(true_data_lg)),
  Value = c(true_data_lg, gen_data_lg),
  Type = rep(c("True", "Generated"), each = length(true_data_lg))
)

# Erstellen der Violinplots
ampd_ftu <- ggplot(df_lg, aes(x = Type, y = Value)) +
  geom_boxplot() +
  labs(
       x = "Counterfactual",
       y = "AMPD") +
  theme_bw() +
 ylim(0,1)+
  theme(
    # Title and axis labels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    
    # Axis text
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))




# AMPD FTU
true_data_lg <- results_fair$res_true_confounding_s_lg[[1]]
gen_data_lg <- results_fair$res_gen_confounding_s_lg[[1]]


# Erstellen des Data Frames für die Violinplots
df_lg <- data.frame(
  Dataset = rep("No Confounding LG", length(true_data_lg)),
  Value = c(true_data_lg, gen_data_lg),
  Type = rep(c("True", "Generated"), each = length(true_data_lg))
)

# Erstellen der Violinplots
ampd_fairdata <- ggplot(df_lg, aes(x = Type, y = Value)) +
  geom_boxplot() +
  labs(
    x = "Counterfactual",
    y = "AMPD") +
  theme_bw() +
  ylim(0,1)+
  theme(
    # Title and axis labels
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    
    # Axis text
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))

pdf("plots/ampd_fairdata.pdf", width=4, height=3)
print(ampd_fairdata)
dev.off()

pdf("plots/ampd_ftu.pdf", width=4, height=3)
print(ampd_ftu)
dev.off()

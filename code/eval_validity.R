### This code creates the results from chapter 5.2.2 ### 

library("xtable")
library("ggplot2")

# Importing the lists from the results
results_aware <- readRDS("intermediate/results_aware.Rda")
results_ftu <- readRDS("intermediate/results_ftu.Rda")
results_fairadd <- readRDS("intermediate/results_fairadd.Rda")
results_fair <- readRDS("intermediate/results_fair.Rda")


#----------------------------------#
#### Structure of this code ########
#----------------------------------#
# 1. Creating tables
# 2. Creating box plot
#----------------------------------#

#----------------------------------#
#### 1. Creating tables ####
#----------------------------------#

# Function to create tables of the differences from the MBE values
create_difference_table <- function(results_gen, caption = "Differences Between Generated and True Results") {
  
  # Calculation of the difference in MBE values: generated versus true
  difference_list <- list(
    no_confounding_lg = abs(results_gen$res_gen_no_confounding_lg[[2]] - results_gen$res_true_no_confounding_lg[[2]]),
    no_confounding_rf = abs(results_gen$res_gen_no_confounding_rf[[2]] - results_gen$res_true_no_confounding_rf[[2]]),
    confounding_s_lg = abs(results_gen$res_gen_confounding_s_lg[[2]] - results_gen$res_true_confounding_s_lg[[2]]),
    confounding_s_rf = abs(results_gen$res_gen_confounding_s_rf[[2]] - results_gen$res_true_confounding_s_rf[[2]])
  )
  # Make a dataframe for the differences
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
  
  difference_df_wide <- reshape(difference_df, 
                                timevar = "Dataset", 
                                idvar = "Learner", 
                                direction = "wide")

  names(difference_df_wide) <- gsub("Difference\\.", "", names(difference_df_wide))
  
  # Create Latex-table
  xtable_obj <- xtable(difference_df_wide, caption = caption, digits = 3)
  print(xtable_obj, 
        include.rownames = FALSE, 
        caption.placement = "top", 
        hline.after = c(-1, 0, nrow(difference_df_wide)))
}

# Return tables for differences for each model
table_aware <- create_difference_table(results_aware, "Differences Between Generated and True Results (Fair)")
table_unaware <- create_difference_table(results_ftu, "Differences Between Generated and True Results (FTU)")
table_fairadd <-  create_difference_table(results_fairadd, "Differences Between Generated and True Results (Fair Add)")
table_fair <- create_difference_table(results_fair, "Differences Between Generated and True Results (Fair)")


#----------------------------------#
#### 1. Creating boxplot ####
#----------------------------------#


# Extraction of the AMPD values of the Unaware model
true_data_lg <- results_ftu$res_true_confounding_s_lg[[1]] 
gen_data_lg <- results_ftu$res_gen_confounding_s_lg[[1]]
df_lg <- data.frame(
  Dataset = rep("No Confounding LG", length(true_data_lg)),
  Value = c(true_data_lg, gen_data_lg),
  Type = rep(c("True", "Generated"), each = length(true_data_lg))
)

# Make Boxplot for AMPD values: generated versus true counterfactuals
ampd_ftu <- ggplot(df_lg, aes(x = Type, y = Value)) +
  geom_boxplot() +
  labs(
       x = "Counterfactual",
       y = "AMPD") +
  theme_bw() +
 ylim(0,1)+
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10))

# Save plot
pdf("plots/ampd_ftu.pdf", width=4, height=3)
print(ampd_fairdata)
dev.off()
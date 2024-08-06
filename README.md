# Challenges of Counterfactual Fairness: Multi-Objective Counterfactual Fairness and its Robustness to Confounders
- - -

This repository contains the code and resources for my Bachelor thesis: "Challenges of Counterfactual Fairness: Multi-Objective Counterfactual Fairness and its Robustness to Confounders". In this thesis, a simulation study was conducted to investigate the robustness of multi-objective counterfactual fairness methods to unmeasured confounders. 

## Directory Structure
- - -
- `code/`: Contains the main code to reproduce the results.
  - `eval_learner.R`: Contains code to create plots for evaluating learners.
  - `eval_methods.R`: Contains code to create plots for evaluating methods.
  - `eval_validity.R`: Contains code to create plots for evaluating validity.
  - `experiment_FTU.R`: Performs experiments using the Unaware method.
  - `experiment_aware.R`: Performs experiments using the Aware method.
  - `experiment_fairadd.R`: Performs experiments using the Fair Add method.
  - `experiment_fairdata.R`: Performs experiments with Fair Data.
  - `sim_data_fair.R`: Simulates fair datasets.
  - `sim_data_unfair.R`: Simulates unfair datasets.
  - `utils.R`: Contains functions to calculate MBE and AMPD values.
- `extern/counterfactuals/`: Contains the code needed to generate counterfactuals based on MOCF.
- `intermediate/`: Stores the results of the experiments.
- `plots/`: Contains all generated plots and visualizations of the results.


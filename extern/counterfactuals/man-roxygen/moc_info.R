#' @description 
#' MOC (Dandl et. al 2020) solves a multi-objective optimization problem to find counterfactuals. The four objectives
#' to minimize are:
#' \enumerate{
#'    \item {Distance to `desired_prob` (classification tasks) or `desired_prob` (regression tasks).}
#'    \item {Dissimilarity to `x_interest` measured by Gower's dissimilarity measure (Gower 1971).}
#'    \item {Number of feature changes.}
#'    \item {(Weighted) sum of dissimilarities to the `k` nearest data points in `predictor$data$X`.}
#' }  
#' 
#' For optimization, it uses the NSGA II algorithm (Deb et. al 2002) with mixed integer evolutionary 
#' strategies (Li et al. 2013). 
#' 
#' @details 
#' 
#' Several population initialization strategies are available:
#' \enumerate{
#'    \item {`random`: Feature values of new individuals are sampled from the feature value ranges in `predictor$data$X`.
#'    Some features values are randomly reset to their initial value in `x_interest`.}
#'    \item {`sd`: Like `random`, except that the sample ranges of numerical features are limited to one standard 
#'    deviation from their initial value in `x_interest`.}
#'    \item {`icecurve`: As in `random`, feature values are sampled from the feature value ranges in `predictor$data$X`. 
#'    Then, however, features are reset with probabilities relative to their importance: the higher the importance 
#'    of a feature, the higher the probability that its values differ from its value in `x_interest`. 
#'    The feature importance is measured using ICE curves (Goldstein et al. 2015).} 
#'    \item {`traindata`: Contrary to the other strategies, feature values are drawn from (non-dominated) data points
#'    in `predictor$data$X`; if not enough non-dominated data points are available, remaining individuals
#'    are initialized by random sampling. Subsequently, some features values are randomly reset to their initial value 
#'    in `x_interest` (as for `random`).}  
#' }  
#' 
#' 
#' @references 
#'
#' Dandl, Susanne, Christoph Molnar, Martin Binder, and Bernd Bischl. 2020. “Multi-Objective Counterfactual Explanations.” 
#' In Parallel Problem Solving from Nature – PPSN XVI, edited by Thomas Bäck, Mike Preuss, André Deutz, Hao Wang, Carola Doerr, 
#' Michael Emmerich, and Heike Trautmann, 448–69. Cham: Springer International Publishing.
#' 
#' Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A fast and elitist multiobjective genetic algorithm: NSGA-II. 
#' IEEE transactions on evolutionary computation, 6(2), 182-197.
#' 
#' Goldstein, Alex, Adam Kapelner, Justin Bleich, and Emil Pitkin. 2015. "Peeking Inside the Black Box: Visualizing 
#' Statistical Learning with Plots of Individual Conditional Expectation." Journal of Computational and Graphical 
#' Statistics 24 (1): 44–65. https: //doi.org/10.1080/10618600.2014.907095.
#' 
#' Gower, J. C. (1971), "A general coefficient of similarity and some of its properties". Biometrics, 27, 623–637.
#' 
#' Li, Rui, Michael T. M. Emmerich, Jeroen Eggermont, Thomas Bäck, M. Schütz, J. Dijkstra, and J. H. C. Reiber. 2013. 
#' "Mixed Integer Evolution Strategies for Parameter Optimization." Evolutionary Computation 21 (1): 29–64. https:
#' //doi.org/10.1162/EVCO_a_00059.
#' 

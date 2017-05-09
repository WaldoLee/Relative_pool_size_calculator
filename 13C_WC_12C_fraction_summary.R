library(xlsx)
#############################################################################
# Input data
neg_source <- "C:/Users/biology/Desktop/LAB DB/Project_ing/Spatial fluxomics/1. Rapid fractionation/2 experiment/LCMS result/20170429_13C_gln_glc_WC_12C_fraction_relative_pool_size_neg/relative_pool_size_neg.csv"
neg_table <- read.csv(neg_source, header = TRUE)
head(neg_table)
neg_table <- neg_table[, c(8, 9, 15, 17, 19, 21, 23, 25, 27, 29, 31)]
neg_table <- as.matrix(neg_table)
#############################################################################
# R_calculator
R_calculator <- function (col_num = 3) {
  I_nonlabeled <- numeric()
  I_labeled <- numeric()
  R <- numeric()
  labeled_set <- vector('numeric')
  result_vector <- vector()
  result_matrix <- vector()
  
  for (i in 1:nrow(neg_table)) {
    if (neg_table[i, 1] == "C12 PARENT") {
      if (i == 1) {
      } else {
        I_labeled <- sum(labeled_set)
        R <- I_labeled/I_nonlabeled
        result_vector <- c(result_vector, I_nonlabeled)
        result_vector <- c(result_vector, I_labeled)
        result_vector <- c(result_vector, R)
        #print(result_vector)
        result_matrix <- rbind(result_matrix, result_vector)
        }
      metabolite <- as.character(neg_table[i, 2])
      result_vector <- vector()
      I_nonlabeled <- numeric()
      I_labeled <- numeric()
      R <- numeric()
      result_vector <- c(result_vector, metabolite)
      I_nonlabeled <- as.numeric(neg_table[i, col_num])
      labeled_set <- vector('numeric')
    } else {
      labeled_set <- c(labeled_set, as.numeric(neg_table[i, col_num]))
    }
  }
  
  I_labeled <- sum(labeled_set)
  R <- I_labeled/I_nonlabeled
  result_vector <- c(result_vector, I_nonlabeled)
  result_vector <- c(result_vector, I_labeled)
  result_vector <- c(result_vector, R)
  result_matrix <- rbind(result_matrix, result_vector)
  colnames(result_matrix) <- c("compound", "nonlabeled", "labeled", "R")
  rownames(result_matrix) <- NULL
  result_matrix[, c(2,3,4)] <- as.numeric(result_matrix[, c(2,3,4)])
  result_df <- as.data.frame(result_matrix)
  return(result_df)
}
#############################################################################
create_R_summary_df <- function(input_data=neg_table){
  result_summary <- vector()
  for (i in 3:ncol(input_data)) {
    result_table <- R_calculator(i)
    result_table_trim <- result_table[,4]
    result_table_trim <- as.vector(result_table_trim)
    result_summary <- rbind(result_summary, result_table_trim)
  }
  colnames(result_summary) <- R_calculator(3)[,1]
  rownames(result_summary) <- colnames(input_data)[3:ncol(input_data)]
  result_summary_df <- as.data.frame(result_summary)
  result_summary_df <- data.frame(t(result_summary_df))
  return(result_summary_df)
}

R_summary <- create_R_summary_df()
R_summary <- t(R_summary)
#############################################################################
write.xlsx(x = R_summary, file = "C:/Users/biology/Desktop/LAB DB/Project_ing/Spatial fluxomics/1. Rapid fractionation/2 experiment/LCMS result/20170429_13C_gln_glc_WC_12C_fraction_relative_pool_size_neg/relative_pool_size_neg.xlsx", sheetName = "R_summary", row.names = TRUE)
#############################################################################
create_WC_fraction <- function(input_data=neg_table){
  result_summary <- vector()
  for (i in 3:ncol(input_data)) {
    result_table <- R_calculator(i)
    result_table <- as.matrix(result_table)
    result_table_trim <- as.numeric(result_table[,2])/(as.numeric(result_table[,2])+as.numeric(result_table[,3]))
    result_table_trim <- as.vector(result_table_trim)
    result_summary <- rbind(result_summary, result_table_trim)
  }
  colnames(result_summary) <- R_calculator(3)[,1]
  rownames(result_summary) <- colnames(input_data)[3:ncol(input_data)]
  result_summary_df <- as.data.frame(result_summary)
  result_summary_df <- data.frame(t(result_summary_df))
  return(result_summary_df)
}

WC_fraction <- create_WC_fraction()
WC_fraction <- t(WC_fraction)
#############################################################################
write.xlsx(x = WC_fraction, file = "C:/Users/biology/Desktop/LAB DB/Project_ing/Spatial fluxomics/1. Rapid fractionation/2 experiment/LCMS result/20170429_13C_gln_glc_WC_12C_fraction_relative_pool_size_neg/relative_pool_size_neg.xlsx", sheetName = "WC_fraction", row.names = TRUE, append=TRUE)
#############################################################################


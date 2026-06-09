
# Coursework MA321-7-SU: Team Project Task - R Code to Get Started
# Version:May 2026
rm(list=ls())
# --- Setup ---
# Set your working directory to the folder where your data is stored
# Example: setwd("path/to/your/directory")
# If you're using a University lab computer, ensure you save your work on your network drive 
# or back it up using cloud storage (e.g., Apple iCloud, Google Drive) or a USB stick.
# Always keep multiple backups of your work to prevent data loss.

# --- Load Data ---
# Copy the file "gene-expression-invasive-vs-noninvasive-cancer.csv" from Moodle to your working directory

InitialData <- read.csv(file = "gene-expression-invasive-vs-noninvasive-cancer.csv")

# --- Check the Data ---
# Use the following commands to understand the structure and dimensions of the dataset
str(InitialData)
# Output Example:
# 'data.frame': 78 obs. of 4773 variables
# $ X             : int  1 2 3 4 5 6 7 8 9 10 ...
# $ J00129        : num  -0.448 -0.48 -0.568 -0.819 ...
# $ Contig29982_RC: num  -0.296 -0.512 -0.411 -0.267 ...
# $ Contig42854   : num  -0.1 -0.031 -0.398 0.023 ...

dim(InitialData)  # Returns dataset dimensions (rows and columns)
# Example Output:
# [1] 78 4773

dimnames(InitialData)[[2]][4770:4773]  # View the names of the last columns
# Example Output:
# [1] "NM_000895" "NM_000898" "AF067420" "Class"

# Summary of the dataset:
# - 78 rows (patients)
# - 4773 columns: 4772 columns represent gene expression measurements, 
#   and column 4773 contains the "Class" variable (values: 1 or 2).

# Check the distribution of the "Class" variable
table(InitialData[, 'Class'])
# Example Output:
# Class
#   1   2 
#  34  44 



# --- Randomization Setup ---

# The script assigns a subset of variables to each team.
# In the file 'teamsubsets.csv', each team number is associated with 500 columns (variables).

# Load the file 'teamsubsets.csv', which contains the numbers of the teams 
# and their associated variable subsets.
teamsubsets <- read.table('teamsubsets.csv')

# Specify the number of your team to identify your teams's subset of variables.
# Replace 50 by the number of your team.

your_team <- 5

my_team_subset <- teamsubsets[your_team,]
str(my_team_subset)
# Extract the subset of variables for the number of your team.
# The result is a vector of 500 variables associated with the number of your team.

print(my_team_subset) # Print your team's subset of variables.

# Assume that InitialData is the preloaded dataset containing the original variables.

Class <- InitialData$Class # Extract the "Class" column, which represents the labels or targets.

# Select only the columns (variables) specified in the subset (my_subset).
X <- InitialData[, as.integer(my_team_subset)]

# Combine the "Class" column with the selected variables to create the final dataset.

MyTeam_DataSet <- cbind(Class, X)

# The dataset 'MyTeam_DataSet' contains:
# - The "Class" column as the first column.
# - The 500 variables associated with your team number.

str(MyTeam_DataSet)

#

dim(MyTeam_DataSet)

# The data set has 78 rows (observations/patients) and 501 variables (class variable and 500 features/genes)

dimnames(MyTeam_DataSet)[[2]]

# For example with team number 50 you get
# > dimnames(MyTeam_DataSet)[[2]]
# [1] "Class"          "X98260"         "Contig26706_RC" "NM_002923"      "AL137736"    
# [6] "NM_006086"      "AK000345"       "NM_014668"      "Contig50184_RC" "Contig25659_RC"
# ...
# [496] "NM_001797"      "NM_015623"      "AL117661"       "NM_006762"      "NM_001360"     
# [501] "NM_002729"
# 

MyTeam_DataSet[1:5,1:6]



# -----------------------------------------------------------------------------------------
  # Convert outcome to factor with readable labels
  # Assignment definition:
  # class == 2 means invasive cancer
  # class == 1 means non-invasive cancer
  MyTeam_DataSet$Class <- factor(MyTeam_DataSet$Class,
                                 levels = c(1, 2),
                                 labels = c("non_invasive", "invasive"))


# --------------------------------------------------------------
 # Varun -  Question 1 and some  Q2 :()




# -----------------------------
# Q1. Initial data checking / exploratory analysis
# -----------------------------

cat("Dataset dimensions:\n")
print(dim(MyTeam_DataSet))

cat("\nClass distribution:\n")
print(table(MyTeam_DataSet$Class))
#using the below code to identify proportions
print(prop.table(table(MyTeam_DataSet$Class)))

#checking for missing values since it could cause errors going further
cat("\nNumber of missing values:\n")
print(sum(is.na(MyTeam_DataSet)))

cat("\nSummary of first five selected genes:\n")
print(summary(MyTeam_DataSet[, 2:6]))

# Boxplot for the first six genes by class
first_six_genes <- names(MyTeam_DataSet)[2:7]

# reshaping data into longer format for plotting
boxplot_data <- stack(MyTeam_DataSet[, first_six_genes])
boxplot_data$Class <- rep(MyTeam_DataSet$Class, times = length(first_six_genes))

qplot(x = ind, y = values, fill = Class, data = boxplot_data,
      geom = "boxplot") +
  labs(title = "Expression distributions for first six team genes",
       x = "Gene",
       y = "Expression value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PCA plot using all 500 team genes
gene_matrix <- scale(MyTeam_DataSet[, -1])  # removing class column

pca_fit <- prcomp(gene_matrix, center = TRUE, scale. = TRUE)

pca_data <- data.frame(
  PC1 = pca_fit$x[, 1],
  PC2 = pca_fit$x[, 2],
  Class = MyTeam_DataSet$Class
)

var_explained <- round(100 * summary(pca_fit)$importance[2, 1:2], 2)

ggplot(pca_data, aes(x = PC1, y = PC2, colour = Class)) +
  geom_point(size = 3) +
  labs(title = "PCA plot of team gene-expression data",
       x = paste0("PC1 (", var_explained[1], "% variance)"),
       y = paste0("PC2 (", var_explained[2], "% variance)"))


# -----------------------------
# 2. Train/test split for supervised learning
# -----------------------------
install.packages("caret")
library(caret)

train_index <- createDataPartition(MyTeam_DataSet$Class,
                                   p = 0.70,
                                   list = FALSE)  # split 70/30
# and used createdatapartition to maintain class distribution

train_data <- MyTeam_DataSet[train_index, ]
test_data  <- MyTeam_DataSet[-train_index, ]

cat("\nTraining class distribution:\n")
print(table(train_data$Class))

cat("\nTest class distribution:\n")
print(table(test_data$Class))

# -----------------------------
# 3. Feature selection using training data only
# -----------------------------
# Reason:
# There are 500 genes but only 78 patients.
# LDA/QDA can become unstable with too many predictors.
# So we select the top genes using only the training data.

t_test_pvalue <- function(gene_values, class_values) {
  out <- t.test(gene_values ~ class_values)
  return(out$p.value)
}

p_values <- sapply(train_data[, -1],
                   t_test_pvalue,
                   class_values = train_data$Class)

p_values <- sort(p_values)

# Top 15 selected genes
# This keeps the number of predictors small enough for QDA.
top_k <- 15
selected_genes <- names(p_values)[1:top_k]

cat("\nSelected genes used for LDA/QDA/SVM/RF:\n")
print(selected_genes)

train_selected <- train_data[, c("Class", selected_genes)]
test_selected  <- test_data[, c("Class", selected_genes)]
# -----------------------------
# 4. Pre-processing
# -----------------------------
# LDA/QDA and SVM are sensitive to scale.
# Scaling is fitted on training data only, then applied to test data.

# calculating mean and SD of each gene
preprocess_fit <- preProcess(train_selected[, -1],
                             method = c("center", "scale"))

train_scaled_x <- predict(preprocess_fit, train_selected[, -1])
test_scaled_x  <- predict(preprocess_fit, test_selected[, -1])

train_scaled <- data.frame(Class = train_selected$Class, train_scaled_x)
test_scaled  <- data.frame(Class = test_selected$Class, test_scaled_x)

# Make invasive the positive class
train_scaled$Class <- relevel(train_scaled$Class, ref = "invasive")
test_scaled$Class  <- relevel(test_scaled$Class, ref = "invasive")

# Cross-validation setup
# controlling hyperparamenters for RF and SVM, im doing repeated 
# cross-validation with 5 folds, repeating 10 times because the dataset is TINY
cv_ctrl <- trainControl(method = "repeatedcv",
                        number = 5,
                        repeats = 10,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions = "final")
# -----------------------------
# Helper function for model evaluation
# -----------------------------

evaluate_model <- function(model_name, predicted_class, predicted_prob, actual_class) {
  
  cm <- confusionMatrix(predicted_class,
                        actual_class,
                        positive = "invasive")
  
  auc_value <- auc(response = actual_class,
                   predictor = predicted_prob,
                   levels = c("non_invasive", "invasive"),
                   direction = "<")
  
  data.frame(Model = model_name,
             Accuracy = as.numeric(cm$overall["Accuracy"]),
             Sensitivity = as.numeric(cm$byClass["Sensitivity"]),
             Specificity = as.numeric(cm$byClass["Specificity"]),
             AUC = as.numeric(auc_value))
}

results <- data.frame()

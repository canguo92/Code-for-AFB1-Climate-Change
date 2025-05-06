# Load necessary libraries
library(readxl)
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

# 1. Read Sheet1 and rename columns to syntactic names
#df <- read_excel("250421-09-18毒素原始数据用于随机森林分类建模.xlsx", sheet = "Sheet1") %>%
#  rename_with(~ make.names(.x, unique = TRUE))
df <- read_excel("250421-09-18毒素原始数据用于随机森林分类建模.xlsx", sheet = "Sheet1") %>%
  rename_with(~ make.names(.x, unique = TRUE))

# Identify the syntactic column name for '超标率 > 10 ppb'
#target_col <- make.names("超标率 > 100 ppb", unique = TRUE)
target_col <- make.names("AFB1平均值", unique = TRUE)

# 2. Define target variable (Pos if >0, Neg if =0)
df <- df %>% mutate(
  target = factor(ifelse(.data[[target_col]] > 0, "Pos", "Neg"))
)

# 3. Select features: syntactic names for columns O-AI (positions 15 to 35)
feature_cols <- names(df)[15:35]
#feature_cols <- names(df)[14:19]

# 4. Combine into a clean dataset and drop rows with missing values
data <- df %>% select(all_of(c("target", feature_cols))) %>% na.omit()

# 5. Split into training and test sets (70%/30%), stratified by target
set.seed(42)
trainIndex <- createDataPartition(data$target, p = 0.7, list = FALSE)
trainData <- data[trainIndex, ]
testData  <- data[-trainIndex, ]

# 6. Train Random Forest classifier
rf_model <- randomForest(target ~ ., data = trainData, importance = TRUE)

# 7. Predict on training and test sets
train_pred <- predict(rf_model, trainData)
test_pred  <- predict(rf_model, testData)

# 8. Compute confusion matrices and classification metrics
train_cm <- confusionMatrix(train_pred, trainData$target)
test_cm  <- confusionMatrix(test_pred, testData$target)
print(train_cm)
print(test_cm)

# 9. Extract top 10 feature importances by MeanDecreaseGini
importance_df <- as.data.frame(importance(rf_model, type = 2))
importance_df$Feature <- rownames(importance_df)
importance_top10 <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 10)

# 10. Visualize results with centered titles, Arial font, and detailed subtitles
library(ggplot2)
library(scales)

# Function to plot confusion matrix heatmap
ggplot_confusion <- function(cm_obj, title) {
  # Extract confusion table
  cm_tab <- as.data.frame(cm_obj$table)
  # Compute metrics
  acc <- cm_obj$overall["Accuracy"]
  sens <- cm_obj$byClass["Sensitivity"]
  spec <- cm_obj$byClass["Specificity"]
  f1   <- cm_obj$byClass["F1"]
  
  ggplot(cm_tab, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = Freq), color = "white", size = 5, fontface = "bold") +
    scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Count") +
    labs(
      title    = title,
      subtitle = sprintf("Accuracy=%.2f; Sensitivity=%.2f; Specificity=%.2f; F1=%.2f", acc, sens, spec, f1),
      x        = "Predicted label",
      y        = "True label"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title       = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle    = element_text(size = 12, color = "gray30", hjust = 0.5),
      axis.title       = element_text(face = "bold", size = 14),
      axis.text        = element_text(size = 12),
      legend.title     = element_text(face = "bold", size = 12),
      panel.grid       = element_blank()
    )
}

# Generate confusion matrix plots
gg1 <- ggplot_confusion(train_cm, "Training Set Confusion Matrix")
gg2 <- ggplot_confusion(test_cm,  "Test Set Confusion Matrix")

# Feature importance bar plot with blue gradient fill
ggp <- ggplot(importance_top10, aes(
  x = reorder(Feature, MeanDecreaseGini),
  y = MeanDecreaseGini,
  fill = MeanDecreaseGini
)) +
  geom_col(width = 0.7) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Importance") +
  coord_flip() +
  labs(
    title = "Top 10 Feature Importances",
    x     = NULL,
    y     = "Mean Decrease in Gini"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title         = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y        = element_text(size = 12),
    axis.text.x        = element_text(size = 12),
    axis.title.x       = element_text(face = "bold", size = 14),
    legend.title       = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# Print plots
print(gg1)
print(gg2)
print(ggp)




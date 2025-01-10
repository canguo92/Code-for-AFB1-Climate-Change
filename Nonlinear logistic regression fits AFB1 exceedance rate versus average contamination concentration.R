# Load required packages
# Uncomment the following line to install the 'tidyverse' package if not already installed
# install.packages("tidyverse")
library(tidyverse)

# 1. Load the dataset
data <- read_csv("Anhui_mydata.csv")

# 2. Data cleaning: Remove rows where exceedance rates or average values are negative or invalid
data_clean <- data %>%
  filter(AFB_exce >= 0, AFB_exce <= 100, AFB_aver >= 0)

# 3. Fit a nonlinear logistic model
# Set initial parameter values
start_vals <- list(a = -4, b = 0.7)

# Perform nonlinear fitting using nls (nonlinear least squares)
model_nls <- try(
  nls(AFB_exce ~ 100 / (1 + exp(-(a + b * log(AFB_aver + 1)))),
      data = data_clean,
      start = start_vals,
      trace = TRUE),
  silent = TRUE
)

# Check model summary
print(summary(model_nls))

# Extract model parameters
a_est <- coef(model_nls)["a"]
b_est <- coef(model_nls)["b"]

# Generate predictions based on the model
AFB_exce_pred <- predict(model_nls, newdata = data_clean)

# Calculate correlation coefficient between observed and predicted values
cor_coeff <- cor(data_clean$AFB_exce, AFB_exce_pred)

# Create data for plotting the fitted curve
AFB_aver_seq <- seq(
  min(data_clean$AFB_aver, na.rm = TRUE),
  max(data_clean$AFB_aver, na.rm = TRUE),
  length.out = 200
)
AFB_exce_pred_curve <- 100 / (1 + exp(-(a_est + b_est * log(AFB_aver_seq + 1))))

# 4. Export the plot as a high-resolution PNG image
# Set the output file name and path
output_file <- "Anhui_AFB_exce_vs_AFB_aver.tiff"

# Open a TIFF device with 500 DPI resolution
tiff(filename = output_file, width = 8, height = 6, units = "in", res = 500)

# Set graphical parameters
par(mar = c(5, 5, 4, 2) + 0.1)

# Plot scatterplot of observed data
plot(data_clean$AFB_aver, data_clean$AFB_exce,
     xlab = expression(AFB[1]~"(Average Concentration, µg/kg)"),
     ylab = expression(AFB[1]~"(Exceedance Rate, %)"),
     main = "Anhui",
     pch = 19, col = "blue",
     cex.lab = 1.8,  # Enlarged axis labels
     cex.axis = 1.5, # Enlarged axis tick labels
     cex.main = 2.5) # Enlarged main title

# Add fitted curve to the plot
lines(AFB_aver_seq, AFB_exce_pred_curve, col = "red", lwd = 2)

# Add grid lines
grid()

# Display the correlation coefficient on the plot
legend_text <- paste0("R: ", round(cor_coeff, 3))
legend("bottomright",
       legend = c("Observed Data", "Fitted Curve", legend_text),
       col = c("blue", "red", "black"), 
       pch = c(16, NA, NA), 
       lty = c(NA, 1, NA), 
       lwd = c(NA, 2, NA),
       cex = 1.8, bty = "n")

# Display the fitted equation on the plot
formula_text <- paste0("AFB_exce = 100 / (1 + exp(-(", 
                       round(a_est, 2), " + ", round(b_est, 2), " * log(AFB_aver + 1))))")
text(x = max(data_clean$AFB_aver) * 0.6, 
     y = max(data_clean$AFB_exce) * 0.9, 
     labels = formula_text, col = "black", cex = 1)

# Close the TIFF device
dev.off()

cat(paste0("The plot has been successfully saved as '", output_file, "' at 500 DPI resolution.\n"))

# 5. Predict exceedance rates for future data and export to an Excel file
# Uncomment to install required packages
# install.packages("openxlsx")
library(readxl)
library(openxlsx)

# Define input and output file paths
input_file <- "Anhui_future_data.xlsx"
output_file <- "Anhui_forecast_exceedance_rates.xlsx"

# Read the input Excel file
future_data <- read_excel(input_file)

# Verify that the required column exists
if (!"AFB_aver" %in% colnames(future_data)) {
  stop("The input Excel file must contain a column named 'AFB_aver'.")
}

# Calculate predicted exceedance rates based on the model
future_data <- future_data %>%
  mutate(AFB_exce_pred = 100 / (1 + exp(-(a_est + b_est * log(AFB_aver + 1)))))

# Round predicted values for clarity
future_data <- future_data %>%
  mutate(AFB_exce_pred = round(AFB_exce_pred, 2))

# Display the updated dataset
print(head(future_data))

# Write the results to an Excel file
write.xlsx(future_data, file = output_file, overwrite = TRUE)

cat(paste0("Predicted exceedance rates have been successfully saved to '", output_file, "'.\n"))

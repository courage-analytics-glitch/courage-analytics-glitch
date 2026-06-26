# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Ghana Petroleum Sector Data 2000-2023
ghana_petroleum <- data.frame(
  Year = 2000:2023,
  Crude_Oil_Production_Bbl = c(0,0,0,0,0,0,0,0,0,0,
                               26000,29000,35000,38000,102000,105000,101000,
                               98000,167000,172000,163000,158000,162000,170000),
  Petrol_Consumption_ML = c(820,850,880,910,950,980,
                            1020,1060,1110,1150,1200,1260,1320,1380,1440,
                            1500,1560,1620,1680,1740,1800,1860,1920,1980),
  Diesel_Consumption_ML = c(650,680,710,740,780,810,
                            850,890,930,970,1010,1060,1110,1160,1210,1260,
                            1310,1360,1410,1460,1510,1560,1610,1660),
  Crude_Price_USD = c(28,24,26,31,41,55,65,72,97,
                      62,79,111,112,108,99,52,44,54,71,64,42,70,95,83),
  Petroleum_Revenue_USD_M = c(180,165,175,195,240,310,
                              380,430,520,390,480,650,720,690,640,420,380,450,
                              520,490,380,520,680,610)
)

print(head(ghana_petroleum, 10))
print(summary(ghana_petroleum))

# Create visualizations
par(mfrow = c(2,2))

# Plot 1: Crude Oil Production
plot(ghana_petroleum$Year, ghana_petroleum$Crude_Oil_Production_Bbl,
     type = "l", col = "darkgreen", lwd = 2,
     main = "Ghana Crude Oil Production 2000-2023",
     xlab = "Year", ylab = "Barrels per Day")

# Plot 2: Petroleum Revenue
plot(ghana_petroleum$Year, ghana_petroleum$Petroleum_Revenue_USD_M,
     type = "b", col = "darkred", lwd = 2,
     main = "Ghana Petroleum Revenue (USD Millions)",
     xlab = "Year", ylab = "USD Millions")

# Plot 3: Fuel Consumption
plot(ghana_petroleum$Year, ghana_petroleum$Petrol_Consumption_ML,
     type = "l", col = "blue", lwd = 2,
     main = "Petrol vs Diesel Consumption",
     xlab = "Year", ylab = "Million Litres")
lines(ghana_petroleum$Year, ghana_petroleum$Diesel_Consumption_ML,
      col = "orange", lwd = 2)
legend("topleft", legend = c("Petrol", "Diesel"),
       col = c("blue", "orange"), lwd = 2)

# Plot 4: Crude Price Trend
plot(ghana_petroleum$Year, ghana_petroleum$Crude_Price_USD,
     type = "l", col = "purple", lwd = 2,
     main = "Crude Oil Price USD per Barrel",
     xlab = "Year", ylab = "USD")

par(mfrow = c(1,1))


# Correlation Analysis
cor_data <- ghana_petroleum[, c("Crude_Oil_Production_Bbl",
                                "Petrol_Consumption_ML",
                                "Diesel_Consumption_ML",
                                "Crude_Price_USD",
                                "Petroleum_Revenue_USD_M")]

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color",
         type = "upper",
         title = "Ghana Petroleum Sector Correlation Matrix",
         mar = c(0,0,2,0),
         tl.cex = 0.7)


library(ggplot2)
library(dplyr)
library(corrplot)

ghana_petroleum <- data.frame(
  Year = 2000:2023,
  Crude_Oil_Production_Bbl = c(0,0,0,0,0,0,0,0,0,0,
                               26000,29000,35000,38000,102000,105000,101000,
                               98000,167000,172000,163000,158000,162000,170000),
  Petrol_Consumption_ML = c(820,850,880,910,950,980,
                            1020,1060,1110,1150,1200,1260,1320,1380,1440,
                            1500,1560,1620,1680,1740,1800,1860,1920,1980),
  Diesel_Consumption_ML = c(650,680,710,740,780,810,
                            850,890,930,970,1010,1060,1110,1160,1210,1260,
                            1310,1360,1410,1460,1510,1560,1610,1660),
  Crude_Price_USD = c(28,24,26,31,41,55,65,72,97,
                      62,79,111,112,108,99,52,44,54,71,64,42,70,95,83),
  Petroleum_Revenue_USD_M = c(180,165,175,195,240,310,
                              380,430,520,390,480,650,720,690,640,420,380,450,
                              520,490,380,520,680,610)
)

cor_data <- ghana_petroleum[, c("Crude_Oil_Production_Bbl",
                                "Petrol_Consumption_ML",
                                "Diesel_Consumption_ML",
                                "Crude_Price_USD",
                                "Petroleum_Revenue_USD_M")]

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color",
         type = "upper",
         title = "Ghana Petroleum Sector Correlation Matrix",
         mar = c(0,0,2,0),
         tl.cex = 0.7)

# Correlation Analysis - Ghana Petroleum Sector

cor_data <- ghana_petroleum[, c("Crude_Oil_Production_Bbl",
                                "Petrol_Consumption_ML",
                                "Diesel_Consumption_ML",
                                "Crude_Price_USD",
                                "Petroleum_Revenue_USD_M")]

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 2))

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.75,
         title = "Ghana Petroleum Sector Correlation Matrix",
         mar = c(0, 0, 2, 0),
         tl.cex = 0.7,
         tl.col = "black",
         col = colorRampPalette(c("#d73027", "white", "#1a9850"))(200))

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.75,
         title = "Ghana Petroleum Sector Correlation Matrix",
         mar = c(0, 0, 2, 0),
         tl.cex = 0.7,
         tl.col = "black",
         col = colorRampPalette(c("#d73027", "white", "#1a9850"))(200))

# Key Insights
cat("\n--- GHANA PETROLEUM SECTOR: KEY FINDINGS ---\n")
cat("Crude Price vs Revenue correlation:", round(cor_matrix["Crude_Price_USD", "Petroleum_Revenue_USD_M"], 2), "\n")
cat("Petrol Consumption vs Revenue correlation:", round(cor_matrix["Petrol_Consumption_ML", "Petroleum_Revenue_USD_M"], 2), "\n")
cat("Diesel Consumption vs Revenue correlation:", round(cor_matrix["Diesel_Consumption_ML", "Petroleum_Revenue_USD_M"], 2), "\n")
cat("Crude Production vs Revenue correlation:", round(cor_matrix["Crude_Oil_Production_Bbl", "Petroleum_Revenue_USD_M"], 2), "\n")
cat("Peak Revenue Year:", ghana_petroleum$Year[which.max(ghana_petroleum$Petroleum_Revenue_USD_M)], "\n")
cat("Peak Crude Price Year:", ghana_petroleum$Year[which.max(ghana_petroleum$Crude_Price_USD)], "\n")
cat("Average Annual Revenue USD M:", round(mean(ghana_petroleum$Petroleum_Revenue_USD_M), 0), "\n")

# Save chart to file
png("Ghana_Petroleum_Correlation_Matrix.png", width = 1200, height = 1000, res = 150)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.75,
         title = "Ghana Petroleum Sector Correlation Matrix",
         mar = c(0, 0, 2, 0),
         tl.cex = 0.7,
         tl.col = "black",
         col = colorRampPalette(c("#d73027", "white", "#1a9850"))(200))
dev.off()

cat("\nChart saved as Ghana_Petroleum_Correlation_Matrix.png\n")
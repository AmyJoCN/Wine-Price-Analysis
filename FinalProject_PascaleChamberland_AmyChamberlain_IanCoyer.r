
# Load libraries
# Load any additional libraries. No install.package()
library(lubridate)
library(tidyverse)

# Load data and select columns as necessary
wine <- "winemag-data_first150k.csv" %>% read_csv()

wine <- wine %>% rename(id = X1)
wine <- wine  %>% 
    mutate_if(is.character, factor) %>% 
    mutate(value = points/price)  %>% 
    mutate(nameLength = str_length(designation))  %>% 
    select(-description, -region_2, -winery, -designation)  %>%
    rename(region = region_1)  %>% 
    filter(complete.cases(.))  %>% 
    filter(price < 400)

# filter out all data except for wines from Washington, Oregon, and Napa Valley
pnw_wine  <- wine  %>% 
    filter (province %in% c("Washington", "Oregon") | region == "Napa Valley")

pnw_wine  %>% glimpse()
pnw_wine %>% summary()

# Create dataframes for t-tests
# Split by location
wa_wine <- pnw_wine  %>% filter(province == "Washington")
or_wine <- pnw_wine  %>% filter(province == "Oregon")
napa_wine <- pnw_wine  %>% filter(province == "California")

# Split by points
high_points <- pnw_wine %>% filter(points > 89)
low_points <- pnw_wine %>% filter(points < 90)

# Split by name length
short_name  <- pnw_wine  %>% filter(nameLength < 15)
long_name  <- pnw_wine  %>% setdiff(short_name)

# explore the relationship of points to price
pnw_wine %>% filter (price < 400) %>% 
ggplot(aes(x=points,y=price, color = province)) + 
   geom_jitter(alpha = 0.4) + 
   geom_smooth(method = "lm", color = "black") +
   facet_grid(~province) +
   labs(title="Wine price vs points",
       subtitle="by province")

# explore how the province impacts price
pnw_wine  %>% ggplot(mapping = aes(x = province, y = price)) +
    geom_boxplot() +
   labs(title="Price vs province")

# Explore how name length impacts price
pnw_wine %>% 
ggplot(aes(x=nameLength,y=price)) + 
   geom_jitter(alpha = 0.4) + 
   geom_smooth(method = "lm") +
   labs(title="Price by length of wine name")

# compare price by high points (above median) vs. low points (below median)
cat("\n**** Price of High Points Wine > Price of Low Points Wine ****")
t.test (high_points$price, low_points$price, conf.level = 0.95 , alternative="greater")

# compare price by province
cat("\n**** Price of CA wine != WA wine ****")
t.test (napa_wine$price, wa_wine$price, conf.level = 0.95 , alternative="two.sided")

cat("\n**** Price of CA wine != OR wine ****")
t.test (napa_wine$price, or_wine$price, conf.level = 0.95 , alternative="two.sided")

cat("\n**** Price of OR wine != WA wine ****")
t.test (or_wine$price, wa_wine$price, conf.level = 0.95 , alternative="two.sided")

# compare the price of wines with short name to the price of wines with long names
cat("\n**** Price of wine with short name != price of wine with long name ****")
t.test(short_name$price, long_name$price, alternative = "two.sided", conf.level = 0.95)

# normalize features
normalize <- function(x) (x - mean(x))/sd(x)

pnw_wine <- pnw_wine  %>% 
    mutate(normal_points = normalize(points),
           normal_value = normalize(value),
           normal_nameLength = normalize(nameLength))
    
pnw_wine  %>% glimpse()

# Set seed for predictability
set.seed(1222)

# Get 70% of the data for training
wine_train <- pnw_wine %>% 
  sample_frac(0.7)

# Creat test set with the remaining rows
wine_test <- pnw_wine %>%  
  setdiff(wine_train)

# Display row counts of train and test data
print(str_c("wine_train rows: ", nrow(wine_train)))
print(str_c("wine_test rows: ", nrow(wine_test)))

# Compare train and test data to original data source
nrow(wine_train) + nrow(wine_test) == nrow(pnw_wine)

# create model to predict price
model = lm(price ~  normal_points + province + normal_nameLength, data = wine_train)
model$coefficients

summary(model)
confint(model)

# create second model to predict price
model2 = lm(price ~  normal_points + normal_value + province + normal_nameLength, data = wine_train)
model$coefficients

summary(model2)
confint(model2)

# assess how well the model predicts the test data
# calculate the predicted score and residuals for Model 1
wine_test1 <- wine_test %>% 
  mutate(predicted_price = predict(model, newdata = wine_test),
        resids = price - predicted_price)  %>% 
  select(price, predicted_price, resids)  %>% 
  glimpse()

# calculate the predicted score and residuals for Model 2
wine_test2 <- wine_test %>% 
  mutate(predicted_price = predict(model2, newdata = wine_test),
        resids = price - predicted_price)  %>% 
  select(price, predicted_price, resids)  %>% 
  glimpse()

# Create a function to print the same plots to compare the residuals for Model 1 and Model 2
plot_resids <- function(df, model){
  # Plot both a histogram and a density plot
  p1 <- df %>% 
    ggplot(aes(resids, ..density..)) + 
    geom_histogram(bins = 30, alpha = 0.3, color = 'blue') +
    geom_density(size = 1) +
    labs(title="Histogram and density function for residuals", subtitle = model, x="Residual value")
  
  # Plot a qq plot
  p2 <- df %>% 
    ggplot(aes(sample = resids)) + 
    geom_qq() + 
    labs(title="Quantile-quantile Normal plot of residuals", subtitle = model)
  
  # Plot a scatter
  p3 <- df %>% 
    ggplot(aes(x = predicted_price, y = resids)) +
      geom_point() + 
      geom_smooth(size = 1, color = 'red') +
      labs(title="Residuals vs fitted values", subtitle = model, x="Fitted values", y="Residuals")
  
  # Print the plots
  p1 %>% print()
  p2 %>% print()
  p3 %>% print()
}

plot_resids(wine_test1, "Model 1")
plot_resids(wine_test2, "Model 2")

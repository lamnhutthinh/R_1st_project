library(readxl)
library(tidyverse)
# Import data
data = read_excel("040522 Data Mid-term test Final.xlsx")
View(data)
# Create dataset
set.seed(751)
index = sample(1:nrow(data), 100)
index
data_sample = data[index,]
# Choosing columns
data_sample = data_sample[c('no', 'firmcode', 
                            'firmname','industry', 
                            'exchangename', 'totaldebt', 
                            'totalasset', 'cash')]
View(data_sample)
# Replace NA
data_sample['totalasset'][is.na(data_sample['totalasset'])] = 
  median(data_sample$totalasset, na.rm = T)
data_sample['cash'][is.na(data_sample['cash'])] = 
  median(data_sample$cash, na.rm = T)
data_sample['totaldebt'][is.na(data_sample['totaldebt'])] = 
  median(data_sample$totaldebt, na.rm = T)
# Check if there is any NA values left in the sample
sum(is.na(data_sample))
# Calculate cash holding
library("dplyr")
data_sample$cash_holding = data_sample$cash/data_sample$totalasset
data_sample$leverage = data_sample$totaldebt/data_sample$totalasset
data_sample$firmsize = log(data_sample$totalasset)
# 5 firms with highest cash holding
data_sample_1 = data_sample %>% 
  arrange(desc(cash_holding))
colnames(data_sample_1)[3] = 'Highest Cash Holding Companies'
highest5 = data_sample_1[1:5,c(3,9)]
highest5
# 5 firms with lowest cash holding
data_sample_2 = data_sample %>% 
  arrange(cash_holding)
colnames(data_sample_2)[3] = 'Lowest Cash Holding Companies'
lowest5 = data_sample_2[1:5,c(3,9)]
lowest5
# Industry Name
print(paste("All industry names"))
unique(data_sample$industry)
data_sample_1[1:5,c(3,4)]
data_sample_2[1:5,c(3,4)]
# Descriptive statistics:
# Different categories of the discrete variable

mean_firmsize = mean(data_sample$firmsize)
data_sample$class_firmsize = data_sample$firmsize > mean_firmsize
data_sample$class_firmsize = replace(data_sample$class_firmsize, data_sample$class_firmsize == 'TRUE', 'LARGE')
data_sample$class_firmsize = replace(data_sample$class_firmsize, data_sample$class_firmsize == 'FALSE', 'SMALL')
View(data_sample)
discrete_stats = data_sample %>% 
  group_by(class_firmsize) %>% 
  summarise(median_cashholding = median(cash_holding),
            mean_cashholding = mean(cash_holding),
            max_cashholding = max(cash_holding),
            min_cashholding = min(cash_holding),
            sd_cashholding = sd(cash_holding)
  )
view(discrete_stats)

# Groups of above/below median of the continuous variable

med_leverage = median(data_sample$leverage)
med_leverage
data_sample$class_leverage = data_sample$leverage > med_leverage


data_sample$class_leverage = replace(data_sample$class_leverage, data_sample$class_leverage == 'TRUE', 'HIGH')
data_sample$class_leverage = replace(data_sample$class_leverage, data_sample$class_leverage == 'FALSE', 'LOW')

continuous_stats = data_sample %>% 
  group_by(class_leverage) %>% 
  summarise(median_cashholding = median(cash_holding),
            mean_cashholding = mean(cash_holding),
            max_cashholding = max(cash_holding),
            min_cashholding = min(cash_holding),
            sd_cashholding = sd(cash_holding)
)
view(continuous_stats)
# 4. Data visualization

library(forcats)
library(ggplot2)
library(scales)

# 1. provide histogram of cash holding

ggplot(data_sample, aes(x = cash_holding))+
  geom_histogram(color = 'green')

# 2. provide scatter plot of cash holding with the continuous variable
ggplot(data_sample, aes(x = cash_holding, y = leverage ))+
  geom_point(size = 2) +
  geom_smooth(method = 'lm')
# 3. provide boxplot of cash holding with the discrete variable (different color for different categories of discrete variable)
ggplot(data_sample, aes( x = class_firmsize, y = cash_holding,fill = class_firmsize ))+
  geom_boxplot()

# 4. provide a plot that allow the combination of continuous, discrete variables and leverage/cash holding
ggplot(data_sample, aes(x = cash_holding, y = leverage, color = class_firmsize ))+
  geom_point(size = 2) +
  scale_color_manual( values = c('SMALL' = 'green',
                                 'LARGE' = 'red')) +
  geom_smooth(method = 'lm')


# 5. Using LOOP:

# Count the number of firms in an industry
input = 'Healthcare'
count = 0
for (i in 1:nrow(data_sample)) {
  if (data_sample[i,4] == input){
    count = count + 1
  }
}
print(paste(input,':',count,'firm(s)'))

# Count the number of firms in an industry and with cash holding above a certain value 
input = 'Financials'
count = 0
threshold = 0.3
for (i in 1:nrow(data_sample)) {
  if (data_sample[i,4] == input&data_sample[i,9]>threshold){
    count = count + 1
  }
}
print(paste(input,'firm(s) with cash holding ratio greater than',threshold,':',count))

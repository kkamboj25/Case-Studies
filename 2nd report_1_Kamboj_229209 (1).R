
library(dplyr)
library(gridExtra)
library(ggplot2)


rent_index <- read.csv('C:/Users/karti/Downloads/rent_index_99.csv')
summary(rent_index)
summary(rent_per_sqmtr_qloc_3)
rent_index <- rent_index %>% 
  mutate(rent_per_sqmtr = net.rent/living.area)
rent_per_sqmtr_df <- data.frame(rent_index$rent_per_sqmtr,rent_index$quality.of.location)


rent_per_sqmtr_qloc_1 <- rent_index[rent_index$quality.of.location == 1, ]
rent_per_sqmtr_qloc_2 <- rent_index[rent_index$quality.of.location == 2, ]
rent_per_sqmtr_qloc_3 <- rent_index[rent_index$quality.of.location == 3, ]

#Task1
#Check assumptions of the data
#1) Normality using Q-Q plot
qqnorm(rent_per_sqmtr_qloc_1$rent_per_sqmtr, main = "Average location (1)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )
qqline(rent_per_sqmtr_qloc_1$rent_per_sqmtr, col = "darkred", lwd = 2)

qqnorm(rent_per_sqmtr_qloc_2$rent_per_sqmtr, main = "Good location (2)", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )
qqline(rent_per_sqmtr_qloc_2$rent_per_sqmtr, col = "darkred", lwd = 2)

qqnorm(rent_per_sqmtr_qloc_3$rent_per_sqmtr, main = "Top location (3)", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )
qqline(rent_per_sqmtr_qloc_3$rent_per_sqmtr, col = "darkred", lwd = 2)

## Homogeneity of variances


#Boxplot
ggplot(data = rent_index, aes(y = rent_per_sqmtr, x = factor(quality.of.location))) +
  stat_boxplot(alpha=0.6, size=0.7, outlier.colour = "Black", width = 0.5,) +
  ylab("rent per square meter ") +
  xlab("location quality")
theme(axis.title.x = element_text(colour="Black", size=12, face = "bold"),
      axis.title.y = element_text(colour="Black", size=12, face = "bold"),
      axis.text.x = element_text(size=12, face = "bold"),
      axis.text.y = element_text(size=12, face = "bold"),
      legend.title = element_text(size=12, face = "bold"),
      legend.text = element_text(size=10))

#Variance of all location qualities
var(rent_per_sqmtr_qloc_1$rent_per_sqmtr)
var(rent_per_sqmtr_qloc_2$rent_per_sqmtr)
var(rent_per_sqmtr_qloc_3$rent_per_sqmtr)

#Homogeneity of variance assumption is violated so we will use Kruskal-Wallis test(non parametric test)
 kruskal.test(rent_per_sqmtr ~ quality.of.location, data = rent_index)
# p-value less than significant value so we will reject null hypothesis

#Tasks2 b)Pairwise difference

pairwise.wilcox.test(rent_index$rent_per_sqmtr,rent_index$quality.of.location, p.adjust.method = 'none' )
pairwise.wilcox.test(rent_index$rent_per_sqmtr,rent_index$quality.of.location, p.adjust.method = 'bonf' )

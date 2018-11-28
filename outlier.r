




#---------------------------------------------------------------------------------------------------
#
# TCASH Cash Out (Amount vs Count) Transaction Outlier Detection with Mahalanobis Distance
#
#---------------------------------------------------------------------------------------------------


# Load libraries


library(ggplot2)
library(dplyr)
library(tidyverse)
library(e1071)

#wd <- "/home/rickygf/NBBRA/OutlierDetection"
wd <- "D:/RA-FILES/Wholesale and Broadband Revenue Assurance/03New Business/TCASH/05_Current_Alert/SERVER_TCASH_ANALYTIC/OutlierDetection/"
setwd(wd)


# Set color palette
cbPalette <- c("#999999", "#4288D5", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Example Data

df_file <- read.csv("tdr7001_parsed.csv",colClasses=c(rep("character",15)))     

df <- df_file %>% 
  select(msg.CreditMSISDN, msg.DebitShortCode,msg.PrincipleTransactionAmount) %>% 
  mutate(msg.PrincipleTransactionAmount=as.numeric(msg.PrincipleTransactionAmount)/100) %>%
  filter(!is.na(msg.PrincipleTransactionAmount)) %>%
  filter(msg.PrincipleTransactionAmount < 22000000) %>%
  filter(grepl("^[[:digit:]]+$", msg.DebitShortCode)) %>%  #ambil yang numeric saja
  #filter(!grepl("^[[:digit:]]+$", msg.DebitShortCode)) %>%  #ambil yang mengandung alphabet saja
  group_by (msg.DebitShortCode) %>%
  summarise(TOTAL_AMOUNT_DEBET=sum(msg.PrincipleTransactionAmount), COUNT_TRX = n())
colnames(df) <- c("ACC_NO","TOTAL_AMOUNT_DEBET","COUNT_TRX")
df <- as.data.frame(df)

skala_amount_debet <- 1000000
max_x <- max(df$TOTAL_AMOUNT_DEBET)/skala_amount_debet 
max_y <- max(df$COUNT_TRX) 

for (i in seq(10,1000000,10)) {
  if (max_x%%i==max_x){
    interval_x <- i/10
    break
  }
}
for (i in seq(10,1000000,10)) {
  if (max_y%%i==max_y){
    interval_y <- i/10
    break
  }
}


#=====================Outlier Detection Statistical Learning based=============

# Calculate Mahalanobis
m_dist <- mahalanobis(df[, 2:3], colMeans(df[, 2:3]), cov(df[, 2:3])) 
df$m_dist <- round(m_dist, 2)

if(FALSE){
  # Mahalanobis Distance Histogram
  ggplot(df, aes(x = m_dist)) +
    geom_histogram(bins = 50) +
    labs(title = "Mahalanobis Distances",
         subtitle = "Histogram based on Mahalanobis Distances for Amount - Count",
         caption = "Source: TCASH Trx Log 2018-09-26") +
    xlab("Mahalanobis Distance") +
    scale_y_continuous(breaks = seq(0, 1, 0.01))
  
  hist(m_dist,xlab = "Weight",col = "green",border = "red", xlim =c(0,0.1),ylim = c(0,500),
       breaks = 750000)
  
  # Mahalanobis Distance boxplot
  boxplot(m_dist,data=df, main="Mahalanobis Distances", 
          xlab="Mahalanobis Distance", ylab="Mahalanobis Distance")
}

#Define threshold
m_dist_mean <- mean(m_dist)
m_dist_median <- median(m_dist)
ratio_mean_median <- m_dist_mean/m_dist_median
scores<-apply(as.data.frame(m_dist),2,scale)
colnames(scores) <-c("zscore")
m_dist_score <- cbind(df,scores)
m_dist_score <- m_dist_score[with(m_dist_score, order(zscore)), ]
max_zscore <- max(m_dist_score$zscore)
m_dist_score_treshold <- m_dist_score[m_dist_score$zscore >=(3*(max_zscore/ratio_mean_median)),]
threshold <- m_dist_score_treshold$m_dist[[1]]

# Maha Outliers
df$outlier_maha <- "No"
df$outlier_maha[df$m_dist >= threshold] <- "Yes"


# Scatterplot with Maha Outliers
myplot <- ggplot(df, aes(x = TOTAL_AMOUNT_DEBET/skala_amount_debet, y = COUNT_TRX, color = outlier_maha)) +
  geom_point(size = 5, alpha = 0.6) +
  labs(title = "Amount - Count Scatterplot",
       subtitle = "Outlier Detection in Amount - Count data - Using Mahalanobis Distances",
       caption = "Source: TCASH Trx Log") +
  ylab("Total Count trx-out") + xlab("Total Amount Debit in Rp Million") +
  scale_y_continuous(breaks = seq(0, max_y, interval_y)) +
  scale_x_continuous(breaks = seq(0, max_x+(2*interval_x), interval_x),limits=c(0,max_x+(2*interval_x)),labels = seq(0, max_x+(2*interval_x), interval_x)) +
  scale_colour_manual(values=cbPalette) +
  #annotate("text", x = (df$TOTAL_AMOUNT_DEBET/skala_amount_debet), y=df$COUNT_TRX, label = df$ACC_NO, hjust = -0.1, colour = "red")
  annotate("text", x = (df[df$outlier_maha=="Yes",]$TOTAL_AMOUNT_DEBET)/skala_amount_debet, y=(df[df$outlier_maha=="Yes",]$COUNT_TRX), label = (df[df$outlier_maha=="Yes",]$ACC_NO), hjust = -0.1, colour = "red",size = 3)

ggsave("myplot.png")
#dev.off()

# Outliers print
outlier <- df[df$outlier_maha=="Yes",]

write.table(outlier,"outlier.csv",col.names = T, row.names = F, sep="'")



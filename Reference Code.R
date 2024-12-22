# Create bar plot
MBTAplot +
  geom_histogram(aes(Line, fill = Restriction_Reason), stat = "count", color = "black") +
  scale_y_continuous(breaks = seq(0, 30, 2)) +
  labs(title = "Restriction Reason For Each Line", y = "Count",
       subtitle = "Joshua Soriano") +
  guides(fill = guide_legend(title="Restriction Reason"))

# Extract Restriction Speed MPH columns
max_length <- max(c(length(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH), length(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)))

SRspeed <- data.frame(col1 = c(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH, rep(NA, max_length - length(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH))), 
                      col2 = c(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH, 
                               rep(NA, max_length - length(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH))))

# Unlist the data frame and omit NA values
SRspeed <- as.data.frame(unlist(SRspeed))
SRspeed <- na.omit(SRspeed)

# Rename column to Restriction Speed MPH
colnames(SRspeed)[1] = "Restriction_Speed_MPH"

# Create objectid column
SRspeed$ObjectID <- seq(1, 238)

# Create new plot for SR speed for Jan and Feb
MBTAtotalplot <- ggplot(SRspeed)

# Create linear regression model
MBTAtotalplot + geom_point(aes(ObjectID, Restriction_Speed_MPH))

# Original Research Questions
# 1.	What is the total restriction distance for each line? What is the primary restriction reason for each line? How many speed restrictions were put in place due to track wear and tear? 
# 2.	Has the average restriction distance decreased between January and February? What about restriction speed? (Hypothesis testing)
# 3.	Can we construct a reliable linear regression model that displays the correlation between the speed restriction values for both data sets? (Modeling)
# 4.	Does the total track length have any noticeable impact on how the speed restriction is determined? (Modeling)
# 5.	Is there a difference in the average maximum restriction speed applied to each branch? (Chi-square test)
# 6.	Are there more speed restrictions being applied than there are ending?
# 7.	What is the average speed restriction duration? Has it changed between the two months? (Hypothesis testing)
# 8.	Which track lines have the most impact on speed restriction distance? (ANOVA)

# This section of code was originally used for barplots of total restriction speed per month. It was discarded due to 
# lack of conciseness.

#load packages
library(ggplot2)
library(scales)

# base plots
Jan.plot <- ggplot(JanSR)
Feb.plot <- ggplot(FebSR)
Mar.plot <- ggplot(MarSR)
Apr.plot <- ggplot(AprSR)
All.plot <- ggplot(AllSR)

# Create bar plot, summing total restriction distance in miles for January
Jan.plot +
  geom_histogram(aes(factor(Branch), y = Restriction_Distance_Miles), stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks = seq(0, 300, 10)) +
  xlab("Branch") +
  ylab("Total Restriction Distance in Miles") +
  labs(title = "January")

# Create bar plot, summing total track distance in miles for February
Feb.plot +
  geom_histogram(aes(factor(Branch), y = Restriction_Distance_Miles), stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks = seq(0, 300, 10)) +
  xlab("Branch") +
  ylab("Total Restriction Distance in Miles") +
  labs(title = "February")

# Create bar plot, summing total track distance in miles for March
Mar.plot +
  geom_histogram(aes(factor(Branch), y = Restriction_Distance_Miles), stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks = seq(0, 300, 20)) +
  xlab("Branch") +
  ylab("Total Restriction Distance in Miles") +
  labs(title = "March")

# Create bar plot, summing total track distance in miles for April
Apr.plot +
  geom_histogram(aes(factor(Branch), y = Restriction_Distance_Miles), stat = "identity") +
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks = seq(0, 300, 20)) +
  xlab("Branch") +
  ylab("Total Restriction Distance in Miles") +
  labs(title = "April")

MBTA_Rapid_Transit_Speed_Restrictions <- read_csv("MBTA_Rapid_Transit_Speed_Restrictions.csv")
View(MBTA_Rapid_Transit_Speed_Restrictions)
MBTA_Rapid_Transit_Speed_Restrictions_February_2023 <- read_csv("MBTA_Rapid_Transit_Speed_Restrictions_February_2023.csv")
View(MBTA_Rapid_Transit_Speed_Restrictions_February_2023)

MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH <- gsub(" mph","", as.character(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH)) %>%
  as.numeric(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH)
MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH <- gsub(" mph","", as.character(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)) %>%
  as.numeric(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)


z.test(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH, 
       MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH, "two.sided", 
       sigma.x=sd(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH), 
       sigma.y = sd(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH))

z.test(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH, 
       MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)

SR.mean.1 <- mean(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH)
SR.mean.2 <- mean(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)
sigma_sq_1 <- sd(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Speed_MPH)**2
sigma_sq_2 <- sd(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Speed_MPH)**2

z_stat <- (SR.mean.1 - SR.mean.2) / 
  sqrt(sigma_sq_1 / 70 + sigma_sq_2 / 168)

Z <- Normal(0, 1)  # make a standard normal r.v.
1 - cdf(Z, abs(z_stat)) + cdf(Z, -abs(z_stat))

# 3.1 Original Regression Analysis

# Extract Restriction Distance MPH columns
max_length <- max(c(length(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Distance_Miles), length(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Distance_Miles)))

SRdistance <- data.frame(col1 = c(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Distance_Miles, rep(NA, max_length - length(MBTA_Rapid_Transit_Speed_Restrictions$Restriction_Distance_Miles))), 
                         col2 = c(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Distance_Miles, 
                                  rep(NA, max_length - length(MBTA_Rapid_Transit_Speed_Restrictions_February_2023$Restriction_Distance_Miles))))

# Unlist the data frame and omit NA values
SRdistance <- as.data.frame(unlist(SRdistance))
SRdistance <- na.omit(SRdistance)

# Rename column to Restriction Speed MPH
colnames(SRdistance)[1] = "Restriction_Distance_Miles"

# Create objectid column
SRdistance$ObjectID <- seq(1, 238)

# Create new plot for SR speed for Jan and Feb
MBTAtotalplot <- ggplot(SRdistance)

# Create linear regression model
MBTAtotalplot + geom_point(aes(ObjectID, Restriction_Distance_Miles)) +
  geom_smooth(aes(ObjectID, Restriction_Distance_Miles), method = "lm", formula = y~x) +
  xlab("Track Section ID") +
  ylab("Restriction Distance in Miles") +
  scale_x_continuous(breaks = seq(0., 300, 25))

# Create log of restriction distance
AllSR$log.SR.dist <- log10(AllSR$Restriction_Distance_Miles)

ggplot(AllSR) + 
  geom_point(aes(Restriction_Speed_MPH, log.SR.dist)) +
  geom_smooth(aes(Restriction_Distance_Miles, log.SR.dist), method = "lm", formula = y~x) +
  xlab("Restriction Speed (mph)") +
  ylab("Log of Restriction Distance in Miles")

# sqrt restriction distance
AllSR$sqrt.SR.dist <- sqrt(AllSR$Restriction_Distance_Miles)

ggplot(AllSR) + 
  geom_point(aes(Restriction_Speed_MPH, sqrt.SR.dist)) +
  geom_smooth(aes(Restriction_Distance_Miles, sqrt.SR.dist), method = "lm", formula = y~x) +
  xlab("Restriction Speed (mph)") +
  ylab("Log of Restriction Distance in Miles")

ggplot(AllSR) + 
  geom_point(aes(Restriction_Speed_MPH, sqrt.SR.dist)) +
  geom_smooth(aes(Restriction_Distance_Miles, log.SR), method = "lm", formula = y~x) +
  xlab("Restriction Speed (mph)") +
  ylab("Square Root of Restriction Distance in Miles")

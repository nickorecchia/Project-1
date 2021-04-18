#######Project 1#####
#####Basketball Player Performance#######
######Austin Lacey, David Woo, Nick Orecchia####

#Loading the dataset into r studio
df<-read.csv('https://raw.githubusercontent.com/nickorecchia/Project-1/main/summary%20(cleaned).csv') 
dim(df) #getting the dimensions of the data frame
View(df) #viewing the data in r studio
install.packages('ggplot2')  #installing ggplot package 
library(ggplot2) #loading ggplot library 

#summary of the dataset: Min,1st quart., median, mean, 3rd qurt., max
summary(df)

#histograms for specific variables get an idea of what they look like and frequency 
hist(df$GamesPlayed)
hist(df$MinutesPlayed)
hist(df$PointsPerGame)
hist(df$FieldGoalsMade)
hist(df$FieldGoalsAttempt)
hist(df$X3PointMade)
hist(df$X3PointAttempt)
hist(df$FreeThrowMade)
hist(df$FreeThrowAttempt)
hist(df$OffensiveRebounds)
hist(df$DefensiveRebounds)
hist(df$Rebounds)
hist(df$Assists)
hist(df$Steals)
hist(df$Turnovers)


####understand the different correlations between variables### 
cor(df[,3:20])

#####scatterplots to visualize the correlations from last line of code###
plot(df$MinutesPlayed, df$PointsPerGame)

plot(df$MinutesPlayed, df$FieldGoalPercent)

plot(df$MinutesPlayed, df$X3PointPercent)

plot(df$MinutesPlayed, df$FreeThrowPercent)

plot(df$MinutesPlayed, df$Rebounds)

plot(df$MinutesPlayed, df$Assists)

plot(df$MinutesPlayed, df$Steals)

plot(df$MinutesPlayed, df$Blocks)

plot(df$MinutesPlayed, df$Turnovers)


###############
#PIE CHART depicting the number of players who are either 1 or zero for target variable
###Career longer than five years or less####


####creating the data frame###
df2<- data.frame(
  CareerLength=c("< 5 yrs","> 5 yrs"),
  value=c(487,807)
)

#run below line to change from numeric to a factor
#This is needed so we don't have that (0,0.25, 0.5, etc) scale on side of chart
df2$CareerLength<-as.factor(df2$CareerLength)
class(df2$CareerLength)

#constructing the pie chart 
ggplot(df2, aes(x="", y= value, fill=CareerLength)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  ggtitle('Players Career Length') + 
  theme(plot.title = element_text(hjust = .5)) +
  scale_fill_discrete(name = "Career Length") +
  labs(x=NULL , y= NULL)   #gets rid of x and y axis 
  

#This is to changes back to numeric 
df2$CareerLength<-as.numeric(df2$CareerLength)
class(df$CareerLength)

####Frequency poly graphs#####
####Frequency of major stats compared across the two target groups####

###Field Goals Made####
ggplot(df, aes(FieldGoalsMade)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Field Goals Made') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Field Goals Made')+
  ylab('Number of Players') 

####Points per Game####
ggplot(df, aes(PointsPerGame)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Points Per Game') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Points Per Game')+
  ylab('Number of Players') 

###Rebounds####
ggplot(df, aes(Rebounds)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Rebounds') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Rebounds')+
  ylab('Number of Players') 


#####Assists####
ggplot(df, aes(Assists)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Assists') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Assists')+
  ylab('Number of Players') 


####Steals####
ggplot(df, aes(Steals)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Steals') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Steals')+
  ylab('Number of Players') 

###Blocks####
ggplot(df, aes(Blocks)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Blocks') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Blocks')+
  ylab('Number of Players') 

####Turnovers####
ggplot(df, aes(Turnovers)) +
  geom_freqpoly(binwidth = .2) +
  facet_wrap(~Target, ncol=1)+
  ggtitle('Frequency of Turnovers') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Turnovers')+
  ylab('Number of Players') 

#######Bar charts######
#CREATE SYNTHETIC DATA for Offensive Statistics#
####calculated average of statistics in excel###
df1 <- data.frame(
  x = c("Points","Offensive Rebounds","Assists","Field Goals"),
  y = c(6.8, 1.0, 1.6, 2.6),
  label = c("a", "b", "c", "d")
)

###viewing synthetic data frame
View(df1)

###creating the graph
p <- ggplot(df1, aes(x, y, label = label)) +
  labs(x = "Statistics",y=  "Averages") + 
  theme(plot.title = element_text(size = 12, hjust = .5) ) 

p + geom_bar(stat = "identity", fill="cadetblue4") + ggtitle("Averages of Offensive Statistics") 



#CREATE SYNTHETIC DATA for Defensive Statistics#
####calculated average of statistics in excel###
df3 <- data.frame(
  x = c("Steals","Defensive Rebounds","Blocks"),
  y = c(.62, 2.0 , .37),
  label = c("a", "b", "c")
)

###viewing synthetic data frame
View(df3)

###creating the graph
p <- ggplot(df3, aes(x, y, label = label)) +
  labs(x = "Statistics",y=  "Averages") + 
  theme(plot.title = element_text(size = 12, hjust = .5) ) 

p + geom_bar(stat = "identity", fill="cadetblue4") + ggtitle("Averages of Defensive Statistics") 


###############Scatter Plot ################ 
####Depicting correlations between major statistical variables###
####with minutes played and then comparing it across two target groups####

#run below line to change from numeric to a factor
#This is needed so we don't have that (0,0.25, 0.5, etc) scale on side of chart
df$Target<-as.factor(df$Target)
class(df$Target)

ggplot(df, aes(MinutesPlayed, PointsPerGame, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Points per Game') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Points per Game')

ggplot(df, aes(MinutesPlayed, FieldGoalPercent, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Field Goal Percentage') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Field Goal Percentage')



ggplot(df, aes(MinutesPlayed, FreeThrowPercent, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Free Throw Percentage') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Free Throw Percentage')


ggplot(df, aes(MinutesPlayed, Rebounds, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Rebounds') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Rebounds')


ggplot(df, aes(MinutesPlayed, Assists, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Assists') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Assists')


ggplot(df, aes(MinutesPlayed, Steals, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Steals') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Steals')

ggplot(df, aes(MinutesPlayed, Blocks, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Blocks') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Blocks')


ggplot(df, aes(MinutesPlayed, Turnovers, color= Target)) + 
  geom_point() +
  facet_wrap(~Target)+
  geom_smooth(col="azure1") +
  ggtitle('Scatter Plot: Minutes Played & Turnovers') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Turnovers')

###############Box and Whisker############ 
##looking at major stat categories and their distribution###
## central value and variability across both target groups##

####Points per game####
ggplot(df, aes(MinutesPlayed, PointsPerGame)) + geom_boxplot(fill='cadetblue3') +
  facet_wrap(~Target) +
  ggtitle('Box & Whisker Plot: Points per Game & Minutes Played') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Points per Game') 

##Rebounds####
ggplot(df, aes(MinutesPlayed, Rebounds)) + geom_boxplot(fill='cadetblue3') +
  facet_wrap(~Target) +
  ggtitle('Box & Whisker Plot: Rebounds & Minutes Played') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Rebounds') 


###Assists#####
ggplot(df, aes(MinutesPlayed, Assists)) + geom_boxplot(fill='cadetblue3') +
  facet_wrap(~Target) +
  ggtitle('Box & Whisker Plot: Assists & Minutes Played') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Assists') 


#####Blocks####
ggplot(df, aes(MinutesPlayed, Blocks)) + geom_boxplot(fill='cadetblue3') +
  facet_wrap(~Target) +
  ggtitle('Box & Whisker Plot: Blocks & Minutes Played') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Blocks') 


#####Turnovers####
ggplot(df, aes(MinutesPlayed, Turnovers)) + geom_boxplot(fill='cadetblue3') +
  facet_wrap(~Target) +
  ggtitle('Box & Whisker Plot:Turnovers & Minutes Played') + 
  theme(plot.title = element_text(hjust = .5))+ 
  xlab('Minutes Played')+
  ylab('Turnovers')



#####Histogram#####
library(plyr) #loads plyr library

#run below line to change from numeric to a factor
#This is needed so we don't have that (0,0.25, 0.5, etc) scale on side of chart
df$Target<-as.factor(df$Target)
class(df$Target)

#This is to change to numeric if needed, for graph keep Target as a factor variable 
df$Target<-as.numeric(df$Target)
class(df$Target)

# Have the Target variable in factor format . We have 2 Histograms in 1 row.
# This seems to be a faster way to get this chart put together
# Below allows us to custimize it more
ggplot(df, aes(MinutesPlayed, fill = Target)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth=.4) +
  facet_wrap(~Target, nrow = 1)

###########################################################################

###Layering the histogram#####
# stating to build up the plots. This is going to be the empty canvas for us
# to Build the layers on
plot_a<-ggplot(df, aes(x=MinutesPlayed))
plot_a

# This step creates the histogram all on one chart with MinutesPlayed
# also through this I have been adjust the binwidth smaller significantly
plot_b<-ggplot(df, aes(x=MinutesPlayed)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth=.3)
plot_b

# Showing the nonparametric density curve, Definitely showing a skew
plot_c<-ggplot(df,aes(x=MinutesPlayed)) +
  geom_density()
plot_c

# Next we are adding a vertical line that show the exact mean.
# This alone only adds the vline to an empty chart with mintues played below
plot_d<-ggplot(df, aes(MinutesPlayed)) +
  geom_vline(xintercept = mean(df$MinutesPlayed))
plot_d

# Now it time to add all of these together
plot_e<-ggplot(df,aes(x=MinutesPlayed)) +
  geom_histogram(aes(y=..density..), position = "identity", binwidth=.3) +
  geom_density() +
  geom_vline(xintercept = mean(df$MinutesPlayed))
plot_e

# Overlayed the histograms adding colored by the MinutesPlayed variable
plot_f<-ggplot(df,aes(x=MinutesPlayed, fill = Target)) + 
  geom_histogram(aes(y=..density..), position = "identity", binwidth=.3) 
plot_f

# Next we add the nonparametic density curve on both the histogram
# on the same chart/ assuming this may get a bit bunched
# Alpha controls the degree of transparency on the density curve
plot_g<-plot_f + geom_density(alpha=.5)  
plot_g

# Creating a vector for the means of each of our groups
# gives the mean for MinutesPlayed by target
### MUST INSTALL THE dplyr TOOL ###
# once adding code to RStudio with dplyr written in, should notify on
# to left of screen to install
means <- ddply(df, "Target", summarise, meanMinutesPlayed=mean(MinutesPlayed))
means

# Adds the vline at the mean for each distribution
plot_h<-plot_g +
  geom_vline(data=means, aes(xintercept=meanMinutesPlayed,  col=factor(Target)), linetype="dashed", size=1, show.legend = FALSE)
plot_h

# Creating cleaner axis labels
plot_i<-plot_h + 
  xlab('Minutes Played') +  #x-axis label
  ylab('Probability Density') #y-axis label
plot_i

# Cleaning up the scale for the axis/ Went with 2 sd to left of mean bc 3 were irrelevent
# so close to zero just pushed the chart way to the right. Also 0 and 0 doesn't leave much room for outliers
plot_j<-plot_i +
  ylim(0,0.125) + #controls y-axis scale#
  xlim(mean(df$MinutesPlayed)-2*sd(df$MinutesPlayed), mean(df$MinutesPlayed)+3*sd(df$MinutesPlayed)) #controls x-axis scale
plot_j

# Adding the title
plot_k<-plot_j +
  ggtitle('Comparison of Minutes Played to Career Length') 
plot_k

# Centering the title
plot_l<-plot_k +
  ggtitle('Comparison of Minutes Played to Career Length') +
  theme(plot.title = element_text(hjust = .5))
plot_l

# Labeling the legend
plot_m<-plot_l + 
  scale_fill_discrete(name = "Career Length", labels = c("< 5", "> 5"))
plot_m
















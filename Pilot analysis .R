shhh <- suppressPackageStartupMessages 
install.packages("tidyr", dependencies = TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("ggplot2", dependencies = TRUE)
install.packages("gridExtra", dependencies = TRUE)
install.packages("MASS", dependencies = TRUE)
install.packages("Matrix", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
install.packages("ape", dependencies = TRUE)
install.packages("vegan", dependencies = TRUE)
install.packages("RColorBrewer", dependencies = TRUE)
install.packages("cocor", dependencies = TRUE)
install.packages("DescTools", dependencies = TRUE)
install.packages("reshape2", dependencies = TRUE)
install.packages("grid", dependencies = TRUE)
install.packages("ggplotify", dependencies = TRUE)
install.packages("stringr", dependencies = TRUE)
install.packages("rgl", dependencies = TRUE)
install.packages("tidyverse", dependencies = TRUE)
install.packages("plot.matrix", dependencies = TRUE)
install.packages("farver", dependencies = TRUE)
install.packages("matrixStats", dependencies = TRUE)
install.packages("readxl", dependencies = TRUE)


{library(tidyr)
  library(plyr)
  library(dplyr) %>% shhh
  library(ggplot2) 
  library(gridExtra) %>% shhh
  library(MASS) %>% shhh
  library(Matrix) %>% shhh
  library(reshape2)%>% shhh
  library(ape) %>% shhh # stats
  library(vegan) %>% shhh # stats
  library(RColorBrewer)
  library(cocor)
  library(DescTools) %>% shhh
  library(reshape2) 
  library(grid)
  library(ggplotify)
  #library(ggpubr) %>% shhh
  #library(ggthemes)
  library(stringr) 
  library(rgl)
  library(tidyverse)
  library(plot.matrix) %>% shhh
  library(farver)
  library(matrixStats) %>% shhh
  #library(plotrix) %>% shhh
  #library(JWileymisc)
  library(readxl)
  #library(visreg)
}


# for MDS visualisation
Sys.setenv(RGL_USE_NULL = TRUE)
library(webshot2)

library(purrr)
library(stringi)


# load data
data <- read_excel("Bella _color_similarity_master_2023-04-19_10h22.10.426.xlsx", col_names = TRUE)


# calculate number of unique participants in dataset
n.participants <- function(df){
  return(length(unique(df$participant)))
}
n.participants(data)

#te`st`

# catch scores
get_catchscoredecimal<- function(z){ 
  temp_df <- subset(catchdata,participant==z)
  total_catch <- nrow(temp_df)
  correct_catch<- nrow(subset(temp_df,catchnumber==catchresponse))
  catchscoredecimal<-format(round(correct_catch/total_catch, 2), nsmall = 2)
  catchscoredecimal<-as.numeric(catchscoredecimal)
  return(catchscoredecimal)
}

# scoring participants

#MAKE CATCHDATA DATAFRAME WITH ONE ROW PER CATCH TRIAL, FOR ALL PARTICIPANTS
#select catch variables for analysis
catch_vars<- c("participant", "catchnumber", "catchresponse", "response_time_catch")

#make catch data frame
catchdata<-data[data$Catch==1, catch_vars]

#remove empty cells
catchdata <- catchdata %>% drop_na()

catch_perperson<- data.frame(unique(catchdata$participant))
catch_perperson<-rename(catch_perperson, participant = unique.catchdata.participant.)
catch_perperson$score_decimal<-lapply(catch_perperson$participant, get_catchscoredecimal)
catch_perperson$score_decimal <- as.numeric(catch_perperson$score_decimal)

# remove participants with low scores
acceptable.catch <- subset(catch_perperson, score_decimal>=0.77)

hist(catch_perperson$score_decimal)


# remove participants with low catch scores
data <- subset(data, participant %in% acceptable.catch$participant)

n.participants(data)

# rescale data to dissimilarity
rescale <- function(trialdata){
    outdata <- trialdata
  outdata$similarity[trialdata$similarity == 4] <-  0
  outdata$similarity[trialdata$similarity == 3] <-  1
  outdata$similarity[trialdata$similarity == 2] <-  2
  outdata$similarity[trialdata$similarity == 1] <-  3
  outdata$similarity[trialdata$similarity == -1] <- 4
  outdata$similarity[trialdata$similarity == -2] <- 5
  outdata$similarity[trialdata$similarity == -3] <- 6
  outdata$similarity[trialdata$similarity == -4] <- 7
    return(outdata)
}

#double pass correlations

trialdata <- rescale(data)
#nrow(trialdata)
#head(trialdata$Temporder)
trialdata <- subset(trialdata, !is.na(main_trial.thisN))
#nrow(trialdata)


sort(colnames(trialdata))


# calculate the double-pass correlation for a participant

double.cor <- function(similarity){
  first_pass <- similarity[1:99]
  second_pass <- similarity[100:198]
  return(cor(first_pass,second_pass))
}

# get a df of participant double-pass correlations
double.cor.summary <- trialdata %>% group_by(participant) %>% summarise(correlation = double.cor(similarity))
#head(double.cor.summary)
double.cor.acceptable <- subset(double.cor.summary, correlation >= 0.5)

# plot the double-pass correlations
cor.plot <- ggplot(data = double.cor.summary, aes(y = correlation,x=1)) +
  geom_boxplot(width=0.2) + geom_jitter(width=0.1) +
  xlab("") + xlim(0.5,1.5) + ylim(-1,1) + geom_hline(yintercept = 0, colour = 'blue', linetype = 'dashed')
cor.plot

# exclude participants with double-pass correlations <0.5
# HAD PREVIOUSLY MADE A MISTAKE AND FAILED TO EXCLUDE PARTICIPANTS ON THE BASIS OF THIS

data <- subset(data, participant %in% double.cor.acceptable$participant)

#regenerating trialdata
trialdata <- subset(data, participant %in% double.cor.acceptable$participant)
trialdata <- subset(trialdata, !is.na(main_trial.thisN)) # get rid of extraneous rows



#convert RGB to HEX for plotting
#split rgb strings
#x=rgb string
splitrgb <-function(x){ 
  variable <- str_replace_all(x, "\\[|\\]", "") #delete brackets
  variable<-strsplit(variable, split = ",") #split string
  variable<-lapply(variable, as.numeric)   #convert to numeric
  variable<-lapply(variable, "+", 1)  #convert rgb scale 
  variable<-lapply(variable, "*", 127.5)
  #return (as.list(unlist(variable)))
  return(variable)
}

trialdata$Circle1_colour <- lapply(trialdata[,"Circle1_colour"], splitrgb) #apply function
trialdata$Circle2_colour <- lapply(trialdata[,"Circle2_colour"], splitrgb)


#make dataframes with rows per r, g, and b
circle1_df <- as.data.frame(lapply(trialdata[,"Circle1_colour"], function(x) t(do.call(cbind, x))))
circle2_df <- as.data.frame(lapply(trialdata[,"Circle2_colour"], function(x) t(do.call(cbind, x))))

#swap rows and columns
circle1_df <- t(circle1_df)
circle2_df <- t(circle2_df)

#name columns
colnames(circle1_df) <- c("r1", "g1", "b1")
colnames(circle2_df) <- c("r2", "g2", "b2")

circle1_df<-as.data.frame(circle1_df) 
circle2_df<-as.data.frame(circle2_df)

circle1_df$hex1 <- NA
circle2_df$hex2 <- NA

#rgb2hex function
rgb2hex <- function(r, g, b) {rgb(r, g, b, maxColorValue = 255)}
#Get hex in each circle dataframe
circle1_df$hex1 <- apply(circle1_df, 1, function(x) rgb2hex(x[1], x[2], x[3]))
circle2_df$hex2 <- apply(circle2_df, 1, function(x) rgb2hex(x[1], x[2], x[3]))

#bind circle colour dataframes to trialdata
trialdata <- cbind(trialdata, circle1_df, circle2_df)

# give each colour comparison a unique ID
realcomparison.label <- function(df,mode='hex'){
  realcomparison <- vector()
  for(row in 1:nrow(df)){
    if(mode=='hex'){
      val <- stri_paste(sort(c(df[row,]$hex1,df[row,]$hex2)),collapse='')
    } else if(mode=='colour'){
      val <- stri_paste(sort(c(df[row,]$firstColour,df[row,]$secondColour)),collapse='')
    } else if(mode=='color'){
      val <- stri_paste(sort(c(df[row,]$color1,df[row,]$color2)),collapse='')
    }
    
    realcomparison <- c(realcomparison,val)
  }
  df$realcomparison <- realcomparison
  return(df)
}

trialdata <- realcomparison.label(trialdata)

head(trialdata$realcomparison)


#matrix visualisation - construction 
# individual participant files use a unique ID to mark which colour pairs they saw
truthColourTableColumnNames <- c("r1", "g1", "b1", "r2", "g2", "b2")
truthColourTable <- read_xlsx("colourcodes.xlsx")
names(truthColourTable) <- truthColourTableColumnNames

# getting the colour pair comparison information

# compartmentalising the two dots presented to participants
firstColourSet <- truthColourTable[,truthColourTableColumnNames[1:3]]
secondColourSet <- truthColourTable[,truthColourTableColumnNames[4:6]]

names(firstColourSet) <- names(secondColourSet) <- c("r", "g", "b")

#conversion of 3 RGB columns in both colour tables to HEX for easier manipulation
firstColourSetHEX <- apply(firstColourSet, 1, function (x) rgb2hex(x[1], x[2], x[3]))
secondColourSetHEX <- apply(secondColourSet, 1, function (x) rgb2hex(x[1], x[2], x[3]))

# colourSetHEX is used as a reference throughout the rest of the analysis for the colour pair comparisons     
colourSetHEX <- data.frame(firstColour = firstColourSetHEX, secondColour = secondColourSetHEX)


# read in the colour comparison truth table
colourSetHEX <- data.frame(firstColour = firstColourSetHEX, secondColour = secondColourSetHEX)

rowsTotalSetHEX <- rbind(data.frame(colour = firstColourSetHEX), data.frame(colour = secondColourSetHEX)) %>% group_by_all %>% tally

# preliminary check that the data has been read in correctly
countOfUniqueRowsFirstSet <- length(unique(firstColourSetHEX))
countOfUniqueRowsSecondSet <- length(unique(secondColourSetHEX))
countOfUniqueRowsTotalSet <- length(rowsTotalSetHEX$colour)

# set lower triangle of matrix to NA - this gives heatmap its unique upper triangle
upperTriangularMatrix <- upper.tri(matrix(1, ncol = countOfUniqueRowsTotalSet, nrow = countOfUniqueRowsTotalSet), 
                                   diag = FALSE)
upperTriangularMatrix[upperTriangularMatrix == 1] <- NA
color.mat.df <- data.frame(upperTriangularMatrix)
color.mat <- upperTriangularMatrix


# setting all give colours as both row and column names
colnames(color.mat.df) <- rownames(color.mat.df) <- rowsTotalSetHEX$colour



group.mean <- trialdata %>% 
  group_by(realcomparison) %>% 
  summarise(across(similarity, mean, na.rm = TRUE))

group.var <- trialdata %>% 
  group_by(realcomparison) %>% 
  summarise(across(similarity, var, na.rm = TRUE))

head(group.mean)
head(group.var)

group.count <- trialdata %>% 
  group_by(realcomparison) %>% 
  summarise(similarity = n())
head(group.count)


# create a mean dissimilarity dataframe using realcomparison indexing
group.avg.df <- function(data, colors.df){
  means.df <- data %>%
    group_by(realcomparison) %>%
    summarise(across(dissimilarity, mean, na.rm = TRUE))
  # sorting 
  means.df$realcomparison <- as.numeric(means.df$realcomparison)
  # fill in NA values
  if(nrow(means.df) < 4371){
    full <- 1:4371
    incomplete <- means.df$realcomparison
    missing <- setdiff(full,incomplete)
    addon <- tibble(realcomparison = missing, dissimilarity = NA)
    means.df <- rbind(means.df,addon)
    
  }
  # get it in the right format for making a matrix
  means.df <- means.df[order(means.df$realcomparison),]
    colors.df$value <- means.df$dissimilarity
    return(colors.df)
}

gp.mean.data <- colourSetHEX
gp.mean.data <- realcomparison.label(gp.mean.data,mode='colour')
gp.mean.data$value <- NA



fill.gp.mean.data <- function(gp,means){
  for(row in 1:nrow(means)){
    line <- means[row,]
    real <- line$realcomparison
    dissim <- line$similarity
    if(real %in% gp$realcomparison){
      #print(row)
      gp$value[gp$realcomparison == real] <- dissim
    }
    #print(head(gp$realcomparison))
    #print(line)
    #print(real)
    #print(dissim)
  }
  return(gp)
}


gp.mean.data <- fill.gp.mean.data(gp.mean.data,group.mean)
gp.var.data <- fill.gp.mean.data(gp.mean.data,group.var)
gp.count.data <- fill.gp.mean.data(gp.mean.data,group.count)

# fill matrix
matrix.df.fill <- function(data,matrix.df){
  for(i in 1:nrow(data)){
    #print(i)
    row <- data[i,]
    #print(matrix.df[rowsecondColour])
    matrix.df[row$firstColour,row$secondColour] <- row$value 
    matrix.df[row$secondColour,row$firstColour] <- row$value # symmetric
    #print(matrix.df[rowsecondColour])
  }
  return(matrix.df)
}

group.mean.mat.df <- matrix.df.fill(gp.mean.data,color.mat.df)
group.var.mat.df <- matrix.df.fill(gp.var.data,color.mat.df)
group.n.mat.df <- matrix.df.fill(gp.count.data,color.mat.df)

# visualisations
group.mean.mat.df$color1 <- rowsTotalSetHEX$colour
group.mean.mat.df$color2 <- rowsTotalSetHEX$colour
    
group.var.mat.df$color1 <- rowsTotalSetHEX$colour
group.var.mat.df$color2 <- rowsTotalSetHEX$colour
    
group.n.mat.df$color1 <- rowsTotalSetHEX$colour
group.n.mat.df$color2 <- rowsTotalSetHEX$colour
  
group.mean.df <- gather(group.mean.mat.df, color1, mean,
    rowsTotalSetHEX$colour[1]:rowsTotalSetHEX$colour[nrow(rowsTotalSetHEX)],
    factor_key=TRUE)

group.var.df <- gather(group.var.mat.df, color1, var,
    rowsTotalSetHEX$colour[1]:rowsTotalSetHEX$colour[nrow(rowsTotalSetHEX)],
    factor_key=TRUE)

group.n.df <- gather(group.n.mat.df, color1, n,
    rowsTotalSetHEX$colour[1]:rowsTotalSetHEX$colour[nrow(rowsTotalSetHEX)],
    factor_key=TRUE)


# INSERT FACTORS STUFF
row.facs <- colourSetHEX$secondColour[1:93]

group.mean.df$color1 <- with(group.mean.df, factor(color1, levels = row.facs))
group.mean.df$color2 <- with(group.mean.df, factor(color2, levels = row.facs))

group.var.df$color1 <- with(group.var.df, factor(color1, levels = row.facs))
group.var.df$color2 <- with(group.var.df, factor(color2, levels = row.facs))

group.n.df$color1 <- with(group.n.df, factor(color1, levels = row.facs))
group.n.df$color2 <- with(group.n.df, factor(color2, levels = row.facs))

block <- paste(strrep("\U2588",10),sep='')
block_rep <- rep(block,93)


countOfUniqueRowsTotalSet <- length(rowsTotalSetHEX$colour)
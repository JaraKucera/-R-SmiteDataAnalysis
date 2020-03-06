#-----Libraries-----#
library(caret)
library(GGally)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(patchwork)
library(DescTools)
library(hexbin)
library(RColorBrewer)
library(ggpubr)
#-----Data-----#

OskiData <- read.csv(file="C:/Users/kucera/Documents/Code2/oski.csv", strip.white = T)
Winrate <- (OskiData$Wins/OskiData$Matches)*100
KD <- (OskiData$Kills/OskiData$Deaths)
KDA <- ((OskiData$Kills+OskiData$Assists)/OskiData$Deaths)
OskiData <- cbind(OskiData, Winrate, KD, KDA) 
OskiData[96, 8:10] <- 0
OskiData[86, 8:10] <- 0
OskiData[73, 8:10] <- 0
OskiData[34, 8:10] <- 0
GodDetails <- read.csv(file="C:/Users/kucera/Documents/Code2/gods.csv", sep="-")
OskiData <- cbind(OskiData, GodDetails[, 2:5])
OskiData$Pantheon <- as.character(OskiData$Pantheon)
OskiData$Type <- as.character(OskiData$Type)

JKData <- read.csv(file="C:/Users/kucera/Documents/Code2/smiteData.csv")
Winrate <- (JKData$Wins/JKData$Matches)*100
KD <- (JKData$Kills/JKData$Deaths)
KDA <- ((JKData$Kills+JKData$Assists)/JKData$Deaths)
JKData <- cbind(JKData, Winrate, KD, KDA)
JKData <- cbind(JKData, GodDetails[, 2:5])
as.character(JKData$Pantheon)
as.character(JKData$Type)
#-----Data Analysis OSKI-----#

#Development of KD as more matches are played
plot(x=OskiData$Matches, y=OskiData$KD, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KD against Matches",
     xlab = "Matches", ylab = "KDA"
)

#Development of KDA as more matches are played
plot(x=OskiData$Matches, y=OskiData$KDA, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KDA against Matches",
     xlab = "Matches", ylab = "KDA"
     )

#Development of KDA as more wins occur
plot(x=OskiData$Wins, y=OskiData$KDA, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KDA against Wins",
     xlab = "Wins", ylab = "KDA"
)

#KDA with reference to God Type
plot(x=OskiData$Type, y=OskiData$KDA, 
     cex=1, 
     col="#69b3a2",
     pch = 21, 
     main = "KDA with Reference to God Type",
     xlab = "Type", ylab = "KDA"
)

#KD with reference to God Type
plot(x=OskiData$Type, y=OskiData$KD, 
     cex=1, 
     col="#69b3a2",
     pch = 21, 
     main = "KD with Reference to God Type",
     xlab = "Type", ylab = "KD"
)

#Number of matches played
a1 <- ggplot(OskiData, aes(x=OskiData$Matches))+
        geom_histogram(binwidth = 20,fill="#69b3a2", color="#e9ecef", alpha=0.9)

#Correlation among Matches, wins, losses, kills, deaths, winrate, kd , kda
ggpairs(OskiData[, 2:10])
#Correlation 2 among Matches, wins, losses, kills, deaths, winrate, kd, kda
ggcorr(OskiData[,2:10], method = c("everything", "pearson"))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on type
ggpairs(OskiData[1:108, 2:10], ggplot2::aes(colour = OskiData$Type ))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on attack type
ggpairs(OskiData[1:108, 2:10], ggplot2::aes(colour = OskiData$AttackType ))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on class
ggpairs(OskiData[1:108, 2:10], ggplot2::aes(colour = OskiData$Class ))

#Correlation among winrate, kd, kda based on pantheon
ggpairs(OskiData[1:108, 8:10], ggplot2::aes(colour = OskiData$Pantheon ))

#Correlation among winrate, kd, kda based on pantheon
ggpairs(OskiData[1:108, 3:5], ggplot2::aes(colour = OskiData$Pantheon ))

ggpairs(
        OskiData[1:108,3:6],
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet")
)

#KDA development in wins over matches
ggplot(OskiData, aes(x=Matches, y=Wins, size = KDA, fill= Class))+
        geom_point(alpha=0.7, shape=21, color="black")+
        scale_size(range=c(.1,15), name="Matches(N)")+
        scale_fill_viridis(discrete = T, guide = F, option="A")+
        theme_ipsum()+
        theme(legend.position = "bottom")+
        ylab("Wins") +
        xlab("Matches")+
        theme(legend.position = "none")

#-----Data Analysis JK-----#

#Development of KD as more matches are played
plot(x=JKData$Matches, y=JKData$KD, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KD against Matches",
     xlab = "Matches", ylab = "KDA"
)

#Development of KDA as more matches are played
plot(x=JKData$Matches, y=JKData$KDA, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KDA against Matches",
     xlab = "Matches", ylab = "KDA"
)

#Development of KDA as more wins occur
plot(x=JKData$Wins, y=JKData$KDA, 
     cex=1, 
     col="#69b3a2", 
     pch = 16, 
     main = "Development of KDA against Wins",
     xlab = "Wins", ylab = "KDA"
)

#KDA with reference to God Type
plot(x=JKData$Type, y=JKData$KDA, 
     cex=1, 
     col="#69b3a2",
     pch = 21, 
     main = "KDA with Reference to God Type",
     xlab = "Type", ylab = "KDA"
)

#KD with reference to God Type
plot(x=JKData$Type, y=JKData$KD, 
     cex=1, 
     col="#69b3a2",
     pch = 21, 
     main = "KD with Reference to God Type",
     xlab = "Type", ylab = "KD"
)

#Number of matches played
a2 <- ggplot(JKData, aes(x=JKData$Matches))+
        geom_histogram(binwidth = 20,fill="#69b3a2", color="#e9ecef", alpha=0.9)

#Correlation among Matches, wins, losses, kills, deaths, winrate, kd , kda
ggpairs(JKData[, 2:10])
#Correlation 2 among Matches, wins, losses, kills, deaths, winrate, kd, kda
ggcorr(JKData[,2:10], method = c("everything", "pearson"))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on type
ggpairs(JKData[1:108, 2:10], ggplot2::aes(colour = JKData$Type ))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on attack type
ggpairs(JKData[1:108, 2:10], ggplot2::aes(colour = JKData$AttackType ))

#Correlation among matches, wins, losses, kills, deaths, winrate, kd, kda based on class
ggpairs(JKData[1:108, 2:10], ggplot2::aes(colour = JKData$Class ))

#Correlation among winrate, kd, kda based on pantheon
ggpairs(JKData[1:108, 8:10], ggplot2::aes(colour = JKData$Pantheon ))

#Correlation among winrate, kd, kda based on pantheon
ggpairs(JKData[1:108, 3:5], ggplot2::aes(colour = JKData$Pantheon ))

ggpairs(
        JKData[1:108,3:6],
        upper = list(continuous = "density", combo = "box_no_facet"),
        lower = list(continuous = "points", combo = "dot_no_facet")
)

#KDA development in wins over matches
ggplot(JKData, aes(x=Matches, y=Wins, size = KDA, fill= Class))+
        geom_point(alpha=0.7, shape=21, color="black")+
        scale_size(range=c(.1,15), name="Matches(N)")+
        scale_fill_viridis(discrete = T, guide = F, option="A")+
        theme_ipsum()+
        theme(legend.position = "bottom")+
        ylab("Wins") +
        xlab("Matches")+
        theme(legend.position = "none")

#-----Comparison-----#
theme_set(theme_pubr())
ggplot() +
        geom_line(data = OskiData, aes(x = Matches, y = Wins), color= "#6f03fc")+
        geom_line(data = JKData, aes(x = Matches, y = Wins), color = "#03fc90")

ggplot() +
        geom_line(data = OskiData, aes(x = Matches, y = Losses), color= "#6f03fc")+
        geom_line(data = JKData, aes(x = Matches, y = Losses), color = "#03fc90")

ggplot() +
        geom_line(data = OskiData, aes(x = OskiData$Class, y = Kills), color= "#6f03fc", size=2)+
        geom_line(data = JKData, aes(x = JKData$Class, y = Kills), color = "#03fc90", size=2)







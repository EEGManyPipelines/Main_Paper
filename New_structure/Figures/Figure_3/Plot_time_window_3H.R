rm(list = ls())

#load libraries
library(tidyverse)
library(ggplot2)
library(readxl)

##-- H1 --##
# Load data on the significant window length
h1 <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/results_resubmitted_h1_168.csv", sep=";")
timeDatasig <-setNames(data.frame(h1$team_identifier)  , "ID") 
timeDatasig$tw_start <- h1$time_window_start 
timeDatasig$tw_end <- h1$time_window_end
timeDatasig$sig <- h1$significance

# Load Analysis questionnaire data for the analysed time window
AQ <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Analysis questionnaire final sample 168 corrected.csv", sep=";")

all_time <- data.frame(ID = AQ$teamID, start = AQ$ans_temp_roi_tw_start_h1, end = AQ$ans_temp_roi_tw_end_h1) #set for each hypothesis

#correct syntax
all_time$end[all_time$end == "anterior: 150. posterior:173"] <- 173
all_time$end[all_time$end == "150ms"] <- 150
all_time$end[all_time$end == "200 ms"] <- 200
all_time$end[all_time$end == "125 ms fronto-central N1, 160 ms for parieto-occipital N1"] <- 160
all_time$end[all_time$end == "~140 ms"] <- 140
all_time$end[all_time$end == "150ms"] <- 150

all_time$end[all_time$end == "`500"] <- 500
all_time$end[all_time$end == "1.3"] <- 1300

all_time$start[all_time$start=="anterior: 100. posterior: 123"] <- 100
all_time$start[all_time$start=="100ms"] <- 100
all_time$start[all_time$start=="100 ms"] <- 100
all_time$start[all_time$start=="75 ms for fronto-central N1, 120 ms for parieto-occipital N1"] <- 75
all_time$start[all_time$start=="~100 ms"] <- 100
all_time$start[all_time$start=="50ms"] <- 50
all_time$start[all_time$start=="85ms"] <- 85
all_time$start[all_time$start=="-0.2"] <- -200

# Teams 112, 162 did not provide information on the significant time window
all_time <- all_time[-c(112,162),]

nonsense_val <- all_time[as.numeric(all_time$end) - as.numeric(all_time$start) < 0,] #when start or the window is higher than the end or has NA
all_time[31,2] <- all_time[31,3] 
all_time[31,3] <- 800
all_time$legend1 <- "analyzed"

#give order
all_time$wl <- as.numeric(all_time$end) - as.numeric(all_time$start)

all_time_orderered <- all_time %>%
  arrange(desc(wl))

all_time_orderered$order <- c(1:166)

#mark teams that found a significant effect
pos_res <- timeDatasig[timeDatasig$sig=='yes',]
pos_res$position <- 0
matchedIDs <- match(pos_res$ID, all_time_orderered$ID)
which(is.na(matchedIDs))
pos_res$ID[77] <- "d7303b34d38b2a18"
pos_res <- pos_res[-135,] # team that was identified and removed above from the time window specification

matchedIDs <- match(pos_res$ID, all_time_orderered$ID) # now all exists
which(is.na(matchedIDs))

pos_res$position <- matchedIDs
pos_res$legend2 <- rep('significant', 205) 

## --- PLOT -- ##
#test <- pos_res[order(pos_res$position),]
png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/time_window_plot_h1.png",
     units="in", width=6, height=4, res=200)

ggplot(all_time_orderered) + 
  geom_segment(aes(x=as.numeric(start), y=order, xend=as.numeric(end), yend=order, color =`legend1`), size=1.5) +
  geom_segment(data = pos_res, aes(x=tw_start, y=position, xend=tw_end,
                                   yend=position, color =`legend2`), size=1.5) +
  xlab("Time window (ms)") + ylab("Team")+ 
  theme_minimal()+
  theme(text = element_text(size=27), axis.text.y = element_blank(), legend.position="top")  +
  scale_color_manual(" ", values = c("#1f6bab","#40c473")) 

#scale_x_continuous(limits = c(min(as.numeric(all_time$start), na.rm = T), max(as.numeric(all_time$end), na.rm = T)))+
#scale_linetype_manual(" ", values=c("solid", "solid")) +

dev.off()


## -- H2 -- ##
# Load significant time window data for h2
h2a <-read.csv("/Users/ecesnaite/Downloads/Kopie von results_resubmitted_h1_2a_4a_168(H2a).csv", sep = ";")
timeDatasig <-setNames(data.frame(h2a$team_identifier)  , "ID") 
timeDatasig$tw_start <- h2a$time_window_start 
timeDatasig$tw_end <- h2a$time_window_end
timeDatasig$sig <- h2a$significance

AQ <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Analysis questionnaire final sample 168 corrected.csv", sep=";")

all_time <- data.frame(ID = AQ$teamID, start = AQ$ans_temp_roi_tw_start_h2a, end = AQ$ans_temp_roi_tw_end_h2a) #set for each hypothesis

#correct syntax
all_time$end[all_time$end == "500 ms"] <- 500
all_time$end[all_time$end == "500ms"] <- 500
all_time$end[all_time$end == "~ 422 ms "] <- 422
all_time$end[all_time$end == "+500"] <- 500
all_time$end[all_time$end == "0.5"] <- 500

all_time$start[all_time$start=="350370"] <- 350
all_time$start[all_time$start=="300 ms"] <- 300
all_time$start[all_time$start=="~380 ms"] <- 380
all_time$start[all_time$start=="0.3"] <- 300
all_time$start[all_time$start=="300ms"] <- 300

nonsense_val <- all_time[as.numeric(all_time$end) - as.numeric(all_time$start) < 0,] #when start or the window is higher than the end or has NA
all_time$legend1 <- "analyzed"

#give order
all_time$wl <- as.numeric(all_time$end) - as.numeric(all_time$start)

all_time_orderered <- all_time %>%
  arrange(desc(wl))

all_time_orderered$order <- c(1:168)

#mark teams that found a significant effect
pos_res <- timeDatasig[timeDatasig$sig=='yes',]
pos_res$position <- 0
matchedIDs <- match(pos_res$ID, all_time_orderered$ID)

which(is.na(matchedIDs))
pos_res[pos_res$ID == '#TeamKeele',1] <- "TeamKeele" # correct team IDs
pos_res$ID[376] <- "CIMHPipe"

matchedIDs <- match(pos_res$ID, all_time_orderered$ID) # now all exists
which(is.na(matchedIDs))

pos_res$position <- matchedIDs
pos_res$legend2 <- rep('significant', 406) 

#correct values
pos_res$tw_start <- as.numeric(gsub(",", ".", gsub("\\.", "", pos_res$tw_start)))
pos_res$tw_end <- as.numeric(gsub(",", ".", gsub("\\.", "", pos_res$tw_end)))

pos_res$tw_start[pos_res$tw_start=="350370"] <- 350

pos_res$tw_start[pos_res$ID=="8b7719c916df0670"]<- 312
pos_res$tw_end[pos_res$ID=="8b7719c916df0670"]<- 496

pos_res$tw_start[pos_res$ID=="Varuwa"] <- 300
pos_res$tw_end[pos_res$ID=="Varuwa"] <- 500

indx_nan <- which(is.na(pos_res$tw_end)) # teams that didn't indicate the end of a time window
pos_res <- pos_res[-indx_nan,]

## --- PLOT -- ##

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/time_window_plot_h2a.png", 
    units="in", width=6, height=4, res=200)

ggplot(all_time_orderered) + 
  geom_segment(aes(x=as.numeric(start), y=order, xend=as.numeric(end), yend=order, color =`legend1`), size=1.5) +
  geom_segment(data = pos_res, aes(x=tw_start, y=position, xend=tw_end,
                                   yend=position, color =`legend2`), size=1.5) +
   xlab("Time window (ms)") + ylab("Team")+ 
  theme_minimal()+
  theme(text = element_text(size=27), axis.text.y = element_blank(), legend.position="top")  +
  scale_color_manual(" ", values = c("#1f6bab","#40c473")) 

  #scale_x_continuous(limits = c(min(as.numeric(all_time$start), na.rm = T), max(as.numeric(all_time$end), na.rm = T)))+
  #scale_linetype_manual(" ", values=c("solid", "solid")) +

dev.off()

## -- H3a -- ##
#Load info on the significant time window
h3a <-read_excel("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/results_overview_h3a_corrected.xlsx", col_names = T)

timeDatasig <-setNames(data.frame(h3a$team_identifier)  , "ID") 
timeDatasig$tw_start <- h3a$time_window_start 
timeDatasig$tw_end <- h3a$time_window_end
timeDatasig$sig <- h3a$significance

AQ <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Analysis questionnaire final sample 168 corrected.csv", sep=";")

all_time <- data.frame(ID = AQ$teamID, start = AQ$ans_temp_roi_tw_start_h3a, end = AQ$ans_temp_roi_tw_end_h3a) #set for each hypothesis

#correct syntax
all_time$end[all_time$end == "500, 800"] <- 800
all_time$end[all_time$end == "500 ms"] <- 500
all_time$end[all_time$end == "1000 ms"] <- 1000
all_time$end[all_time$end == "100,200,300,400,500"] <- 500
all_time$end[all_time$end == "~ 563ms / ~ 876ms / ~1375ms"] <- 1375
all_time$end[all_time$end == "+500"] <- 500
all_time$end[all_time$end == "early time window (500), late time window (800ms)"] <- 800
all_time$end[all_time$end == "2000ms"] <- 2000
all_time$end[all_time$end == "550, 900, 650"] <- 900
all_time$end[all_time$end == "[...230] [...450] [...650] [...1000]"] <- 1000
all_time$end[all_time$end == "1.3"] <- 1300


all_time$start[all_time$start=="300, 500"] <- 300
all_time$start[all_time$start=="0 ms"] <- 0
all_time$start[all_time$start=="0,100,200,300,400"] <- 0
all_time$start[all_time$start=="~ 438ms / ~ 813ms / ~1250ms"] <- 438
all_time$start[all_time$start=="early time window (300), late time window (500 ms)"] <- 300
all_time$start[all_time$start=="500ms"] <- 500
all_time$start[all_time$start=="250, 500, 550"] <- 250
all_time$start[all_time$start=="[180...] [350...] [550...] [700...]"] <- 180
all_time$start[all_time$start=="-0.2"] <- -200


nonsense_val <- all_time[as.numeric(all_time$end) - as.numeric(all_time$start) < 0,] #when start or the window is higher than the end or has NA
all_time$legend1 <- "analyzed"

#give order
all_time$wl <- as.numeric(all_time$end) - as.numeric(all_time$start)

all_time_orderered <- all_time %>%
  arrange(desc(wl))

all_time_orderered$order <- c(1:168)

#identify teams where a significant effect was found
pos_res <- timeDatasig[timeDatasig$sig=='yes',]

indx_nan_h3a <- which(is.na(pos_res$ID))
pos_res <- pos_res[-indx_nan_h3a,]

pos_res$position <- 0
matchedIDs <- match(pos_res$ID, all_time_orderered$ID)

which(is.na(matchedIDs))

matchedIDs <- match(pos_res$ID, all_time_orderered$ID) 

pos_res$position <- matchedIDs
pos_res$legend2 <- rep('significant', 410) 

## --- PLOT -- ##

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/time_window_plot_h3a.png", 
    units="in", width=6, height=4, res=200)

ggplot(all_time_orderered) + 
  geom_segment(aes(x=as.numeric(start), y=order, xend=as.numeric(end), yend=order, color =`legend1`), size=1.5) +
  geom_segment(data = pos_res, aes(x=tw_start, y=position, xend=tw_end,
                                   yend=position, color =`legend2`), size=1.5) +
  xlab("Time window (ms)") + ylab("Team")+ 
  theme_minimal()+
  theme(text = element_text(size=27), axis.text.y = element_blank(), legend.position="top")  +
  scale_color_manual(" ", values = c("#2e1a6e","#c4580f")) 

#scale_x_continuous(limits = c(min(as.numeric(all_time$start), na.rm = T), max(as.numeric(all_time$end), na.rm = T)))+
#scale_linetype_manual(" ", values=c("solid", "solid")) +

dev.off()

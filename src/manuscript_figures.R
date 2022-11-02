###
# Code for manuscript figures
# Author: Jeffrey Minucci 12/19/2018
###


library(MASS)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(reshape2)
library(corrplot)
library(nlme)
library(gridExtra)
library(grid)

#set wd
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  rare_data <- "k:\\git\\rare_pollen\\"
} else if(Sys.info()[4]=="stp-air.local" || Sys.info()[4]=="stp-air"){
  rare_data <- path.expand("~/git/rare_pollen/")
} else
  rare_data <- paste(getwd(),"/",sep="")

osu_data <- paste(rare_data, "data_in/", sep="")



### Data formatting

#import raw data - everything
#file.exists(paste(osu_data,"CDRC_2017EPA_FP_DBT.csv",sep=""))

#Using neonic dataset with one high outlier sampling point removed (site FSR, 5/2/2015)
osu_covariate <- read.table(paste(osu_data,"CDRC_2017EPA_FP_DBT_clean_no_outlier.csv",sep=""), header = TRUE, 
                            sep = ",",strip.white=T,na.strings=c("","NA"))
interval_site <- read.csv(paste(osu_data,"interval_site_level.csv",sep=""),header=T, stringsAsFactors = T)


###############
# data cleaning and re-formatting - for neonic sampling data (may)
###############
osu_covariate <- osu_covariate[,1:23]
colnames(osu_covariate)[c(4,6)] <- c("planting","per_corn")
osu_covariate$planting<- factor(osu_covariate$planting,labels=c("no","yes"))
neonics_cols <- which(grepl("DBT|FP",colnames(osu_covariate))& !grepl("sample_id",colnames(osu_covariate)))

#Create categorical vars which track whether any given neonic was detected
neonics_detected <- as.data.frame(lapply(osu_covariate[,neonics_cols],function(x) ifelse((grepl("<",x) | is.na(x)),0,1)))
colnames(neonics_detected) <- paste(colnames(neonics_detected),"detected",sep="_")
osu_covariate <- cbind(osu_covariate,neonics_detected)

#Create two single categorical vars which tracks whether ANY neonic was detected in dead bees or pollen
osu_covariate$DBT_any_detected <- ifelse(rowSums(neonics_detected[,grepl("DBT",colnames(neonics_detected))]) > 0,1,0)
osu_covariate$FP_any_detected <- ifelse(rowSums(neonics_detected[,grepl("FP",colnames(neonics_detected))]) > 0,1,0)


#convert neonic concs to numeric
osu_covariate[,neonics_cols] <- as.numeric(gsub("< ","",as.matrix(osu_covariate[,neonics_cols])))

#create variables for total neonic loads
osu_covariate$DBT_total_neonic <- rowSums(osu_covariate[,grepl("DBT",colnames(osu_covariate))& !grepl("sample_id|detected",colnames(osu_covariate))])

osu_covariate$FP_total_neonic <- rowSums(osu_covariate[,grepl("FP",colnames(osu_covariate))& !grepl("sample_id|detected",colnames(osu_covariate))])

neonics_cols <- c(neonics_cols,which(colnames(osu_covariate) %in% c("DBT_total_neonic","FP_total_neonic")))

#str(osu_covariate) #display summary of cleaned dataframe


###############
# data cleaning and re-formatting - for hive health data (apr - aug)
###############

## Clean site-level interval data

# Make column names consistent
colnames(interval_site)[1:4] <- c("site_code","interval","n_hives","per_corn")
colnames(interval_site) <- gsub("mean.","",tolower(colnames(interval_site)))
colnames(interval_site) <- ifelse(substr(colnames(interval_site),nchar(colnames(interval_site)),
                                         nchar(colnames(interval_site))) == ".",substr(colnames(interval_site),1,nchar(colnames(interval_site))-1),colnames(interval_site))
colnames(interval_site) <- gsub("\\.","_",colnames(interval_site))

# Remove sums of neonic concentrations
interval_site <- interval_site[,!colnames(interval_site) %in% 
                                 c("sumclo_plt","sumclo_all","sumct_plt","sumct_all","sumneo_all")]

# Assign types
interval_site$interval <- factor(interval_site$interval,levels=c(1,2,3))
interval_site$per_corn <- interval_site$per_corn*100


## Create site-level means of the interesting neonics (thiamethoxam and clothianidin) over the planting period and all of May
## For merging with interval_site dataset

mean_may <- aggregate(cbind(FP_thiamethoxam, FP_clothianidin, FP_total_neonic)~site_code,data=osu_covariate,FUN=mean,na.rm=T)
colnames(mean_may)[2:4] <- c("mean_thi_may", "mean_clo_may","mean_total_may") 

mean_planting <- aggregate(cbind(FP_thiamethoxam, FP_clothianidin, FP_total_neonic)~site_code,data=subset(osu_covariate,planting=='yes'),FUN=mean,na.rm=T)
colnames(mean_planting)[2:4] <- c("mean_thi_planting", "mean_clo_planting","mean_total_planting") 

site_means <- cbind(mean_may,mean_planting[,-1])

#Make site names consistent between datasets
levels(site_means$site_code) <- c(levels(site_means$site_code),"MB","MO")
site_means[site_means$site_code =="MC","site_code"] <- "MB"
site_means[site_means$site_code =="MM","site_code"] <- "MO"
site_means$site_code <- droplevels(site_means$site_code)

## Inner join mean neonic conc. with site-level interval data
interval_site <- merge(interval_site,site_means,by="site_code")


#str(interval_site)

# Write cleaned data to csv
#write.csv(interval_site,paste(osu_data,"interval_site_level_clean.csv",sep=""))


## Convert intervals from long to wide
keep<- colnames(interval_site)[!grepl("chg|interval|n_hives",colnames(interval_site))]
site_wide <- reshape(interval_site,idvar=keep,timevar="interval",direction="wide",sep="_",
                     drop=c("n_hives"),new.row.names=c(1:10))



#Read frame-level data, fix column names
frame_data <- read.csv(paste(osu_data,"raw_frame_data.csv",sep=""),stringsAsFactors = T)[1:153,-c(25:34)]
colnames(frame_data) <- gsub("\\.comb","per_comb",colnames(frame_data))
colnames(frame_data) <- tolower(gsub("\\.","",colnames(frame_data)))
colnames(frame_data)[3] <- "per_corn"
frame_data[,grepl("per_comb",colnames(frame_data))] <- as.numeric(gsub("%","",as.matrix(frame_data[,grepl("per_comb",colnames(frame_data))])))
frame_data[["per_corn"]] <- as.numeric(gsub("%","",as.matrix(frame_data[,"per_corn"])))

#Make site DS hive_no consistent ** Note: need to find out what's going on with these DS hives?
frame_data$hive_no[frame_data$hive_no == 31.01] <- 31
frame_data$hive_no[frame_data$hive_no == 31.02] <- 32


#convert long to wide format
keep <- colnames(frame_data)[!grepl("cm2|comb|bees|yr_site|date|varroa|month",colnames(frame_data))]
frame_wide <- reshape(frame_data,idvar=keep,timevar="month",direction="wide",sep="_",
                      drop=c("yr_site","date","varroaincomplete"))
colnames(frame_wide)[1] <- "site_code"


##create relative change variables for entire study length (april to august) - for each hive

total_chg <- (frame_wide[,grepl("_8",colnames(frame_wide))] - frame_wide[,grepl("_4",colnames(frame_wide))]) / frame_wide[,grepl("_4",colnames(frame_wide))] * 100

total_chg <- cbind(hive_no = frame_wide$hive_no, site_code = frame_wide$site_code,total_chg)
total_chg <- do.call(data.frame,lapply(total_chg, function(x) replace(x, is.infinite(x),NA)))
colnames(total_chg)[2:ncol(total_chg)] <- gsub("_8","_pchg_total",colnames(total_chg)[2:ncol(total_chg)]) 

#create relative change vars for intervals 1, 2 and 3
months <- c(4,5,6,8)


##create initial value variables

initial <- cbind(hive_no = frame_wide$hive_no,site_code = frame_wide$site_code, frame_wide[,grepl("_4",colnames(frame_wide))])
colnames(initial)[2:ncol(initial)] <- gsub("_4","_initial",colnames(initial)[2:ncol(initial)]) 


#merge with original frame data to create a hive level dataset
new_vars <- merge(total_chg[-2],initial[,-2],by="hive_no")
frame_wide <- merge(frame_wide,new_vars,by="hive_no")

#bring in neonic concentrations from site level data
frame_wide <- merge(site_wide[,grepl("mean_t|mean_clo|site_code",colnames(site_wide))],frame_wide,by="site_code")

#Write new frame level dataset (in wide format)
#write.csv(frame_wide,paste(osu_data,"frame_data_expanded.csv",sep=""))

#Merge initial hive variables into our original site-level dataset
aggregated_initial <- aggregate( . ~ site_code  ,data=frame_wide[,grepl("_initial|site_code",colnames(frame_wide))],
                                 FUN=mean,na.rm=T)
aggregated_initial <- aggregated_initial[,grepl("site_code|seam|bees|capped|open|pollen|nectar",colnames(aggregated_initial)) & 
                                           !grepl("per_comb",colnames(aggregated_initial))] #keep only columns in initial (wide) site-level dataset
colnames(aggregated_initial)[2:7] <- c("seam_initial","bee_initial","cap_initial","open_initial","pollen_initial","nectar_initial")
site_wide <- merge(site_wide,aggregated_initial,by="site_code")

#remove variable for ALL neonics and replace with Clo + Thi (to be consistent with manuscript)
site_wide$mean_total_may <- site_wide$mean_clo_may + site_wide$mean_thi_may
site_wide$mean_total_planting <- site_wide$mean_clo_planting + site_wide$mean_thi_planting
colnames(site_wide)[colnames(site_wide) == "mean_total_may"] <- "mean_ct_may"
colnames(site_wide)[colnames(site_wide) == "mean_total_planting"] <- "mean_ct_planting"




#### Relative changes in hive health

health_vars <- c("seams_bees","bees_cm2","capped_cm2","open_cm2","pollen_cm2","nectar_cm2")
months <- c("4","5","6","8")
frame_wide_simple <- frame_wide[,colnames(frame_wide) %in% c("hive_no","site_code",as.vector(outer(health_vars,months,paste,sep="_")))]

# take averages at site level
aggregated_site <- aggregate( . ~ site_code  ,data=frame_wide_simple[,-2],FUN=mean,na.rm=T)

# relative changes for each interval (and overall)
for(i in 1:length(health_vars)){
  aggregated_site[[paste(strsplit(health_vars[i],"_")[[1]][1],"_relchg_1",sep="")]] <- (aggregated_site[[paste(health_vars[i],"_5",sep="")]] - aggregated_site[[paste(health_vars[i],"_4",sep="")]]) / 
    aggregated_site[[paste(health_vars[i],"_4",sep="")]] * 100
  
  aggregated_site[[paste(strsplit(health_vars[i],"_")[[1]][1],"_relchg_2",sep="")]] <- (aggregated_site[[paste(health_vars[i],"_6",sep="")]] - aggregated_site[[paste(health_vars[i],"_5",sep="")]]) / 
    aggregated_site[[paste(health_vars[i],"_5",sep="")]] * 100
  
  
  aggregated_site[[paste(strsplit(health_vars[i],"_")[[1]][1],"_relchg_3",sep="")]] <- (aggregated_site[[paste(health_vars[i],"_8",sep="")]] - aggregated_site[[paste(health_vars[i],"_6",sep="")]]) / 
    aggregated_site[[paste(health_vars[i],"_6",sep="")]] * 100
  
  
  aggregated_site[[paste(strsplit(health_vars[i],"_")[[1]][1],"_relchg_total",sep="")]] <- (aggregated_site[[paste(health_vars[i],"_8",sep="")]] - aggregated_site[[paste(health_vars[i],"_4",sep="")]]) / 
    aggregated_site[[paste(health_vars[i],"_4",sep="")]] * 100
  
}

#merge relative changes into site level dataset
site_wide <- merge(site_wide,aggregated_site,by="site_code")
site_wide <- do.call(data.frame,lapply(site_wide, function(x) replace(x, is.infinite(x),NA)))



###### END data formatting

#define a theme for manuscript plots


############################
theme_ms <-function (base_size = 12, base_family = "") {
  theme_bw() + theme(line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"), 
        rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1), 
        text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9,margin = margin(), debug = FALSE), 
        
        axis.text = element_text(size = rel(0.8), colour = "black",margin=margin(0.1,0.1,0.1,0.1,unit= "cm")), 
        strip.text = element_text(size = rel(0.8)), 
        axis.line = element_line(colour = "black"), 
        axis.text.x = element_text(vjust = 1), 
        axis.text.y = element_text(hjust = 1), 
        axis.ticks = element_line(colour = "black"), 
        axis.title.x = element_text(), 
        axis.title.y = element_text(angle = 90), 
        axis.ticks.length = unit(0.15, "cm"), 
        #axis.ticks.margin = unit(0.1, "cm"), 
        
        #panel.background = element_blank(), 
        #panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0.25, "lines"), 
        panel.spacing.x = NULL, 
        panel.spacing.y = NULL, 
        panel.ontop=FALSE,
        legend.key=element_blank(),
        
        
        strip.background = element_rect(fill = "grey80", colour = NA), 
        strip.text.x = element_text(), 
        strip.text.y = element_text(angle = -90), 
        
        plot.background = element_rect(colour = "white"), 
        plot.title = element_text(size = 20,face="bold",vjust=2), 
        plot.margin = unit(c(1, 1, 0.8, 0.5), "lines"), complete = TRUE)
}



#Figure S1 - 3 panel figure showing change in seams of bees over the 3 intervals

#interval 1
#fm_seams_r1 <- lm(seams_relchg_1 ~ mean_ct_may , data=site_wide)
#summary(fm_seams_r1) #significant reduction

theme_set(theme_ms(16))

g1 <- ggplot(site_wide,aes(mean_ct_may,seams_relchg_1))+geom_point(na.rm=T,size=2.5)+
  stat_smooth(method="lm",formula=y~x,na.rm=T,size=1.5, se=F, fullrange=T) + ylim(c(-150,150)) + xlim(c(-5, 20)) +
  labs(x="", y="Change in seams of bees (%)")+ geom_hline(yintercept=0)+
  coord_cartesian(ylim=c(-100, 100),xlim=c(5, 15),expand=T) + 
  annotate("text",x=7,y=100,label = "Late April - late May",size=4.5)+ 
  annotate("text",x=13.5,y=-70,label = "r^2 == 0.44",parse=T,size=4.5)
#g1


# interval 2
#fm_seams_r2 <- lm(seams_relchg_2 ~ mean_ct_may , data=site_wide)
#summary(fm_seams_r2) #significant increase 

g2 <- ggplot(site_wide,aes(mean_ct_may,seams_relchg_2))+geom_point(na.rm=T,size=2.5)+
  stat_smooth(method="lm",formula=y~x,na.rm=T,se=F, size=1.5, fullrange=T)+ ylim(c(-150,150)) + xlim(c(2.5, 20)) +
  labs(x="", y="Change in seams of bees (%)")+geom_hline(yintercept=0)+
  coord_cartesian(ylim=c(-100, 100),xlim=c(5, 15),expand=T)+ 
  annotate("text",x=7,y=100,label = "Late May - late June",size=4.5) + 
  annotate("text",x=13.5,y=-70,label = "r^2 == 0.43",parse=T,size=4.5)

g3 <- ggplot(site_wide,aes(mean_ct_may,seams_relchg_3))+geom_point(na.rm=T,size=2.5)+ 
  ylim(c(-150,150)) + xlim(c(2.5, 20)) +
  #geom_smooth(method="lm",formula=y~x,na.rm=T,size=2)+
  labs(x=expression(paste("Neonicotinoid content of pollen (ng g"^"-1",")",sep="")),
       y="Change in seams of bees (%)")+geom_hline(yintercept=0) +
  coord_cartesian(ylim=c(-100, 100),xlim=c(5, 15),expand=T) + 
  annotate("text",x=7.3,y=100,label = "Late June - mid August",size=4.5)


gA <- ggplotGrob(g1)
gB <- ggplotGrob(g2)
gC <- ggplotGrob(g3)
grid::grid.newpage()
#windows(w=24,h=14)
pdf(file="figures/seams_bees_change.pdf",width=5,height=13)
grid.draw(gtable_rbind(gA, gB,gC, size="max"))
dev.off()

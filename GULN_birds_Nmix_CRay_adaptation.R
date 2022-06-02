### Hierarchical models
### ADAPTATION OF C.RAY 2017 CODE, FOR USE WITH GULN BIRDS DATA

# Adapts the pre-processed survey and point data for use in hierarchical models, and
# fits Amundson-type models extended to include variation in interval length
# and trends in population density using data from multiple parks and years
# Code file name "script4trendAnalysis.R" available from C. Ray (cray@birdpop.org)
# 1) Adapt the clean survey and point data for the model in part 2 (~line 500)
# Read the .csv output from survey data query "qs_a121..." and point data query
# "qs_a204..."; interpret survey data from the perspective of a given focal species;
# select and display data from a subset of parks, years to guide model construction
# "# <<<<<" is used below to throw an error and call attention to any parameters
# requiring updates before fitting data from different species or to different models
require(dplyr)
# <<<<< update 5 settings as needed:
  species <- "OSFL"
  year <- "all" # one of '2005' ... to '2014' or 'all'
  park <- "all" # one of 'MORA', 'NOCA', 'OLYM' or 'all' 3 mtn parks
  bins <- "equalDensity" # one of 'equalWidth', 'equalDensity' or 'equalArea'
  outfile <- paste(species,"out1.eqD",sep="")
  # <<<<< update input file names as needed
    sd <- read.csv(file="nccn.survey.data.2005to2014.csv") #survey data
  pd <- read.csv(file="nccn.site.data.2005to2014.csv") #point/site data
  cd <- read.csv(file="nccn.climate.data.2005to2014-lagged1at2004-2013.csv")
  #ClimateWNA data are lagged 1 yr for both PAS and MST
  # - merge survey, point and climate data
  #before merging, compare structures to ensure compatibility; e.g., Location_code
  #might contain leading zeros for a transect in sd ("069a") but not in pd ("69a")
  #note: just replace "069a" with "69a" in the input file before reading it in; to
  #no avail I coded fixes (levels(x)[levels(x)=="069a"]<-"69a" or the "plyr" fn
  #revalue(x,c("069a"="69a")); though the merge DID work it still gave a warning
  names(sd); names(pd); names(cd)
  sd <- sd[,-1]; pd <- pd[,-1]; cd <- cd[,-1] #remove first columns
  sharedVars <- intersect(colnames(sd),colnames(pd))
  str(sd[,sharedVars])
  str(pd[,sharedVars])
  sharedVars <- intersect(colnames(sd),colnames(cd))
  str(cd[,sharedVars])
  sharedVars <- intersect(colnames(pd),colnames(cd))
  str(cd[,sharedVars])
  #Start_date format varies, so omit it from the merge (we'll use Day instead)
  j <- c("Event_year","Site_code","Location_code","Park_code","Stratum_name",
         "Species","Observer","Noise_level","Ever_sang","Seen_first","Group_size",
         "Obs_distance_m","Obs_notes","Day","Hour","y1","y2","y3")
  sd <- sd[,j]; dim(sd)
  j <- c("Event_year","Site_code","Location_code","Park_code","Panel_type","Panel_name",
         "Location_type","Detection_class","Is_forested","Habitat_num",
         "Elevation_m","Slope_deg_90mAvg","Aspect_deg","Beers_90mAvg","UTM_east","UTM_north")
  pd <- pd[,j]; dim(pd)
  #merge survey data with point data, keeping Park_code.x and Park_code.y separate
  44
  #note: this merge eliminates any problematic rows of pt-yr data w/o survey data
  data <- left_join(sd,pd,by=c("Event_year","Site_code","Location_code")); dim(data)
  #use Park_code, etc. to inspect the merge
  all.equal(data$Park_code.x,data$Park_code.y) #if not TRUE, fix
  sum(is.na(data$Park_code.x))
  sum(is.na(data$Park_code.y))
  sum(is.na(data$Elevation_m))
  head(data); tail(data) #everything looks good!
  #clean up the data frame
  data <- rename(data,Park_code=Park_code.x)
  data <- select(data,-Park_code.y)
  head(data)
  #merge with climate data
  data <- left_join(data,cd,by=c("Event_year","Site_code","Location_code")); dim(data)
  ***problems here can originate with the "069a" Location_code
  #inspect the merge
  names(data)
  all.equal(data$Park_code.x,data$Park_code.y) #if not TRUE, fix
  ***problems here can originate with the "069a" Location_code
  table(data$Park_code.x); table(data$Park_code.y)
  sum(is.na(data$Park_code.x))
  sum(is.na(data$anomPAS1))
  sum(data$Event_year=="2014")
  head(data); tail(data) #MAKE SURE everything looks good!
  #clean up the data frame
  data <- rename(data,Park_code=Park_code.x)
  data <- select(data,-Park_code.y)
  head(data); tail(data)
  # - remove data from small parks LEWI and SAJH
  i <- which(data$Park_code=="LEWI"|data$Park_code=="SAJH"); length(i)
  data <- data[-i,]; dim(data)
  # - set Group_size = 0 for non-focal spp (including species=NA/Nearest_obs=FALSE)
  data[which(data$Species!=species),"Group_size"] <- 0
  data[is.na(data$Species),"Group_size"] <- 0
  table(data$Group_size)
  focal_ct <- table(data$Group_size)[2] #keep this as a check below
  # - construct focal-sp non-detections, retaining all survey covar data
  #this method retains data on observer, etc.; in contrast, using unique()
  #is complicated by multiple records of the focal sp at a pt-yr, etc.
  #omit column of species names (Group_size will represent focal-sp detections)
  data <- data.frame(data[,-which(names(data)=="Species")])
  names(data); dim(data)
  #separate focal-sp detections from other detections
  data1 <- data[which(data$Group_size!=0),]; dim(data1) #focal-sp detections
  data0 <- data[which(data$Group_size==0),]; dim(data0) #non-focal sp detections
  data0 <- data0[!duplicated(data0[,c("Event_year","Site_code","Location_code")]),]
  dim(data0) #unique pt-yrs in which a non-focal sp was detected
  #eliminate non-focal sp detections from pt-yrs with focal-sp detections (fsd) by
  #stacking unique (nd) pt-yrs w/fsd above unique pt-yrs w/non-fsd to use !duplicated()
  45
  #note: we are not eliminating any focal-sp data in this temporary process
  data1.nd <- data1[!duplicated(data1[,c("Event_year","Site_code","Location_code")]),]
  dim(data1.nd) #unique pt-yrs in which the focal sp was detected
  temp <- rbind.data.frame(data1.nd,data0); dim(temp) #unique pt-yrs w/fsd over w/o
  nd <- temp[!duplicated(temp[,c("Event_year","Site_code","Location_code")]),]
  dim(nd) #a single event (fsd or non-fsd) for every pt-yr surveyed
  data0 <- nd[nd$Group_size==0,]; dim(data0) #non-fsd only at pt-yrs w/no fsd
  #remove interval counts and associated data from pt-yrs w/no fsd, retaining NAs
  data0[,c("Obs_distance_m","Obs_notes")] <- NA
  j <- c("Ever_sang","Seen_first","y1","y2","y3")
  apply(data0[,j],2,function(x) sum(is.na(x))) #display NAs for the check below
  data0[,j][data0[,j]>0] <- 0 #set all numbers to zero, retaining NAs
  apply(data0[,j],2,function(x) sum(x,na.rm=T)) #check - all zeros?
  apply(data0[,j],2,function(x) sum(is.na(x))) #check - NAs retained?
  #create compact data frame of survey data, restoring all focal-sp data
  data <- rbind.data.frame(data1,data0); dim(data); head(data)
  table(data$Group_size)
  focal_ct #should agree with table above
  # - index the focal data
  ip <- iy <- 1:dim(data)[1] #in case park=="all" & year=="all"
  if (park!="all") {ip <- which(data$Park_code==park)}
  if (year!="all") {iy <- which(data$Event_year==year)}
  data.i <- intersect(ip,iy)
  # - determine extent of NAs within the useful set of survey covars
  j <- which(names(data)=="Obs_distance_m"|names(data)=="Obs_notes"|names(data)=="y3")
  j
  sum(complete.cases(data[data.i,-j])); sum(is.na(data[data.i,-j]))
  # - write focal data to file
  dat <- data[data.i,]
  dat <- dat[order(dat$Event_year,dat$Site_code,dat$Location_code),]
  str(droplevels(dat))
  write.csv(dat,file=paste(species,".",year,".",park,".csv",sep="")) #*** update file name as needed
  # - if year=all, pad data w/NAs for points not visited in certain years
  if (year=="all") {
    #find unique points and unique point-years
    uniq.pts <- dat[!duplicated(dat[,c("Site_code","Location_code")]),]
    uniq.pt.yrs <-
      dat[!duplicated(dat[,c("Event_year","Site_code","Location_code")]),]
    dim(dat); dim(uniq.pts); dim(uniq.pt.yrs)
    #create "PY" data frame to hold all pt-yr data, including pt-yrs not surveyed
    a <- uniq.pts; names(a) #set time-varying covars to NA
    a[,c("Detection_class","Is_forested","Habitat_num")] <- NA
    yrs <- range(a$Event_year,na.rm=T); totyrs <- yrs[2]-yrs[1]+1; totyrs
    PY <- a #stack "a" into PY "totyrs" times
    for (i in 2:totyrs) PY <- rbind(PY,a)
    ey <- rep(yrs[1]:yrs[2],each=dim(uniq.pts)[1]) #create Event_year column
    length(ey)
    PY[,"Event_year"] <- ey; head(PY); tail(PY$Event_year)
    #join by static point attributes
    PY <- left_join(PY,dat,by=c("Event_year","Site_code","Location_code",
                                46
                                "Park_code","Stratum_name","Panel_type","Panel_name",
                                "Location_type","Elevation_m","Slope_deg_90mAvg",
                                "Aspect_deg","Beers_90mAvg","UTM_east","UTM_north"))
    apply(PY,2,function(x) sum(is.na(x))) #NAs should vary between .x and .y columns
    names(PY); j <- grep(".x",names(PY)); j #j identifies .x columns to delete
    PY <- PY[,-j]; names(PY)
    names(PY) <- sub("\\.y","",names(PY)); head(PY) #re-name .y columns
    #add climate vars for every pt-yr regardless of sampling
    PY <- left_join(PY,cd,by=c("Event_year","Site_code","Location_code","Park_code"))
    apply(PY,2,function(x) sum(is.na(x))) #NAs should vary between .x and .y columns
    names(PY); j <- grep(".x",names(PY)); j #j identifies .x columns to delete
    PY <- PY[,-j]; names(PY)
    names(PY) <- sub("\\.y","",names(PY)); head(PY) #re-name .y columns
    write.csv(PY,file=paste(species,".with.NAs.csv",sep="")) #*** update file name
    dat <- PY
  } # end: if (year=="all")
  # - calculate y = count of individual birds by point-year
  dat <- mutate(dat,ptyr=paste(Site_code,Location_code,Event_year,sep="."))
  dat <- mutate(dat,PTYR=ptyr)
  by_ptyr <- group_by(dat,ptyr) #group by pt-yr
  #re-code Detection_class to allow averaging across records below (for now)
  dat$Detection_class <- abs(as.numeric(dat$Detection_class)-2)
  by_ptyr_v <- summarize(by_ptyr,
                         y=sum(Group_size,na.rm=T),
                         PTYR=first(PTYR),
                         noise=first(Noise_level),
                         day=first(Day),
                         hour=mean(Hour,na.rm=T),
                         obs=first(Observer),
                         forest=mean(Is_forested,na.rm=T),
                         dense=mean(abs(as.numeric(Detection_class)-2),na.rm=T),
                         elev=first(Elevation_m),
                         slope=first(Slope_deg_90mAvg),
                         aspect.d=first(Aspect_deg),
                         aspect.b=first(Beers_90mAvg),
                         strat=first(Stratum_name),
                         tran=first(Site_code),
                         Park=first(Park_code),
                         Year=first(Event_year),
                         PASanom=first(anomPAS1),
                         MSTanom=first(anomMST1))
  #re-name the vars
  y <- by_ptyr_v$y
  PTYR <- by_ptyr_v$PTYR
  noise <- by_ptyr_v$noise #covars will be rescaled in data step
  day <- by_ptyr_v$day
  hour <- by_ptyr_v$hour
  obs <- droplevels(by_ptyr_v$obs)
  forest <- by_ptyr_v$forest
  dense <- by_ptyr_v$dense
  elev <- by_ptyr_v$elev
  slope <- by_ptyr_v$slope
  aspect.d <- by_ptyr_v$aspect.d #Aspect_deg
  aspect.b <- by_ptyr_v$aspect.b #Beers_90mAvg
  strat <- droplevels(by_ptyr_v$strat)
  tran <- droplevels(by_ptyr_v$tran)
  Park <- droplevels(by_ptyr_v$Park) #lowercase "park" is used elsewhere to
  #define the dataset (park==all)
  Year <- by_ptyr_v$Year #lowercase "year" used elsewhere
  47
  PASanom <- by_ptyr_v$PASanom
  MSTanom <- by_ptyr_v$MSTanom
  #get rid of zeros in y that are really NAs
  i <- which(is.na(day)); length(i)
  y[i] <- NA; table(y)
  #get rid of NaNs in hour, which resulted from NAs when taking the mean
  i <- which(is.nan(hour)); length(i)
  hour[i] <- NA; table(hour)
  # display data and relationships
  page <- 1
  pdf(file=paste("pt-yr.plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1,1)+0.1)
  layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
  #day
  h <- hist(day,plot=F); h
  tb <- h$counts; labText <- "Day of count (ordinal)"
  names(tb) <- round(h$mids)
  bp <- barplot(tb,ylab="Point-counts completed",cex.axis=1.5,cex.lab=1.5,
                cex.names=1.25,xlab=labText,las=3)
  legend(bp[round(length(bp)/2)],max(tb),xjust=0.5,bty="n",
         legend=c(paste("Min",min(day,na.rm=T)),paste("Max",max(day,na.rm=T))))
  #hour
  tb <- table(hour); labText <- "Hour of count (a.m.)"
  bp <- barplot(tb,ylab="Point-counts completed",cex.axis=1.5,cex.lab=1.5,
                cex.names=1.5,xlab=labText)
  #observers
  tb <- table(droplevels(obs)); labText <- "Observer (initials)"
  firstNames <- sub("^.*_","",names(tb)); firstNames
  firstInitials <- substring(firstNames,1,1); firstInitials
  lastNames <- sub("_.*$","",names(tb)); lastNames
  lastInitials <- substring(lastNames,1,1); lastInitials
  initials <- paste(firstInitials,lastInitials,sep=""); initials
  names(tb) <- initials; names(tb)
  bp <- barplot(tb,ylab="Point-counts completed",cex.axis=1.5,cex.lab=1.5,
                cex.names=1.25,xlab=labText,las=3)
  dev.off()
  page <- 2
  pdf(file=paste("pt-yr.plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,8,2,8)+0.1)
  #surveys X park
  tb <- table(droplevels(Park[hour>0])); labText <- "Park"
  bp <- barplot(tb,ylab="Point-counts completed",cex.axis=1.5,cex.lab=1.5,space=0.75,
                cex.names=1.5,las=3)
  #strat
  tb <- table(droplevels(strat[hour>0])); labText <- "Stratum"
  names(tb) <- sub("^.*_","",names(tb))
  bp <- barplot(tb,ylab="Point-counts completed",cex.axis=1.5,cex.lab=1.5,space=0.75,
                cex.names=1.5,xlab=labText)
  #forest
  tb <- table(forest); labText <- "Forested?"
  names(tb) <- c("No","Yes")
  48
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,space=1,
                cex.names=1.5,xlab=labText)
  #dense
  tb <- table(dense); labText <- "Dense cover?"
  names(tb) <- c("No","Yes")
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,space=1,
                cex.names=1.5,xlab=labText)
  dev.off()
  page <- 3
  pdf(file=paste("pt-yr.plots",page,".pdf",sep=""))
  par(mfrow=c(1,1),mar=c(4,8,1,8)+0.1)
  #noise
  tb <- table(noise); labText <- "Noise level during count"
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,
                cex.names=1.5,xlab=labText)
  for (i in 0:max(tb)) {text(bp[i+1],0.9*max(tb),tb[i+1],cex=0.9)}
  dev.off()
  page <- 4
  pdf(file=paste("pt-yr.plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  #elev
  h <- hist(elev[hour>0],plot=F); h
  tb <- h$counts; labText <- "Elevation (x100 m)"
  names(tb) <- as.numeric(h$mids)/100
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,
                cex.names=1,xlab=labText,las=3)
  #slope
  h <- hist(slope[hour>0],plot=F); h
  tb <- h$counts; labText <- "Slope (degrees)"
  names(tb) <- round(as.numeric(h$mids))
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,
                cex.names=1.25,xlab=labText,las=3)
  #aspect.d
  h <- hist(aspect.d[hour>0],plot=F); h
  tb <- h$counts; labText <- "Aspect (degrees)"
  names(tb) <- h$mids
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,
                cex.names=1,xlab=labText,las=3)
  #aspect.b
  h <- hist(aspect.b[hour>0],plot=F); h
  tb <- h$counts; labText <- "Aspect (Beers transform)"
  names(tb) <- h$mids
  bp <- barplot(tb,ylab="Frequency",cex.axis=1.5,cex.lab=1.5,
                cex.names=1,xlab=labText,las=3)
  #Beers_90mAvg=1+cos((45-Aspect_deg[averaged over 90-m radius])*pi/180) (Beers 1966)
  #can't back-transform uniquely, so grab original Aspect_deg, below
  par(new=TRUE,mar=c(15,13,1.5,1)+0.1)
  plot(aspect.d[hour>0],aspect.b[hour>0],xlab="Aspect",ylab="Beers transform",pch=".")
  dev.off()
  page <- 5
  pdf(file=paste("pt-yr.plots",page,".pdf",sep=""))
  49
  #returns a p-value from ANOVA of y~x
  aov.p.val <- function(y,x) {
    aov.yx <- aov(y~x)
    return(unlist(summary(aov.yx))["Pr(>F)1"])
  }
  assoc <- function(x,y,mtxt,xtxt,ytxt,rtxt) {
    tb <- table(x,y)
    if (!missing(rtxt)) rownames(tb) <- rtxt
    assocplot(tb[,1:dim(tb)[2]],main=mtxt,xlab=xtxt,ylab=ytxt,
              col=c("black","white"))
  }
  par(mfrow=c(2,2),mar=c(4,4,1,1)+0.1)
  if (park=="all") {
    #noise x park
    xa <- droplevels(Park); ya <- noise
    mtxt <- paste("ANOVA p =",round(aov.p.val(ya,xa),digits=3))
    assoc(xa,ya,mtxt,"Park","Noise")
    #park x observer ("initials" for each observer defined above)
    xa <- droplevels(obs); ya <- droplevels(Park)
    mtxt <- paste("ANOVA p =",round(aov.p.val(as.numeric(ya),as.numeric(xa)),
                                    digits=3))
    assoc(xa,ya,mtxt,"Observer","Park",initials)
  } #end: if (park=="all")
  dev.off()
  #display focal-species patterns - page 1
  page <- 1
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  if (park=="all") {
    #count by park ANOVA
    xa <- droplevels(Park[hour>0]); ya <- y
    mtxt <- paste(species,
                  " x park ANOVA p = ",round(aov.p.val(ya,xa),digits=3),sep="")
    assoc(xa,ya,mtxt,"Park","Count")
  } #end: if (park=="all")
  #ct x stratum
  xa <- droplevels(strat); ya <- y
  mtxt <- paste(species," x stratum p = ",round(aov.p.val(ya,xa),digits=3),sep="")
  assoc(xa,ya,mtxt,"Stratum","Count")
  dev.off()
  page <- 2
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  plotpoisson <- function(x,y,mtxt,xtxt,ytxt) {
    plot(jitter(x),jitter(y),main=mtxt,xlab=xtxt,ylab=ytxt,
         cex.main=1.5,cex.axis=1.5,cex.lab=1.5)
    i <- !is.na(x) #index used to limit null fit to same dataset as model fit
    50
    mod <- glm(y[i]~x[i],family=poisson)
    x.rng <- seq(min(x[i]),max(x[i]),length.out=sum(i))
    y.pred <- exp(mod$coefficients[1]+mod$coefficients[2]*x.rng) #predict()
    lines(x.rng,y.pred,lwd=2,col="red")
    null <- glm(y[i]~1,family=poisson)
    null.dAIC <- AIC(mod,null)[2,2]-AIC(mod,null)[1,2]
    text(max(x.rng),max(y,na.rm=T)-0.25,col="red",cex=1,pos=2,
         paste("Null dAIC=",round(null.dAIC,digits=2),sep=""))
    p.val <- summary(mod)$coefficients[2,"Pr(>|z|)"]
    text(max(x.rng),max(y,na.rm=T)-0.5,col="red",cex=1,pos=2,
         paste("P=",round(p.val,digits=2),sep=""))
  }
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  #count by pt-yr
  tb <- table(y); labText <- "Count by point"
  if (year=="all") {yr <- "-year"} else {yr <- paste(" (",year,")",sep="")}
  labText <- paste(labText,yr,sep="")
  bp <- barplot(tb,main=species,ylab="Frequency",cex.main=1.5,cex.axis=1.5,
                cex.lab=1.5,cex.names=1.5,xlab=labText)
  for (i in 0:max(tb)) {text(bp[i+1],0.9*max(tb),tb[i+1],cex=0.9)}
  #count by noise
  plotpoisson(noise,y,"","Noise (jittered)",paste(species,"count (jittered)"))
  #count by pt-yr-day
  plotpoisson(day,y,"","Day (jittered)",paste(species,"count (jittered)"))
  #count by pt-yr-hour
  plotpoisson(hour,y,"","Hour (jittered)",paste(species,"count (jittered)"))
  dev.off()
  page <- 3
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  #raw count by park and year
  par(mfrow=c(1,2),mar=c(4,4,1,1)+0.1)
  yraw <- mutate(data.frame(y),pkyr=paste(Park,Year,sep="."))
  by_pkyr <- group_by(yraw,pkyr) #group by park-yr
  by_pkyr_v <- summarize(by_pkyr,toty=sum(y,na.rm=T))
  toty <- by_pkyr_v$toty; toty
  yraw_yr <- mutate(data.frame(y),Year)
  by_yr <- group_by(yraw_yr,Year) #group by yr
  by_yr_v <- summarize(by_yr,toty_yr=sum(y,na.rm=T))
  toty_yr <- by_yr_v$toty_yr; toty_yr
  ht <- 1.5*max(toty_yr,na.rm=T)
  plot(1:10,toty_yr,ylim=c(0,ht),xlab="Year",ylab=paste(species,"unadjusted count"),
       pch="",cex.lab=1.5,cex.axis=1.25)
  lines(1:10,toty_yr,lwd=2)
  if (park=="all") {
    lines(1:10,toty[1:10],lty=2,col="red")
    lines(1:10,toty[11:20],lty=1,col="green3")
    lines(1:10,toty[21:30],lty=4,col="blue")
    legend(3,ht,legend=c("Total","MORA","NOCA","OLYM"),lwd=c(2,1,1,1),
           lty=c(1,2,1,4),col=c("black","red","green3","blue"),bty="n",cex=1.5)
  } else {
    text(3,ht,park,cex=1.5)
  } #end: if (park=="all")
  #effort-adjusted count by park and year (count per pt surveyed omitting interval 3)
  datprime <- select(dat,y1,y2,Day,Park_code,Event_year,ptyr) #omit interval 3
  datprime <- mutate(datprime,yprime=y1+y2,pkyr=paste(Park_code,Event_year,sep="."))
  51
  #group by pt and then by park to avoid over-counting pts surveyed
  by_ptyr_prime <- group_by(datprime,ptyr) #group by pt 1st
  by_ptyr_v_prime <- summarize(by_ptyr_prime,
                               dayprime=first(Day),
                               yprime=sum(yprime,na.rm=T),
                               pkyr=first(pkyr),
                               yearprime=first(Event_year))
  by_pkyr <- group_by(by_ptyr_v_prime,pkyr) #group by park 2nd
  by_pkyr_v <- summarize(by_pkyr,
                         yprime=sum(yprime,na.rm=T),
                         surveyed_pts=sum(dayprime>0,na.rm=T),
                         yearprime=first(yearprime))
  ct_pkyr <- by_pkyr_v$yprime #total count by park-year
  pts_pkyr <- by_pkyr_v$surveyed_pts #total pts surveyed by park-year
  tot_pk <- ct_pkyr/pts_pkyr
  by_yr_prime <- group_by(by_pkyr_v,yearprime) #group by year to graph
  by_yr_v_prime <- summarize(by_yr_prime,
                             totct=sum(yprime,na.rm=T),
                             totpts=sum(surveyed_pts,na.rm=T))
  toty <- by_yr_v_prime$totct/by_yr_v_prime$totpts
  ht <- 1.5*max(tot_pk,na.rm=T)
  plot(1:10,toty,ylim=c(0,ht),xlab="Year",
       ylab=paste(species,"effort-adjusted count per point"),
       pch="",cex.lab=1.5,cex.axis=1.25)
  lines(1:10,toty,lwd=2)
  if (park=="all") {
    lines(1:10,tot_pk[1:10],lty=2,col="red")
    lines(1:10,tot_pk[11:20],lty=1,col="green3")
    lines(1:10,tot_pk[21:30],lty=4,col="blue")
    legend(3,ht,legend=c("Total","MORA","NOCA","OLYM"),lwd=c(2,1,1,1),
           lty=c(1,2,1,4),col=c("black","red","green3","blue"),bty="n",cex=1.5)
  } else {
    text(3,ht,park,cex=1.5)
  } #end: if (park=="all")
  dev.off()
  page <- 4
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  #count by year
  par(mfrow=c(1,1),mar=c(4,4,1.5,1)+0.1)
  y_prime <- by_ptyr_v_prime$yprime[hour>0] #defined for plot above
  par(mfrow=c(1,1),mar=c(4,4,1,1)+0.1)
  plotpoisson(Year,y_prime,"","Year (jittered)",
              paste(species,"effort-adjusted point counts (jittered)"))
  dev.off()
  page <- 5
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  #occ x forest
  occ <- y>0; occ <- as.numeric(occ)
  xa <- forest; ya <- occ
  mtxt <- paste(species,"x forest ANOVA p =",round(aov.p.val(ya,xa),digits=3))
  assoc(xa,ya,mtxt,"Forested","Occupied")
  #occ x dense
  xa <- dense; ya <- occ
  mtxt <- paste(species,"x dense cover ANOVA p =",round(aov.p.val(ya,xa),digits=3))
  assoc(xa,ya,mtxt,"Dense cover","Occupied")
  52
  #count by elevation
  #no need for "hour>0" here, because y contains NAs
  plotpoisson(elev,y,"","Elevation (jittered)",paste(species,"count (jittered)"))
  #count by slope
  plotpoisson(slope,y,"","Slope (jittered)",paste(species,"count (jittered)"))
  dev.off()
  page <- 6
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  #count by pt-yr-aspect (Beers)
  #this regression is ok because y contains NAs (no pseudorep of zeros)
  plotpoisson(aspect.b,y,"","Aspect (Beers transform, jitter)",
              paste(species,"count (jittered)"))
  text(0.25,0.5,"SW",col="red"); text(1.75,0.5,"NE",col="red")
  #count by pt-yr-aspect (untransformed)
  plot(jitter(aspect.d),jitter(y),main="",
       xlab="Aspect (degrees, jitter)",ylab=paste(species,"count (jittered)"),
       cex.main=1.5,cex.axis=1.5,cex.lab=1.5)
  #count by PAS anomaly
  plotpoisson(PASanom,y,"","Lag-1 PAS anomaly (mm, jitter)",
              paste(species,"count (jittered)"))
  #count by MST anomaly
  plotpoisson(MSTanom,y,"","MST anomaly (C, jitter)",
              paste(species,"count (jittered)"))
  dev.off()
  page <- 7
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  #find tinterval = time-of-removal per detection - function used below
  get.first <- function(x) {
    if (sum(x,na.rm=T)>0) {posn <- min(which(x!=0))} else {posn <- NA}
  }
  #plot effects of distance and time
  # nbreaks = number of distance bins
  nbreaks <- 5 #this is the number Andy Royle suggested
  #find a sample for which detection distance and interval are independent
  samp <- 0.9 #initial sample = 90% of all detections at this pt-yr
  repeat{
    #find dclass = distance class per detection
    d <- dat$Obs_distance_m
    par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
    hist(d,breaks=8,main=paste(species,year,"detections"),cex.lab=1.5,
         xlab="Detection distance (m)")
    maxd <- trunc(quantile(d,probs=samp,type=8,na.rm=T)); maxd
    if (bins=="equalDensity") {
      breaks <- quantile(d[d<=maxd],probs=seq(0,1,(1/nbreaks)),type=7,na.rm=TRUE)
      breaks[1] <- 0
    }
    if (bins=="equalArea") { #radii=sqrt((x/nbreaks)*maxd^2) where x = number of bins
      breaks <- trunc(c(0,sqrt((1:nbreaks)/nbreaks*(maxd^2))))
      53
    }
    if (bins=="equalWidth") {
      breaks <- round(seq(0,maxd,maxd/nbreaks))
    }
    dclass <- cut(d,breaks,labels=F)
    areas <- pi*((breaks[2:(nbreaks+1)]^2)-(breaks[1:nbreaks]^2))
    #freq of detections in each dclass
    tb <- table(dclass)
    bp <- barplot(tb,width=areas,ylim=c(0,1.2*max(tb)),ylab="Number of detections",
                  cex.main=1.5,cex.axis=1.5,cex.lab=1.5,cex.names=1.5,
                  xlab="Distance class")
    for (i in 1:nbreaks) {text(bp[i],0.15*max(tb),round(breaks[i+1]),cex=1)}
    for (i in 1:nbreaks) {text(bp[i],0.1*max(tb),"m",cex=1)}
    #pct of detection-distances truncated to avoid fitting noise, non-independence
    yj <- subset(dat,select=c(y1,y2,y3)); yj[1:50,]
    truncPct <- sum(yj[d>maxd,],na.rm=T)/sum(yj,na.rm=T)*100; print(truncPct)
    text(round(bp[nbreaks]),(1.1*max(tb)),col="red",cex=1,pos=2,
         paste("Truncated ",round(truncPct),"% (>",maxd,"m)",sep=""))
    #detections per total (cumulative) area
    density <- cumsum(tb)/(pi*(breaks[2:(nbreaks+1)]^2))
    bp <- barplot(density,width=areas,ylab="Detections/cumulative area",cex.main=1.5,
                  cex.axis=1.5,cex.lab=1.5,cex.names=1.5,xlab="Distance class")
    #detections per area of each class
    density <- tb/areas
    bp <- barplot(density,width=areas,ylab="Detections/class area",cex.main=1.5,
                  cex.axis=1.5,cex.lab=1.5,cex.names=1.5,xlab="Distance class")
    #find tinterval = time-of-removal per detection
    f <- apply(yj,1,get.first)
    tinterval <- as.numeric(f) #plotted way below
    #find interval x distance association; looking for independence
    xa <- dclass; ya <- tinterval
    pval <- aov.p.val(ya,xa)
    mtxt <- paste("Assoc: interval x dist, p =",round(pval,digits=3))
    assoc(xa,ya,mtxt,"Detection distance class","Detection interval")
    #find slope of interval x distance; do not truncate distant detections if
    #detection interval is negatively related to distance
    IxD <- lm(ya~xa); slope <- coef(IxD)[2]
    if (pval > 0.05 | samp < 0.6 | slope < 0) {break} else {samp <- samp - 0.025}
  } #repeat
  dev.off()
  if (samp < 0.6) {stop("Detection interval and distance are not independent")}
  # delta = distance width for each bin (set to be equal intervals in this example)
  delta <- breaks[-1] - breaks[-length(breaks)]; delta
  # mdpts = midpoints of distance bins
  mdpts <- breaks[-1] - delta/2; mdpts
  #find nsurveys = number of points surveyed (less than or equal to rows of data)
  nsurveys <- length(y); nsurveys #y=num of focal-sp indivs detected in each survey
  # find nobs = number of times the species was detected (as an individual or group)
  #NOTE: To use nobs the way Amundson did, I sort a data frame of dclass,
  #tinterval and surveyid such that detections appear in the top 1:nobs rows;
  #nobs is used to index (i <- 1:nobs) surveyid[i], dclass[i] and interval[i]
  54
  #for each observation of a species in the Amundson model, and surveyid[i]
  #should store a number of unique ids equal to nsurveys, because it is used
  #to fill pi.pd.c[j,k] and pi.pa.c[j,k], where k = 1:nsurveys, in the
  #observation-level model, as:
  #for(i in 1:nobs) {
  # dclass[i] ~ dcat(pi.pd.c[,surveyid[i]])
  # tinterval[i] ~ dcat(pi.pa.c[,surveyid[i]]) }
  #set surveyid = point ID (site ID) for each individual (or group) detected
  surveyid <- as.numeric(as.factor(dat$ptyr)); tail(surveyid)
  #look at correlations between detection interval, hour, day and group size
  hr <- dat$Hour
  dy <- dat$Day
  gs <- dat$Group_size
  nobs <- sum(gs>0,na.rm=T); nobs
  obsData <- data.frame(surveyid,dclass,tinterval,hr,dy,gs)
  head(obsData); dim(obsData)
  obsData <- obsData[order(dat$Group_size,decreasing=T),]; obsData[(nobs-2):(nobs+2),]
  #the line above should show the transition between high survey-id #s and low ones
  #if nobs is correct, because all detections are listed first, followed by
  #non-detections
  obsDat <- obsData[1:nobs,] #grab the data associated with detections
  surveyid <- obsDat$surveyid
  dclass <- obsDat$dclass
  tinterval <- obsDat$tinterval
  hr <- obsDat$hr
  dy <- obsDat$dy
  gs <- obsDat$gs
  cor(obsDat[,-1],use="complete.obs")
  cor.test(dy,hr,use="complete.obs",method="kendall")
  cor.test(gs,dclass,use="complete.obs",method="kendall")
  cor.test(dy,hr,use="complete.obs",method="kendall")
  #other correlations are examined below
  #display any dependence of time-of-removal on detection-distance
  page <- 8
  pdf(file=paste(outfile,".plots",page,".pdf",sep=""))
  par(mfrow=c(2,2),mar=c(4,4,1.5,1)+0.1)
  #interval by hour
  xa <- hr; ya <- tinterval
  mtxt <- paste("Assoc: interval x hour ANOVA p =",round(aov.p.val(ya,xa),digits=3))
  assoc(xa,ya,mtxt,"Hour","Detection interval")
  #note: even if the raw count is strongly related to hour, the model will find
  #no effect of hour if the interval of detection is not related to hour
  #display freq of detections in each time interval
  tb <- table(tinterval); tb
  bp <- barplot(tb,ylab="Number of detections",main=paste(species,year),cex.main=1.5,
                cex.axis=1.5,cex.lab=1.5,cex.names=1.5,xlab="Time interval",space=0.5)
  nas <- is.na(yj); nas <- apply(nas,2,sum); nas
  for (i in 1:3) {text(bp[i],0.9*max(tb),paste(nas[i],"NAs"),cex=0.75)}
  #display interval x distance association (hidden in loop above)
  xa <- dclass; ya <- tinterval
  pval <- aov.p.val(ya,xa)
  mtxt <- paste("Assoc: interval x dist, p =",round(pval,digits=3))
  assoc(xa,ya,mtxt,"Detection distance class","Detection interval")
  dev.off()
  55
  # ntrans = total number of transects
  ntrans <- length(unique(dat$Site_code)); ntrans
  # nyears = number of years/primary surveys
  nyears <- totyrs
  # nobservers = number of observers over the season/period modeled
  nobservers <- length(initials)
  # Nst = initial value for estimate of N - must be close to N or model will not run
  Nst <- y+1; table(Nst); sum(is.na(Nst))
  # J = number of time intervals within each point survey
  J <- 3
  # 2) run Amundson-type models but with variable-length count intervals
  # and trends fitted to data from multiple years and parks
  require(jagsUI)
  # R/JAGS code adapted from Amundson et al. (2014) for estimation of
  # population trends from multi-region point-count data using an
  # N-mixture model to fit covariate effects on trend and on each of
  # two components of detection probability: availability based on
  # time-removal data from intervals varying in length (Farnsworth
  # et al. 2002), and perceptibility based on detection distance
  #
  # References:
  # Amundson, C. L., J. A. Royle, and C. M. Handel. 2014. A hierarchical
  # model combining distance sampling and time removal to estimate
  # detection probability during avian point counts. The Auk 131:476-494.
  # Farnsworth, G. L., K. H. Pollock, J. D. Nichols, T. R. Simons, J. E. Hines,
  # and J. R. Sauer. 2002. A removal model for estimating detection
  # probabilities from point-count surveys. The Auk 119:414-425.
  # VARIABLES
  # y = count of birds per point (*associated w/surveyid, not tinterval)
  # surveyid = survey point/site ID for each detection (individual or group)
  # dclass = distance class per detection
  # tinterval = time interval per detection
  # nsurveys = number of points surveyed (*number of pt-yrs)
  # nobs = number of individuals (or groups) detected
  # delta = distance width for each bin (set as equal intervals in this example)
  # nbreaks = number of distance bins
  # mdpts = midpoints of distance bins
  # maxd = maximum truncated distance (e.g., 200 m)
  # J = number of time periods
  # ... = standardized covariates (e.g., cover w/in 200m)
  # ntrans = total number of transects
  # tran = transect ID for each point
  # day = ordinal date of survey
  # Nst = initial value for estimate of N - must be close to N
  # INDICES
  # k in 1:nsurveys # surveys
  # b in 1:nbreaks # distance bins
  # j in 1:J # time intervals
  # i in 1:nobs # detections (individual or group), each having a distance category
  # t in 1:ntrans # transects
  # PARAMETERS TO ESTIMATE
  56
  # meansig = mean scale parameter across sites (half normal shape in this example)
  # meanpdet = mean probability of perceptibility
  # meanpavail = mean probability of availability
  # a0 = intercept for availability
  # b.a1, b.a2, ... = coefficients of covars in availability model
  # sigma0 = intercept for perceptibility
  # b.p1, b.p2, ... = coefficients of covars in perceptibility model
  # b1, b2, ... = coefficients of covars in abundance model
  # meanN = mean site-level abundance
  # mu.tran = mean abundance intercept across transects
  # sd.tran = SD of random transect effect
  # totN = population size of total area surveyed
  # bayesp.pd = Bayesian p-value for pd model
  # bayesp.pa = Bayesian p-value for pa model
  # dens.ha = density of birds per hectare = totN/area surveyed
  # <<<<< update file name as needed
    file.name <- paste(species,"-fixPk-rndYr-rndTran-trend-climate.jagsUI",sep="")
  cat("
model {
# PRIORS
a0 ~ dnorm(0,0.01) # availability intercept
b.a1 ~ dnorm(0,0.01) # coefs of availability covars (detection interval)
b.a2 ~ dnorm(0,0.01)
sigma0 ~ dunif(0,200) #scale of detection, bounded for covergence
b.p1 ~ dnorm(0,0.01) # coefs of perceptibility covars (perception distance)
b.p2 ~ dnorm(0,0.01)
b0 ~ dnorm(0,0.01) # abundance intercept
b1 ~ dnorm(0,0.01) # coefs of abundance covars
b2 ~ dnorm(0,0.01)
b3 ~ dnorm(0,0.01)
b4 ~ dnorm(0,0.01)
for (p in 1:3) { # park-specific effects
parkeff[p] ~ dnorm(0,0.001)T(-15,15) # intercepts bounded for convergence
for (l in 1:3) { # strata
strateff[p,l] ~ dnorm(0,tau.strat[p,l]/wt[p,l]) # weighted stratum effect
tau.strat[p,l] <- pow(sd.strat[p,l],-2) # (to be implemented)
sd.strat[p,l] ~ dunif(0,10)
} # strata
}
for (p in 1:3) { # park-specific trends
bp[p] ~ dnorm(0,0.001)T(-10,10) # slopes bounded for covergence
}
# random transect effect on abundance intercept nested pt w/in tran
for (t in 1:ntrans) {
traneff[t] ~ dnorm(0,tau.tran)
}
tau.tran <- pow(sd.tran,-2)
sd.tran ~ dunif(0,10)
# random year effect on abundance
for (t in 1:nyears) {
yeareff[t] ~ dnorm(0,tau.year)
}
tau.year <- pow(sd.year,-2)
sd.year ~ dunif(0,10)
57
# random observer effect on perception
for (i in 1:(nobservers+1)) {
obseff[i] ~ dnorm(0,tau.obs)
}
tau.obs <- pow(sd.obs,-2)
sd.obs ~ dunif(0,10)
# random transect effect on perception
for (t in 1:ntrans) {
traneff.pd[t] ~ dnorm(0,tau.tr.pd)
}
tau.tr.pd <- pow(sd.tr.pd,-2)
sd.tr.pd ~ dunif(0,10)
# random year effect on perception
for (i in 1:10) {
yeareff.pd[i] ~ dnorm(0,tau.yr.pd)
}
tau.yr.pd <- pow(sd.yr.pd,-2)
sd.yr.pd ~ dunif(0,10)
# random pt-year effect on perception
for (k in 1:nsurveys) {
ptyreff.pd[k] ~ dnorm(0,tau.py.pd)
}
tau.py.pd <- pow(sd.py.pd,-2)
sd.py.pd ~ dunif(0,10)
# DETECTION PROBABILITY FUNCTIONS
for (k in 1:nsurveys) {
# covariates of availability and perceptibility***
log(sigma[k]) <- log(sigma0) #+ obseff[observer[k]] #+ ptyreff.pd[PTYR[k]]
logit(q[k]) <- a0 #+ b.a1*day[k] + b.a2*hour[k]
# distance-based estimation of detection probability
for (b in 1:nbreaks) {
log(g[b,k]) <- -mdpts[b]*mdpts[b]/(2*sigma[k]*sigma[k]) #half-normal
f[b,k] <- (2*mdpts[b]*delta[b])/(maxd*maxd) # bin widths vary***
pi.d[b,k] <- g[b,k]*f[b,k] # p(detection) per pt-bin
pi.d.c[b,k] <- pi.d[b,k]/pd[k] # conditional form stdized by p(detected)
} # b in 1:nbreaks
pd[k] <- sum(pi.d[,k]) # pd is a sum over all dclass bins
# removal-based estimation of availability given unequal intervals
pi.a[1,k] <- 1-pow(q[k],3) # p(available in interval j=1)
pi.a[2,k] <- pow(q[k],3)*(1-pow(q[k],2)) # p(avail in j=2)
pi.a[3,k] <- pow(q[k],5)*(1-pow(q[k],2)) # p(avail in j=3)
# p(available in each interval j | available in at least one interval)
for (j in 1:J) {pi.a.c[j,k] <- pi.a[j,k]/pa[k]}
pa[k] <- sum(pi.a[,k]) # p(available in at least one interval)
} # k in 1:nsurveys
# OBSERVATION-LEVEL MODEL
58
for (i in 1:nobs) {
# single binomial trial with categorical dist linking dclass & tinterval to pt
dclass[i] ~ dcat(pi.d.c[,surveyid[i]]) # p(outcome = 1 to nbreaks dclasses)
tinterval[i] ~ dcat(pi.a.c[,surveyid[i]]) # p(outcome = 1 to J intervals)
} # i in 1:nobs
# ABUNDANCE ESTIMATION
for (k in 1:nsurveys) {
# counts as a function of number available and detection probability
y[k] ~ dbin(pd[k],n.a[k])
# number available for sampling as a function of abundance and p(available)
n.a[k] ~ dbin(pa[k],N[k])
# abundance model
N[k] ~ dpois(lambda[k]) # predicted abundance per survey (point-year)
# covariates of abundance***
log(lambda[k]) <- parkeff[Park[k]] + yeareff[Year[k]] + bp[Park[k]]*Year[k] +
traneff[tran[k]] + b2*PASanom[k] + b3*MSTanom[k]
} # k in 1:nsurveys
# GOODNESS OF FIT STATS
for (k in 1:nsurveys) {
n.a.fit[k] ~ dbin(pa[k],N[k]) # create new realization of model
y.fit[k] ~ dbin(pd[k],n.a[k])
e.pd[k] <- pd[k]*n.a[k] # original model prediction
E.pd[k] <- pow((y[k]-e.pd[k]),2)/(e.pd[k]+0.5) #dev from obs
E.New.pd[k] <- pow((y.fit[k]-e.pd[k]),2)/(e.pd[k]+0.5) #dev from pred
e.pa[k] <- pa[k]*N[k]
E.pa[k] <- pow((n.a[k]-e.pa[k]),2)/(e.pa[k]+0.5)
E.New.pa[k] <- pow((n.a.fit[k]-e.pa[k]),2)/(e.pa[k]+0.5)
} # k in 1:nsurveys
fit.pd <- sum(E.pd[]) #dev from obs
fit.new.pd <- sum(E.New.pd[]) #dev from pred
fit.pa <- sum(E.pa[])
fit.new.pa <- sum(E.New.pa[])
# SUMMARY STATS
mupavail <- mean(pa[]) # mean probability of availability
mupdet <- mean(pd[]) # mean probability of perceptibility
musigma <- mean(sigma[]) # mean scale parameter across sites
bayesp.pd <- step(fit.new.pd-fit.pd) # p-value for perceptibility model
bayesp.pa <- step(fit.new.pa-fit.pa) # p-value for availability model
totN.m <- sum(N[mora]) # population size of area surveyed in MORA
totN.n <- sum(N[noca])
totN.o <- sum(N[olym])
totN <- sum(N[])
for (i in 1:10) { # N per year and park-year
N.yr.m[i] <- sum(N[yr.m[,i]]) #in MORA
N.yr.n[i] <- sum(N[yr.n[,i]])
59
N.yr.o[i] <- sum(N[yr.o[,i]])
N.yr[i] <- sum(N[yr[,i]])
}
muN.mora <- mean(N[mora]) # mean point-level abundance in MORA
muN.noca <- mean(N[noca])
muN.olym <- mean(N[olym])
muN <- mean(N[])
ha <- maxd*maxd*3.14159/10000 # hectares surveyed at each pt
dens.ha.m <- muN.mora/ha # density
dens.ha.n <- muN.noca/ha
dens.ha.o <- muN.olym/ha
dens.ha <- mean(N[])/ha
} #end: model
",file=file.name) #end: cat
  observer <- as.numeric(droplevels(obs))
  observer[is.na(observer)] <- max(observer,na.rm=T)
  sum(is.na(observer)); unique(observer); hist(observer)
  PTYR <- as.numeric(as.factor(PTYR))
  #create some useful indices to pass to jags
  yr <- matrix(0,length(Year)/10,10); dim(yr); str(yr)
  for (i in 2005:2014) {yr[,(i-2004)] <- which(Year==i)}
  if (park=="all") {
    mora <- which(Park=="MORA"); noca <- which(Park=="NOCA")
    olym <- which(Park=="OLYM")
    yr.m <- matrix(0,length(which(Park=="MORA"))/10,10); dim(yr.m)
    yr.n <- matrix(0,length(which(Park=="NOCA"))/10,10); dim(yr.n)
    yr.o <- matrix(0,length(which(Park=="OLYM"))/10,10); dim(yr.o)
    for (i in 2005:2014) {
      yr.m[,(i-2004)] <- which(Year==i&Park=="MORA")
      yr.n[,(i-2004)] <- which(Year==i&Park=="NOCA")
      yr.o[,(i-2004)] <- which(Year==i&Park=="OLYM")
    }
  } #end: if (park=="all")
  win.data <- list(y=y,
                   surveyid=surveyid,
                   maxd=maxd,
                   dclass=dclass,
                   tinterval=tinterval,
                   nsurveys=nsurveys,
                   nobs=nobs,
                   nbreaks=nbreaks,
                   delta=delta,
                   mdpts=mdpts,
                   J=J,
                   ntrans=ntrans,
                   tran=as.numeric(tran),
                   nobservers=nobservers,
                   observer=observer, #as.numeric(droplevels(obs)),
                   PTYR=PTYR,
                   day=as.vector(scale(day)),
                   forest=forest,
                   dense=dense,
                   elev=as.vector(scale(elev)),
                   noise=as.vector(scale(noise)),
                   hour=as.vector(scale(hour)),
                   slope=as.vector(scale(slope)),
                   aspect=as.vector(scale(aspect.b)),
                   Park=as.numeric(Park), # factors 1=MORA, 2=NOCA, 3=OLYM
                   60
                   nyears=nyears,
                   Year=Year-2004,
                   PASanom=as.vector(scale(PASanom)),
                   MSTanom=as.vector(scale(MSTanom)),
                   yr=yr, # indices for Year
                   mora=mora, noca=noca, olym=olym, # indices for N in each park
                   yr.m=yr.m, yr.n=yr.n, yr.o=yr.o #indices for Park-Year
  )
  inits <- function() list(N=Nst) #,a0=runif(1,-1,1))
  params <- c("musigma",
              "mupdet",
              "mupavail",
              "bayesp.pd",
              "bayesp.pa",
              "sigma0",
              #"obseff",
              #"sd.obs",
              #"sd.py.pd",
              #"sd.yr.pd",
              #"sd.tr.pd",
              #"b.p1", #"b.p2",
              "a0",
              #"b.a1", #"b.a2",
              #"b0","b1",
              "b2","b3", #"b4",
              "parkeff",
              "yeareff",
              "sd.year",
              "bp",
              "muN.mora","muN.noca","muN.olym",
              "sd.tran",
              "totN.m","totN.n","totN.o",
              "totN",
              "dens.ha.m","dens.ha.n", "dens.ha.o",
              "dens.ha",
              "N.yr.m","N.yr.n","N.yr.o",
              "fit.pd","fit.new.pd","fit.pa","fit.new.pa",
              "N.yr"
  )
  ni <- 40000
  nt <- 30
  nb <- 10000
  nc <- 3
  # <<<<<update jags output file name as needed
    system.time(OSFLout1.jags <- jags(win.data,inits,params,file.name,n.chains=nc,
                                      n.thin=nt,n.iter=ni,n.burnin=nb,parallel=TRUE))
  sink(file=paste(outfile,".txt",sep=""),split=T)
  print("Fitted models of detection, abundance")
  print(paste("Maximum detection distance (maxd) =",maxd))
  print(paste("% discarded to ensure independence between detection interval",
"& distance (truncPct) =",truncPct))
  print(paste("Finding global effects and trends with climate effects"))
  # <<<<<update models as needed
    print("#log(lambda[k]) <- parkeff[Park[k]] + yeareff[Year[k]] + bp[Park[k]]*Year[k] +
traneff[tran[k]] + b2*PASanom[k] + b3*MSTanom[k] # + b4*PASanom[k]*MSTanom[k]")
  print("#log(sigma[k]) <- log(sigma0)") #+ ptyreff.pd[PTYR[k]]") #+ obseff[observer[k]]
  print("#logit(q[k]) <- a0")
  61
  # <<<<<update jags output file name as needed
    print(OSFLout1.jags,digits=3)
  sink()
  #evaluate fit
  e.obs.pd <- OSFLout1.jags$sims.list$fit.pd
  e.pred.pd <- OSFLout1.jags$sims.list$fit.new.pd
  mu.bayespd <- mean(OSFLout1.jags$sims.list$bayesp.pd)
  e.obs.pa <- OSFLout1.jags$sims.list$fit.pa
  e.pred.pa <- OSFLout1.jags$sims.list$fit.new.pa
  mu.bayespa <- mean(OSFLout1.jags$sims.list$bayesp.pa)
  mean(e.pred.pd > e.obs.pd) # SA bayesp.pd
  mean(e.obs.pd/e.pred.pd)
  mean(e.pred.pa > e.obs.pa) # SA bayesp.pd
  mean(e.obs.pa/e.pred.pa)
  par(mfrow=c(2,2))
  ppi <- 300
  jpeg(file=paste(outfile,".fitStats-%d.jpg",sep=""),width=6*ppi,height=6*ppi,res=ppi)
  plot(e.obs.pd,e.pred.pd,frame.plot=F,main=paste(species,"detected"),cex.lab=1.25,
       xlab="Discrepancy of observed data",ylab="Discrepancy of simulated data")
  abline(0,1,lwd=2)
  mu.obs <- mean(e.obs.pd); mu.pred <- mean(e.pred.pd)
  legend(min(e.obs.pd),max(e.pred.pd),title="Mean discrepancy",bty="n",
         legend=c(paste("Obs:",round(mu.obs)),paste("Sim:",round(mu.pred))))
  legend(max(e.obs.pd),min(e.pred.pd),xjust=1,yjust=0,bty="n",
         legend=c(paste("Bayes p:",round(mu.bayespd,3)),
                  paste("LOF:",round(mean(e.obs.pd/e.pred.pd),3))))
  plot(e.obs.pa,e.pred.pa,frame.plot=F,main=paste(species,"available"),cex.lab=1.25,
       xlab="Discrepancy of observed data",ylab="Discrepancy of simulated data")
  abline(0,1,lwd=2)
  mu.obs <- mean(e.obs.pa); mu.pred <- mean(e.pred.pa)
  legend(min(e.obs.pa),max(e.pred.pa),title="Mean discrepancy",bty="n",
         legend=c(paste("Obs:",round(mu.obs)),paste("Sim:",round(mu.pred))))
  legend(max(e.obs.pa),min(e.pred.pa),xjust=1,yjust=0,bty="n",
         legend=c(paste("Bayes p:",round(mu.bayespa,3)),
                  paste("LOF:",round(mean(e.obs.pa/e.pred.pa),3))))
  dev.off()
  pdf(file=paste(outfile,"Chains.pdf",sep="")) #save all plots in 1 hi-res file
  plot(OSFLout1.jags,ask=FALSE)
  print("ok") #could use 30 of these to handle "Hit <Return> to see next plot:"
  dev.off()
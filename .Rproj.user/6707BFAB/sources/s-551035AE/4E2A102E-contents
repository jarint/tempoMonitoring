newFrame <- function(){
  ######################################################################################
  #install tcltk
  if (require("tcltk")){
    install.packages("tcltk")
    library(tcltk)
  }
  ######################################################################################
  #randomization
  fileSelector = tk_messageBox(type = "okcancel", message = "Choose your Randomization CSV file (Plate 1).",
                               caption = "Enter Randomization CSV")
  if (fileSelector == "ok"){
    rdm <- file.choose()
    outPath <<- dirname(rdm)
    rdm <- read.csv(rdm, header = T)
  } else {
    tk_messageBox(type = 'ok', message = "You must choose a randomization file (plate 1 in DFExplore).",
                  icon = "warning", caption = "Error")
    break;
  }
  ######################################################################################
  #imaging form
  fileSelector = tk_messageBox(type = "okcancel", message = "Please select your Imaging Form CSV file (Plate 16).",
                               caption = "Enter Imaging Form CSV")
  if (fileSelector == "ok"){
    imgf <- file.choose()
    imgf <- read.csv(imgf, header = T)
  } else {
    tk_messageBox(type = 'ok', message = "You must choose an Imaging Form file (plate 16 in DFExplore).",
                  icon = "warning", caption = "Error")
    break;
  }
  ######################################################################################
  #Baseline CT Form
  fileSelector = tk_messageBox(type = "okcancel", message = "Please select your Baseline CT/CTA Site Assessment CSV File (Plate 12).",
                               caption = "Enter Baseline CT/CTA Site Assessment CSV")
  if (fileSelector == "ok"){
    blct <- file.choose()
    blct <- read.csv(blct, header = T)
  } else {
    tk_messageBox(type = 'ok', message = "You must choose a Baseline CT/CTA Site Assessment file (plate 12 in DFExplore).",
                  icon = "warning", caption = "Error")
    break;
  }
  ######################################################################################
  #Treatment Assignment Form
  fileSelector = tk_messageBox(type = "okcancel", message = "Please select a Treatment Assignment CSV File (Plate 13).",
                               caption = "Enter Treatment Assignment CSV")
  if (fileSelector == "ok"){
    ta <- file.choose()
    ta <- read.csv(ta, header = T)
  } else {
    tk_messageBox(type = 'ok', message = "You must choose a Treatment Assignment file (plate 13 in DFExplore).",
                  icon = "warning", caption = "Error")
    break;
  }
  ######################################################################################
  #AOL Score Form
  fileSelector = tk_messageBox(type = "okcancel", message = "Please select an AOL Score CSV File (Plate 4 Tempo Imaging).",
                               caption = "Enter AOL Score CSV")
  if (fileSelector == "ok"){
    p4 <- file.choose()
    p4 <- read.csv(p4, header = T)
  } else {
    tk_messageBox(type = 'ok', message = "You must choose an AOL Score file (plate 4 in Tempo Imagiing Database).",
                  icon = "warning", caption = "Error")
    break;
  }
  ######################################################################################
  #Merge table together
  RandDem <- merge(rdm, imgf, all.x = T, by="ptid")
  ImgBLCT <- merge(imgf,blct, all.x = T, by = "ptid")
  merge1 <- merge(RandDem,ImgBLCT,all.x=T,by="ptid")
  tableForUse <- merge(merge1,ta,all.x=T,by="ptid")

  ######################################################################################
  #Return to Caller
  tk_messageBox(type = 'ok', message = "Finished Successfully.", caption = "Success")
  p4 <<- as.data.frame(p4)
  return(as.data.frame(tableForUse))

  ######################################################################################
}

assemble <- function(){
  if(require("rstudioapi")){
    print("rstudioapi is loaded correctly")
  } else {
    print("Trying to Install rstudioapi")
    install.packages("rstudioapi")
    if(require("rstudioapi")){
      print("rstudioapi installed and loaded")
    } else {
      stop("Could not install rstudioapi")
    }
  }
  ######################################################################################
  #randomization
  fileSelector = rstudioapi::showQuestion(title = "Enter Randomization CSV", message = "Please select your randomization CSV file (Plate 1).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    rdm <- rstudioapi::selectFile("Select your Randomization.CSV file")
    outPath <<- dirname(rdm)
    rdm <- read.csv(rdm, header = T)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a randomization file (plate 1 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #imaging form
  fileSelector = rstudioapi::showQuestion(title = "Enter Imaging Form CSV", message = "Please select your Imaging Form CSV file (Plate 16).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    imgf <- rstudioapi::selectFile("Select your Imaging Form .CSV file")
    imgf <- read.csv(imgf, header = T)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose an imaging file (plate 16 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #Baseline CT Form
  fileSelector = rstudioapi::showQuestion(title = "Enter Baseline CT/CTA Site Assessment CSV", message = "Please select your Baseline CT/CTA Site Assessment CSV File (Plate 12).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    blct <- rstudioapi::selectFile("Select your Baseline CT/CTA .CSV file")
    blct <- read.csv(blct, header = T)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Baseline CT/CTA Site Assesment form (plate 12 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #Treatment Assignment Form
  fileSelector = rstudioapi::showQuestion(title = "Enter Treatment Assignment CSV", message = "Please select a Treatment Assignment CSV File (Plate 13).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    ta <- rstudioapi::selectFile("Select your Treatment Assignment .CSV file")
    ta <- read.csv(ta, header = T)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Treatment Assignment file (plate 13 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #AOL Score Form
  fileSelector = rstudioapi::showQuestion(title = "Enter AOL Score CSV", message = "Please select an AOL Score CSV File (Plate 4 Tempo Imaging).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    p4 <- rstudioapi::selectFile("Select your AOL Score .CSV file")
    p4 <- read.csv(p4, header = T)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose an AOL Score file (plate 4 in TEMPO Imaging database). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #Merge table together
  RandDem <- merge(rdm, imgf, all.x = T, by="ptid")
  ImgBLCT <- merge(imgf,blct, all.x = T, by = "ptid")
  merge1 <- merge(RandDem,ImgBLCT,all.x=T,by="ptid")
  tableForUse <- merge(merge1,ta,all.x=T,by="ptid")
  p4 <- as.data.frame(p4)
  ######################################################################################
  #Return to Caller
  rstudioapi::showDialog(title = "Success", message = "Finished Successfully.")
  p4 <<-(as.data.frame(p4))
  Frame <<-(as.data.frame(tableForUse))
}

Tempo2 <- function(frame = Frame, site = 0, output = outPath){
  if (site == 0 && exists("frame")){
    setwd(output)
    Table1 <- frame
    siteList <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,24,25,26,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45,46,47,60,61,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,87,88,89,91,92,93,94,95,96)
    master<-Table1[FALSE, ]
    for (i in siteList){
      upper = (i*1000)+999
      lower = i*1000
      for (row in 1:nrow(Table1)){
        for (j in lower:upper){
          if (Table1$ptid[row] == j){
            master <- rbind(master,Table1[row, ])
          }
        }
      }
    }
    Table1 <- master
    n = paste("(n = ",as.character(length(Table1$ptid)),")", sep = "")
    nControl = paste("(n = ",as.character(length(Table1$ptid)-sum(Table1$treatment)),")", sep = "")
    nTNK = paste("(n = ",as.character(sum(Table1$treatment)),")", sep = "")
    pdftitle = "All Sites "
    pdfdate = Sys.Date()
    pdf(paste(pdftitle, pdfdate, ".pdf", sep = ""), paper = "letter", pagecentre = T)

    #
    #
    #
    #
    #

    #########################################################
    #subjects recruited by month - boxplot
    DateTable = Table1
    DateTable$rdm_date <- as.Date(DateTable$rdm_date, format="%Y/%m/%d")
    tab <- table(cut(DateTable$rdm_date, 'month'))
    DatesByMonth <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'),
                               Frequency=as.vector(tab))
    boxplot(DatesByMonth$Frequency,main = paste("Subjects Recruited by Month - All Sites","\n",n),
            ylab = "Number of Subjects", axis.lty = 1, horizontal = F)
    text(y=fivenum(DatesByMonth$Frequency), labels = fivenum(DatesByMonth$Frequency), x=1.25)

    #########################################################
    #Imaging Modality

    #piechart
    ImgTable = Table1
    imgmod <- table(ImgTable$image_rdm)
    rownames(imgmod)<-c("spCTA","mCTA","CTP","MRA")
    piepercent<- (round(imgmod/sum(imgmod), 5)*100)
    piepercent<-signif(piepercent, digits = 3)
    piepercent <- as.character(piepercent)
    percent = "%"
    for (character in 1:length(piepercent)){
      piepercent[character] <- paste(piepercent[character], percent)
    }
    cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
    pie(imgmod, main = paste("Imaging Modality Used to Randomize - All Sites","\n",n), labels = piepercent, clockwise = T,col = cols)
    legend("topright", c("spCTA","mCTA","CTP","MRA"), fill = cols)

    #stacked barplot
    ImgTable = Table1
    imgmod <- table(ImgTable$image_rdm)
    rownames(imgmod)<-c("spCTA","mCTA","CTP","MRA")
    imgmod<-t(imgmod)
    imgmod<-t(imgmod)
    cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
    xs<-barplot(imgmod, legend =F,col = cols,
                ylim = range(pretty(c(0,length(Table1$ptid)))),
                main = paste("Imaging Modality Used to Randomize - All Sites","\n",n))
    yvals<-apply(imgmod,2,cumsum)
    yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:4,]/2
    text(xs,yv2, rownames(yvals), cex = 1.0)

    #########################################################
    #TNK vs Control
    TNK = sum(Table1$treatment)/length(Table1$ptid)
    CONTROL = 1-TNK
    x<-c(TNK,CONTROL)
    piepercent<- round(100*x/sum(x), 5)
    piepercent<-signif(piepercent, digits = 3)
    piepercent <- as.character(piepercent)
    percent = "%"
    for (character in 1:length(piepercent)){
      piepercent[character] <- paste(piepercent[character], percent)
    }
    cols = c("#E0F3DB", "#4EB3D3")
    pie(x, labels = piepercent, main = paste("TNK vs Control - All Sites","\n",n), clockwise = T, col = cols)
    legend("topright", c("TNK","Control"), fill = cols)

    #########################################################
    #Time of Onset to TNK/Control
    onsettoTNK<-vector()
    onsettoCTRL <- vector()
    for (row in 1:nrow(Table1)){
      if (Table1$treatment[row] == 1 && Table1$ta_tnk_date_1stdose[row] != "" && !is.na(Table1$ta_tnk_date_1stdose[row]) && !is.null(Table1$ta_tnk_date_1stdose[row]) && Table1$rdm_onset_date[row] != "" && !is.na(Table1$rdm_onset_date[row]) && !is.null(Table1$rdm_onset_date[row])){
        if (Table1$rdm_onset_date[row] == Table1$ta_tnk_date_1stdose[row]){
          difference4 = (Table1$ta_tnk_time_1stdose[row] - Table1$rdm_onset_time[row])/100
          onsettoTNK <- c(onsettoTNK, difference4)
        } else {
          difference4 = ((2400-Table1$rdm_onset_time[row])+as.integer(as.character(Table1$ta_tnk_time_1stdose[row]))/100)
          onsettoTNK <- c(onsettoTNK, difference4)
        }
      }
      if (Table1$treatment[row] == 0 && Table1$ta_c_date_1stdose[row] != "" && !is.na(Table1$ta_c_date_1stdose[row]) && !is.null(Table1$ta_c_date_1stdose[row]) && Table1$rdm_onset_date[row] != "" && !is.na(Table1$rdm_onset_date[row]) && !is.null(Table1$rdm_onset_date[row])){
        if (Table1$rdm_onset_date[row] == Table1$ta_c_date_1stdose[row]){
          difference5 = (as.integer(as.character(Table1$ta_c_time_1stdose[row])) - Table1$rdm_onset_time[row])/100
          onsettoCTRL <- c(onsettoCTRL, difference5)
        } else {
          difference5 = ((2400-Table1$rdm_onset_time[row])+as.integer(as.character(Table1$ta_c_time_1stdose[row]))/100)
          onsettoCTRL <- c(onsettoCTRL, difference5)
        }
      }
    }

    for (i in 1:length(onsettoCTRL)){
      if (!is.na(onsettoCTRL[i]) && onsettoCTRL[i] < 0){
        onsettoCTRL[i] <- NA
      }
    }

    aa = data.frame(group = paste("\n","Time from Onset to Control","\n",nControl), value = onsettoCTRL)
    bb = data.frame(group = paste("\n","Time from Onset to TNK","\n",nTNK), value = onsettoTNK)
    aabb = rbind(aa,bb)
    boxplot(aabb$value~aabb$group, na.rm = T, main = "Time from Onset to TNK/Control - All Sites",
            ylab = "Time (Hours)", ylim = c(0,20), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
    text(y=fivenum(onsettoCTRL), labels = fivenum(onsettoCTRL), x=1.5)
    text(y=fivenum(onsettoTNK), labels = fivenum(onsettoTNK), x= 2.5)

    #########################################################
    #Time of Onset to Randomization
    timeonsettocontrol<-vector()
    for (row in 1:nrow(Table1)){
      if (Table1$rdm_date[row] == Table1$rdm_onset_date[row]){
        difference = (Table1$rdm_time[row]-Table1$rdm_onset_time[row])/100
        timeonsettocontrol <- c(timeonsettocontrol,difference)
      } else {
        difference = ((2400-Table1$rdm_onset_time[row])+Table1$rdm_time[row])/100
        timeonsettocontrol <- c(timeonsettocontrol,difference)
      }
    }
    Table1$timeonsettocontrol<-timeonsettocontrol

    boxplot(timeonsettocontrol, na.rm=T, main = paste("Time from Onset to Randomization - All Sites","\n",n),
            ylab = "Time (Hours)", horizontal = F, staplewex = 1)
    text(y=fivenum(timeonsettocontrol), labels = fivenum(timeonsettocontrol), x=1.25)

    #########################################################
    #Time from CT to TNK/CTRL
    timeBLCTtoTNK<-vector()
    for (row in 1:nrow(Table1)){
      if (Table1$treatment[row] == 1 && !is.null(Table1$bl_ct_date.x[row]) && !is.na(Table1$bl_ct_date.x[row]) && Table1$bl_ct_date.x[row] != "" && !is.null(Table1$ta_tnk_date_1stdose[row]) && !is.na(Table1$ta_tnk_date_1stdose[row]) && Table1$ta_tnk_date_1stdose[row] != ""){
        if (Table1$bl_ct_date.x[row] == Table1$ta_tnk_date_1stdose[row]){
          difference2 = (as.integer(Table1$ta_tnk_time_1stdose[row])-as.integer(Table1$bl_ct_time.x[row]))/100
          timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
        } else {
          difference2 = ((2400-as.integer(Table1$bl_ct_time.x[row]))+as.integer(Table1$ta_tnk_time_1stdose[row]))/100
          timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
        }
      }
    }

    timeBLCTtoCTRL<-vector()
    for (row in 1:nrow(Table1)){
      if (Table1$treatment[row] == 0 && !is.null(Table1$bl_ct_date.x[row]) && !is.na(Table1$bl_ct_date.x[row]) && (Table1$bl_ct_date.x[row] != "") && !is.null(Table1$ta_c_date_1stdose[row]) && !is.na(Table1$ta_c_date_1stdose[row]) && (Table1$ta_c_date_1stdose[row] != "") && (Table1$ta_c_time_1stdose[row] != "") && !is.null(Table1$ta_c_time_1stdose) && !is.na(Table1$ta_c_time_1stdose[row])){
        if (Table1$bl_ct_date.x[row] == Table1$ta_c_date_1stdose[row]){
          difference3 = (as.integer(as.character(Table1$ta_c_time_1stdose[row]))-as.integer(Table1$bl_ct_time.x[row]))/100
          timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
        } else if (Table1$bl_ct_date.x[row] != Table1$ta_c_date_1stdose[row]) {
          difference3 = ((2400-as.integer(Table1$bl_ct_time.x[row]))+as.integer(as.character(Table1$ta_c_time_1stdose[row])))/100
          timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
        }
      }
    }

    for (i in 1:length(timeBLCTtoCTRL)){
      if (!is.na(timeBLCTtoCTRL[i]) && timeBLCTtoCTRL[i] < 0){
        timeBLCTtoCTRL[i] <- NA
      }
    }

    aa = data.frame(group = paste("\n","Time from Baseline CT to Control","\n",nControl), value = timeBLCTtoCTRL)
    bb = data.frame(group = paste("\n","Time from Baseline CT to TNK","\n",nTNK), value = timeBLCTtoTNK)
    aabb = rbind(aa,bb)
    boxplot(aabb$value~aabb$group, na.rm = T, main = "Time from Baseline CT to TNK/Control - All Sites",
            ylab = "Time (Hours)", ylim = c(0,4), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
    text(y=fivenum(timeBLCTtoCTRL), labels = fivenum(timeBLCTtoCTRL), x=1.5)
    text(y=fivenum(timeBLCTtoTNK), labels = fivenum(timeBLCTtoTNK), x= 2.5)

    #########################################################
    #NIHSS
    x<-as.vector(Table1$rdm_nihss)
    remove<-c(99)
    x<-x [! x %in% remove]#gets rid of incorrect values
    boxplot(x, na.rm=T, main = paste("NIHSS Total Score (Baseline) - All Sites","\n",n), ylab = "Score", horizontal = F, staplewex = 1, las = 1)
    text(y=fivenum(x), labels = fivenum(x), x=1.25)

    #########################################################
    #AGE
    age_vec <- as.data.frame(Table1$rdm_dob)
    age_vec<-as.Date(paste(as.character(age_vec$`Table1$rdm_dob`), '01'), format = '%Y%m%d')
    z<-c((Sys.Date()-age_vec)/365)
    z<-signif(z, digits = 3)
    z<-as.numeric(z)
    boxplot(z, na.rm = T, ylab = "Age (Years)", horizontal = F, staplewex = 1, main = paste("Age of TEMPO-2 Enrollees - All Sites","\n",n))
    text(y=fivenum(z), labels = fivenum(z), x = 1.25)

    #########################################################
    #SEX
    SEX_M = sum(Table1$rdm_sex)/length(Table1$ptid)
    SEX_F = 1-SEX_M
    y <- c(SEX_F,SEX_M)
    piepercent<- round(100*y/sum(y), 5)
    piepercent<-signif(piepercent, digits = 3)
    piepercent <- as.character(piepercent)
    percent = "%"
    for (character in 1:length(piepercent)){
      piepercent[character] <- paste(piepercent[character], percent)
    }
    cols = c("#E0F3DB", "#4EB3D3")
    pie(y, labels = piepercent, main = paste("Sex of TEMPO-2 Enrollees - All Sites","\n",n),
        clockwise = T, col = cols)
    legend("topright", c("Female","Male"), fill = cols)

    #########################################################
    #occlusion location - base data
    ICA = 0
    M1 = 0
    M2 = 0
    M3 = 0
    VA = 0
    P1 = 0
    P2 = 0
    A1 = 0
    A2 = 0
    PICA = 0
    BA = 0
    for (row in 1:nrow(Table1)){
      if (!is.na(Table1$occl_location[row])){
        if (Table1$occl_location[row] == 1 || Table1$occl_location[row] == 2){
          ICA <- ICA + 1
        }
        if (Table1$occl_location[row] == 3 || Table1$occl_location[row] == 4){
          M1 <- M1 + 1
        }
        if (Table1$occl_location[row] == 5 || Table1$occl_location[row] == 6){
          M2 <- M2 + 1
        }
        if (Table1$occl_location[row] == 7 || Table1$occl_location[row] == 8){
          M3 <- M3 + 1
        }
        if (Table1$occl_location[row] == 9 || Table1$occl_location[row] == 10){
          VA <- VA + 1
        }
        if (Table1$occl_location[row] == 11 || Table1$occl_location[row] == 12){
          P1 <- P1 + 1
        }
        if (Table1$occl_location[row] == 13 || Table1$occl_location[row] == 14){
          P2 <- P2 + 1
        }
        if (Table1$occl_location[row] == 15 || Table1$occl_location[row] == 16){
          A1 <- A1 + 1
        }
        if (Table1$occl_location[row] == 17 || Table1$occl_location[row] == 18){
          A2 <- A2 + 1
        }
        if (Table1$occl_location[row] == 19 || Table1$occl_location[row] == 20){
          PICA <- PICA + 1
        }
        if (Table1$occl_location[row] == 99){
          BA <- BA + 1
        }
      }
    }
    #stacked barplot
    occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
    taball<-as.data.frame(occlocation)
    rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
    taball<-t(taball)
    taball<-t(taball)
    cols = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081")
    xs<-barplot(taball, legend =F,col = cols,
                ylim = range(pretty(c(0,length(Table1$ptid)))),
                main = paste("Occlusion Location - All Sites","\n",n), ylab = "Number of Subjects")
    yvals<-apply(taball,2,cumsum)
    yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:11,]/2
    text(xs,yv2, rownames(yvals), cex = .6)

    #standard barplot
    occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
    taball<-as.data.frame(occlocation)
    rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
    taball<-t(taball)
    barplot(taball, main = paste("Occlusion Location - All Sites","\n",n), xlab = "Location", axis.lty = 1,
            ylab = "Number of Cases", las = 1, cex.names = 0.8, ylim = range(pretty(c(0,max(occlocation, na.rm = T)))))

    #########################################################
    #Randomization time to treatment
    timeRDMtoTNK<-vector()
    for (row in 1:nrow(Table1)){
      if (Table1$treatment[row] == 1 && !is.null(Table1$rdm_date[row]) && !is.na(Table1$rdm_date[row]) && Table1$rdm_date[row] != "" && !is.null(Table1$ta_tnk_date_1stdose[row]) && !is.na(Table1$ta_tnk_date_1stdose[row]) && Table1$ta_tnk_date_1stdose[row] != ""){
        if (Table1$rdm_date[row] == Table1$ta_tnk_date_1stdose[row]){
          difference2 = (as.integer(Table1$ta_tnk_time_1stdose[row])-as.integer(Table1$rdm_time[row]))/100
          timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
        } else {
          difference2 = ((2400-as.integer(Table1$rdm_time[row]))+as.integer(Table1$ta_tnk_time_1stdose[row]))/100
          timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
        }
      }
    }
    for (i in 1:length(timeRDMtoTNK)){
      if (!is.na(timeRDMtoTNK[i]) && timeRDMtoTNK[i] < 0){
        timeRDMtoTNK[i] <- NA
      }
    }

    timeRDMtoCTRL<-vector()
    for (row in 1:nrow(Table1)){
      if (Table1$treatment[row] == 0 && !is.null(Table1$rdm_date[row]) && !is.na(Table1$rdm_date[row]) && (Table1$rdm_date[row] != "") && !is.null(Table1$ta_c_date_1stdose[row]) && !is.na(Table1$ta_c_date_1stdose[row]) && (Table1$ta_c_date_1stdose[row] != "") && (Table1$ta_c_time_1stdose[row] != "") && !is.null(Table1$ta_c_time_1stdose) && !is.na(Table1$ta_c_time_1stdose[row])){
        if (Table1$rdm_date[row] == Table1$ta_c_date_1stdose[row]){
          difference3 = (as.integer(as.character(Table1$ta_c_time_1stdose[row]))-as.integer(Table1$rdm_time[row]))/100
          timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
        } else if (Table1$rdm_date[row] != Table1$ta_c_date_1stdose[row]) {
          difference3 = ((2400-as.integer(Table1$rdm_time[row]))+as.integer(as.character(Table1$ta_c_time_1stdose[row])))/100
          timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
        }
      }
    }
    for (i in 1:length(timeRDMtoCTRL)){
      if (!is.na(timeRDMtoCTRL[i]) && timeRDMtoCTRL[i] < 0){
        timeRDMtoCTRL[i] <- NA
      }
    }

    timeRDMtoTNK<-timeRDMtoTNK*60
    timeRDMtoCTRL<-timeRDMtoCTRL*60

    aa = data.frame(group = paste("\n","Randomization to Control","\n",nControl), value = timeRDMtoCTRL)
    bb = data.frame(group = paste("\n","Randomization to TNK","\n",nTNK), value = timeRDMtoTNK)
    aabb = rbind(aa,bb)
    boxplot(aabb$value~aabb$group, na.rm = T, main = "Time from Randomization to TNK/Control - All Sites",
            ylab = "Time (Minutes)", ylim = range(pretty(c(0,max(timeRDMtoTNK, na.rm = T)))), staplewex = 1, las = 1,
            xlab = NULL, par(mgp = c(3,1.5,0)))
    text(y=fivenum(timeRDMtoCTRL), labels = fivenum(timeRDMtoCTRL), x=1.5)
    text(y=fivenum(timeRDMtoTNK), labels = fivenum(timeRDMtoTNK), x= 2.5)

    ##########################################################
    # AOL SCORE FIGURES - All Sites
    nr <- nrow(Frame)
    aol <- (data.frame(matrix(ncol = 4, nrow = nr)))
    colnames(aol) = c("PTID", "SUBJID", "TREAT", "AOL")
    aol$PTID = Frame$ptid
    aol$TREAT = Frame$treatment
    for (row in 1:nrow(aol)) {
      for (r in 1:nrow(p4)){
        if (aol$PTID[row] == p4$usubjid[r]){
          aol$SUBJID[row] <- p4$usubjid[r]
          aol$AOL[row] <- p4$fastresc_aolscore[r]
        }
      }
    }
    CTRL0 = 0
    CTRL1 = 0
    CTRL2A = 0
    CTRL2B = 0
    CTRL3 = 0

    TNK0 = 0
    TNK1 = 0
    TNK2A = 0
    TNK2B = 0
    TNK3 = 0
    for (row in 1:nrow(aol)){
      if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 0){
        if (aol$AOL[row] == 0){
          CTRL0 = CTRL0 + 1
        }
        if (aol$AOL[row] == 1){
          CTRL1 = CTRL1 + 1
        }
        if (aol$AOL[row] == 2){
          CTRL2A = CTRL2A + 1
        }
        if (aol$AOL[row] == 3){
          CTRL2B = CTRL2B + 1
        }
        if (aol$AOL[row] == 4){
          CTRL3 = CTRL3 + 1
        }
      }
      if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 1){
        if (aol$AOL[row] == 0){
          TNK0 = TNK0 + 1
        }
        if (aol$AOL[row] == 1){
          TNK1 = TNK1 + 1
        }
        if (aol$AOL[row] == 2){
          TNK2A = TNK2A + 1
        }
        if (aol$AOL[row] == 3){
          TNK2B = TNK2B + 1
        }
        if (aol$AOL[row] == 4){
          TNK3 = TNK3 + 1
        }
      }
    }
    CTRLTOT = CTRL0 + CTRL1 + CTRL2A + CTRL2B + CTRL3
    TNKTOT = TNK0 + TNK1 + TNK2A + TNK2B + TNK3

    ctrlVals = c(CTRL0, CTRL1, CTRL2A, CTRL2B, CTRL3)
    tnkVals = c(TNK0, TNK1, TNK2A, TNK2B, TNK3)

    ctrldf = as.data.frame(ctrlVals)
    rownames(ctrldf) <- c("0", "1", "2A", "2B", "3")
    ctrldf <- t(ctrldf)
    barplot(ctrldf, main = paste("AOL Scores - Control Group", "\n n =", CTRLTOT), xlab = "AOL SCORE", axis.lty = 1,
            ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(ctrldf, na.rm = T)))))

    tnkdf = as.data.frame(tnkVals)
    rownames(tnkdf) <- c("0", "1", "2A", "2B", "3")
    tnkdf <- t(tnkdf)
    barplot(tnkdf, main = paste("AOL Scores - TNK Group", "\n n =", TNKTOT), xlab = "AOL SCORE", axis.lty = 1,
            ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(tnkdf, na.rm = T)))))
    #
    #
    #
    #
    #

    dev.off()
    print("Finished All-Sites PDF Successfully!")

    #
    #
    #
    #
    #

  }
  if (site != 0 && (typeof(site) == "double" || typeof(site) == "integer") && exists("frame")){
    Table1 <- frame
    lower = (site*1000)
    upper = ((site*1000)+999)
    master<-Table1[FALSE, ]
    for (row in 1:nrow(Table1)){
      for (i in lower:upper){
        if (Table1$ptid[row] == i){
          master<-rbind(master,Table1[row, ])
        }
      }
    }
    if (nrow(master) < 10){
      sitename = as.character(site)
      n = paste("(n = ",as.character(length(master$ptid)),")", sep = "")
      print(paste("Site ",sitename," has too few enrollees ",n,"."," You need at least 10 enrollees to generate graphs.",sep = ""))
    }
    else {
      setwd(output)
      sitename = as.character(site)
      n = paste("(n = ",as.character(length(master$ptid)),")", sep = "")
      nControl = paste("(n = ",as.character(length(master$ptid)-sum(master$treatment)),")", sep = "")
      nTNK = paste("(n = ",as.character(sum(master$treatment)),")", sep = "")
      pdftitle = paste("Site",sitename, sep = "")
      pdfdate = Sys.Date()
      pdf(paste(pdftitle, pdfdate, ".pdf"), paper = "letter", pagecentre = T)

      #
      #
      #
      #
      #

      #########################################################
      #subjects recruited by month - boxplot
      DateTable = master
      DateTable$rdm_date <- as.Date(DateTable$rdm_date, format="%Y/%m/%d")
      tab <- table(cut(DateTable$rdm_date, 'month'))
      DatesByMonth <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'),
                                 Frequency=as.vector(tab))
      boxplot(DatesByMonth$Frequency,main = paste("Subjects Recruited by Month - Site",sitename,"\n",n),
              ylab = "Number of Subjects", axis.lty = 1, horizontal = F)
      text(y=fivenum(DatesByMonth$Frequency), labels = fivenum(DatesByMonth$Frequency), x=1.25)

      #########################################################
      #Imaging Modality

      #piechart
      ImgTable = master
      imgmod <- table(ImgTable$image_rdm)
      for (row in 1:nrow(imgmod)) {
        if (rownames(imgmod)[row] == "1"){
          rownames(imgmod)[row] <- "spCTA"
        }
        if (rownames(imgmod)[row] == "2"){
          rownames(imgmod)[row] <- "mCTA"
        }
        if (rownames(imgmod)[row] == "3"){
          rownames(imgmod)[row] <- "CTP"
        }
        if (rownames(imgmod)[row] == "4"){
          rownames(imgmod)[row] <- "MRA"
        }
      }
      piepercent<- (round(imgmod/sum(imgmod), 5)*100)
      piepercent<-signif(piepercent, digits = 3)
      piepercent <- as.character(piepercent)
      percent = "%"
      for (character in 1:length(piepercent)){
        piepercent[character] <- paste(piepercent[character], percent)
      }
      cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
      pie(imgmod, main = paste("Imaging Modality Used to Randomize - Site",sitename,"\n",n), labels = piepercent, clockwise = T,col = cols)
      legend("topright", c("spCTA","mCTA","CTP","MRA"), fill = cols)

      #stacked barplot
      ImgTable = master
      imgmod <- table(ImgTable$image_rdm)
      for (row in 1:nrow(imgmod)) {
        if (rownames(imgmod)[row] == "1"){
          rownames(imgmod)[row] <- "spCTA"
        }
        if (rownames(imgmod)[row] == "2"){
          rownames(imgmod)[row] <- "mCTA"
        }
        if (rownames(imgmod)[row] == "3"){
          rownames(imgmod)[row] <- "CTP"
        }
        if (rownames(imgmod)[row] == "4"){
          rownames(imgmod)[row] <- "MRA"
        }
      }
      imgmod<-t(imgmod)
      imgmod<-t(imgmod)
      cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
      xs<-barplot(imgmod, legend =F,col = cols,
                  ylim = range(pretty(c(0,length(master$ptid)))),
                  main = paste("Imaging Modality Used to Randomize - Site",sitename,"\n",n))
      yvals<-apply(imgmod,2,cumsum)
      yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:nrow(imgmod),]/2
      text(xs,yv2, rownames(yvals), cex = 1.0)

      #########################################################
      #TNK vs Control
      TNK = sum(master$treatment)/length(master$ptid)
      CONTROL = 1-TNK
      x<-c(TNK,CONTROL)
      piepercent<- round(100*x/sum(x), 5)
      piepercent<-signif(piepercent, digits = 3)
      piepercent <- as.character(piepercent)
      percent = "%"
      for (character in 1:length(piepercent)){
        piepercent[character] <- paste(piepercent[character], percent)
      }
      cols = c("#E0F3DB", "#4EB3D3")
      pie(x, labels = piepercent, main = paste("TNK vs Control - Site",sitename,"\n",n), clockwise = T, col = cols)
      legend("topright", c("TNK","Control"), fill = cols)

      #########################################################
      #Time of Onset to TNK/Control
      onsettoTNK<-vector()
      onsettoCTRL <- vector()
      for (row in 1:nrow(master)){
        if (master$treatment[row] == 1 && master$ta_tnk_date_1stdose[row] != "" && !is.na(master$ta_tnk_date_1stdose[row]) && !is.null(master$ta_tnk_date_1stdose[row]) && master$rdm_onset_date[row] != "" && !is.na(master$rdm_onset_date[row]) && !is.null(master$rdm_onset_date[row])){
          if (master$rdm_onset_date[row] == master$ta_tnk_date_1stdose[row]){
            difference4 = (master$ta_tnk_time_1stdose[row] - master$rdm_onset_time[row])/100
            onsettoTNK <- c(onsettoTNK, difference4)
          } else {
            difference4 = ((2400-master$rdm_onset_time[row])+as.integer(as.character(master$ta_tnk_time_1stdose[row]))/100)
            onsettoTNK <- c(onsettoTNK, difference4)
          }
        }
        if (master$treatment[row] == 0 && master$ta_c_date_1stdose[row] != "" && !is.na(master$ta_c_date_1stdose[row]) && !is.null(master$ta_c_date_1stdose[row]) && master$rdm_onset_date[row] != "" && !is.na(master$rdm_onset_date[row]) && !is.null(master$rdm_onset_date[row])){
          if (master$rdm_onset_date[row] == master$ta_c_date_1stdose[row]){
            difference5 = (as.integer(as.character(master$ta_c_time_1stdose[row])) - master$rdm_onset_time[row])/100
            onsettoCTRL <- c(onsettoCTRL, difference5)
          } else {
            difference5 = ((2400-master$rdm_onset_time[row])+as.integer(as.character(master$ta_c_time_1stdose[row]))/100)
            onsettoCTRL <- c(onsettoCTRL, difference5)
          }
        }
      }

      for (i in 1:length(onsettoCTRL)){
        if (!is.na(onsettoCTRL[i]) && onsettoCTRL[i] < 0){
          onsettoCTRL[i] <- NA
        }
      }

      aa = data.frame(group = paste("\n","Time from Onset to Control","\n",nControl), value = onsettoCTRL)
      bb = data.frame(group = paste("\n","Time from Onset to TNK","\n",nTNK), value = onsettoTNK)
      aabb = rbind(aa,bb)
      boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Onset to TNK/Control - Site",sitename),
              ylab = "Time (Hours)", ylim = c(0,20), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
      text(y=fivenum(onsettoCTRL), labels = fivenum(onsettoCTRL), x=1.5)
      text(y=fivenum(onsettoTNK), labels = fivenum(onsettoTNK), x= 2.5)

      #########################################################
      #Time of Onset to Randomization
      timeonsettocontrol<-vector()
      for (row in 1:nrow(master)){
        if (master$rdm_date[row] == master$rdm_onset_date[row]){
          difference = (master$rdm_time[row]-master$rdm_onset_time[row])/100
          timeonsettocontrol <- c(timeonsettocontrol,difference)
        } else {
          difference = ((2400-master$rdm_onset_time[row])+master$rdm_time[row])/100
          timeonsettocontrol <- c(timeonsettocontrol,difference)
        }
      }
      master$timeonsettocontrol<-timeonsettocontrol

      boxplot(timeonsettocontrol, na.rm=T, main = paste("Time from Onset to Randomization - Site",sitename,"\n",n),
              ylab = "Time (Hours)", horizontal = F, staplewex = 1)
      text(y=fivenum(timeonsettocontrol), labels = fivenum(timeonsettocontrol), x=1.25)

      #########################################################
      #Time from CT to TNK/CTRL
      timeBLCTtoTNK<-vector()
      for (row in 1:nrow(master)){
        if (master$treatment[row] == 1 && !is.null(master$bl_ct_date.x[row]) && !is.na(master$bl_ct_date.x[row]) && master$bl_ct_date.x[row] != "" && !is.null(master$ta_tnk_date_1stdose[row]) && !is.na(master$ta_tnk_date_1stdose[row]) && master$ta_tnk_date_1stdose[row] != ""){
          if (master$bl_ct_date.x[row] == master$ta_tnk_date_1stdose[row]){
            difference2 = (as.integer(master$ta_tnk_time_1stdose[row])-as.integer(master$bl_ct_time.x[row]))/100
            timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
          } else {
            difference2 = ((2400-as.integer(master$bl_ct_time.x[row]))+as.integer(master$ta_tnk_time_1stdose[row]))/100
            timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
          }
        }
      }

      timeBLCTtoCTRL<-vector()
      for (row in 1:nrow(master)){
        if (master$treatment[row] == 0 && !is.null(master$bl_ct_date.x[row]) && !is.na(master$bl_ct_date.x[row]) && (master$bl_ct_date.x[row] != "") && !is.null(master$ta_c_date_1stdose[row]) && !is.na(master$ta_c_date_1stdose[row]) && (master$ta_c_date_1stdose[row] != "") && (master$ta_c_time_1stdose[row] != "") && !is.null(master$ta_c_time_1stdose) && !is.na(master$ta_c_time_1stdose[row])){
          if (master$bl_ct_date.x[row] == master$ta_c_date_1stdose[row]){
            difference3 = (as.integer(as.character(master$ta_c_time_1stdose[row]))-as.integer(master$bl_ct_time.x[row]))/100
            timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
          } else if (master$bl_ct_date.x[row] != master$ta_c_date_1stdose[row]) {
            difference3 = ((2400-as.integer(master$bl_ct_time.x[row]))+as.integer(as.character(master$ta_c_time_1stdose[row])))/100
            timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
          }
        }
      }

      for (i in 1:length(timeBLCTtoCTRL)){
        if (!is.na(timeBLCTtoCTRL[i]) && timeBLCTtoCTRL[i] < 0){
          timeBLCTtoCTRL[i] <- NA
        }
      }

      aa = data.frame(group = paste("\n","Time from Baseline CT to Control","\n",nControl), value = timeBLCTtoCTRL)
      bb = data.frame(group = paste("\n","Time from Baseline CT to TNK","\n",nTNK), value = timeBLCTtoTNK)
      aabb = rbind(aa,bb)
      boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Baseline CT to TNK/Control - Site",sitename),
              ylab = "Time (Hours)", ylim = c(0,4), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
      text(y=fivenum(timeBLCTtoCTRL), labels = fivenum(timeBLCTtoCTRL), x=1.5)
      text(y=fivenum(timeBLCTtoTNK), labels = fivenum(timeBLCTtoTNK), x= 2.5)

      #########################################################
      #NIHSS
      x<-as.vector(master$rdm_nihss)
      remove<-c(99)
      x<-x [! x %in% remove]#gets rid of incorrect values
      boxplot(x, na.rm=T, main = paste("NIHSS Total Score (Baseline) - Site",sitename,"\n",n), ylab = "Score", horizontal = F, staplewex = 1, las = 1)
      text(y=fivenum(x), labels = fivenum(x), x=1.25)

      #########################################################
      #AGE
      age_vec <- as.data.frame(master$rdm_dob)
      age_vec<-as.Date(paste(as.character(age_vec$`master$rdm_dob`), '01'), format = '%Y%m%d')
      z<-c((Sys.Date()-age_vec)/365)
      z<-signif(z, digits = 3)
      z<-as.numeric(z)
      boxplot(z, na.rm = T, ylab = "Age (Years)", horizontal = F, staplewex = 1, main = paste("Age of TEMPO-2 Enrollees - Site",sitename,"\n",n))
      text(y=fivenum(z), labels = fivenum(z), x = 1.25)

      #########################################################
      #SEX
      SEX_M = sum(master$rdm_sex)/length(master$ptid)
      SEX_F = 1-SEX_M
      y <- c(SEX_F,SEX_M)
      piepercent<- round(100*y/sum(y), 5)
      piepercent<-signif(piepercent, digits = 3)
      piepercent <- as.character(piepercent)
      percent = "%"
      for (character in 1:length(piepercent)){
        piepercent[character] <- paste(piepercent[character], percent)
      }
      cols = c("#E0F3DB", "#4EB3D3")
      pie(y, labels = piepercent, main = paste("Sex of TEMPO-2 Enrollees - Site",sitename,"\n",n),
          clockwise = T, col = cols)
      legend("topright", c("Female","Male"), fill = cols)

      #########################################################
      #occlusion location - base data
      ICA = 0
      M1 = 0
      M2 = 0
      M3 = 0
      VA = 0
      P1 = 0
      P2 = 0
      A1 = 0
      A2 = 0
      PICA = 0
      BA = 0
      for (row in 1:nrow(master)){
        if (!is.na(master$occl_location[row])){
          if (master$occl_location[row] == 1 || master$occl_location[row] == 2){
            ICA <- ICA + 1
          }
          if (master$occl_location[row] == 3 || master$occl_location[row] == 4){
            M1 <- M1 + 1
          }
          if (master$occl_location[row] == 5 || master$occl_location[row] == 6){
            M2 <- M2 + 1
          }
          if (master$occl_location[row] == 7 || master$occl_location[row] == 8){
            M3 <- M3 + 1
          }
          if (master$occl_location[row] == 9 || master$occl_location[row] == 10){
            VA <- VA + 1
          }
          if (master$occl_location[row] == 11 || master$occl_location[row] == 12){
            P1 <- P1 + 1
          }
          if (master$occl_location[row] == 13 || master$occl_location[row] == 14){
            P2 <- P2 + 1
          }
          if (master$occl_location[row] == 15 || master$occl_location[row] == 16){
            A1 <- A1 + 1
          }
          if (master$occl_location[row] == 17 || master$occl_location[row] == 18){
            A2 <- A2 + 1
          }
          if (master$occl_location[row] == 19 || master$occl_location[row] == 20){
            PICA <- PICA + 1
          }
          if (master$occl_location[row] == 99){
            BA <- BA + 1
          }
        }
      }
      #stacked barplot
      occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
      taball<-as.data.frame(occlocation)
      rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
      newRowNames = vector()
      removeList = vector()
      for(row in rownames(taball)){
        if (taball[row,] != 0){
          newRowNames<-c(newRowNames, row)
        }
      }
      for(row in 1:nrow(taball)){
        if (taball[row,] == 0){
          removeList<-c(removeList, as.integer(row))
        }
      }
      taball<-taball[-c(removeList),]
      taball <- as.data.frame(taball)
      rownames(taball)<-newRowNames
      taball<-t(taball)
      taball<-t(taball)
      cols = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081")
      xs<-barplot(taball, legend =F,col = cols,
                  ylim = range(pretty(c(0,length(master$ptid)))),
                  main = paste("Occlusion Location - Site",sitename,"\n",n), ylab = "Number of Subjects")
      yvals<-apply(taball,2,cumsum)
      yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:nrow(taball),]/2
      text(xs,yv2, rownames(yvals), cex = .6)

      #standard barplot
      occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
      taball<-as.data.frame(occlocation)
      rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
      taball<-t(taball)
      barplot(taball, main = paste("Occlusion Location - Site",sitename,"\n",n), xlab = "Location", axis.lty = 1,
              ylab = "Number of Cases", las = 1, cex.names = 0.8, ylim = range(pretty(c(0,max(occlocation, na.rm = T)))))

      #########################################################
      #Randomization time to treatment
      timeRDMtoTNK<-vector()
      for (row in 1:nrow(master)){
        if (master$treatment[row] == 1 && !is.null(master$rdm_date[row]) && !is.na(master$rdm_date[row]) && master$rdm_date[row] != "" && !is.null(master$ta_tnk_date_1stdose[row]) && !is.na(master$ta_tnk_date_1stdose[row]) && master$ta_tnk_date_1stdose[row] != ""){
          if (master$rdm_date[row] == master$ta_tnk_date_1stdose[row]){
            difference2 = (as.integer(master$ta_tnk_time_1stdose[row])-as.integer(master$rdm_time[row]))/100
            timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
          } else {
            difference2 = ((2400-as.integer(master$rdm_time[row]))+as.integer(master$ta_tnk_time_1stdose[row]))/100
            timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
          }
        }
      }
      for (i in 1:length(timeRDMtoTNK)){
        if (!is.na(timeRDMtoTNK[i]) && timeRDMtoTNK[i] < 0){
          timeRDMtoTNK[i] <- NA
        }
      }

      timeRDMtoCTRL<-vector()
      for (row in 1:nrow(master)){
        if (master$treatment[row] == 0 && !is.null(master$rdm_date[row]) && !is.na(master$rdm_date[row]) && (master$rdm_date[row] != "") && !is.null(master$ta_c_date_1stdose[row]) && !is.na(master$ta_c_date_1stdose[row]) && (master$ta_c_date_1stdose[row] != "") && (master$ta_c_time_1stdose[row] != "") && !is.null(master$ta_c_time_1stdose) && !is.na(master$ta_c_time_1stdose[row])){
          if (master$rdm_date[row] == master$ta_c_date_1stdose[row]){
            difference3 = (as.integer(as.character(master$ta_c_time_1stdose[row]))-as.integer(master$rdm_time[row]))/100
            timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
          } else if (master$rdm_date[row] != master$ta_c_date_1stdose[row]) {
            difference3 = ((2400-as.integer(master$rdm_time[row]))+as.integer(as.character(master$ta_c_time_1stdose[row])))/100
            timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
          }
        }
      }
      for (i in 1:length(timeRDMtoCTRL)){
        if (!is.na(timeRDMtoCTRL[i]) && timeRDMtoCTRL[i] < 0){
          timeRDMtoCTRL[i] <- NA
        }
      }

      timeRDMtoTNK<-timeRDMtoTNK*60
      timeRDMtoCTRL<-timeRDMtoCTRL*60

      aa = data.frame(group = paste("\n","Randomization to Control","\n",nControl), value = timeRDMtoCTRL)
      bb = data.frame(group = paste("\n","Randomization to TNK","\n",nTNK), value = timeRDMtoTNK)
      aabb = rbind(aa,bb)
      boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Randomization to TNK/Control - Site",sitename),
              ylab = "Time (Minutes)", ylim = range(pretty(c(0,max(timeRDMtoCTRL, na.rm = T)))), staplewex = 1, las = 1,
              xlab = NULL, par(mgp = c(3,1.5,0)))
      text(y=fivenum(timeRDMtoCTRL), labels = fivenum(timeRDMtoCTRL), x=1.5)
      text(y=fivenum(timeRDMtoTNK), labels = fivenum(timeRDMtoTNK), x= 2.5)

      #################################################################
      #AOL SCORE FIGURES - PER SITE
      nr <- nrow(master)
      aol <- (data.frame(matrix(ncol = 4, nrow = nr)))
      colnames(aol) = c("PTID", "SUBJID", "TREAT", "AOL")
      aol$PTID = master$ptid
      aol$TREAT = master$treatment
      for (row in 1:nrow(aol)) {
        for (r in 1:nrow(p4)){
          if (aol$PTID[row] == p4$usubjid[r]){
            aol$SUBJID[row] <- p4$usubjid[r]
            aol$AOL[row] <- p4$fastresc_aolscore[r]
          }
        }
      }
      CTRL0 = 0
      CTRL1 = 0
      CTRL2A = 0
      CTRL2B = 0
      CTRL3 = 0

      TNK0 = 0
      TNK1 = 0
      TNK2A = 0
      TNK2B = 0
      TNK3 = 0
      for (row in 1:nrow(aol)){
        if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 0){
          if (aol$AOL[row] == 0){
            CTRL0 = CTRL0 + 1
          }
          if (aol$AOL[row] == 1){
            CTRL1 = CTRL1 + 1
          }
          if (aol$AOL[row] == 2){
            CTRL2A = CTRL2A + 1
          }
          if (aol$AOL[row] == 3){
            CTRL2B = CTRL2B + 1
          }
          if (aol$AOL[row] == 4){
            CTRL3 = CTRL3 + 1
          }
        }
        if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 1){
          if (aol$AOL[row] == 0){
            TNK0 = TNK0 + 1
          }
          if (aol$AOL[row] == 1){
            TNK1 = TNK1 + 1
          }
          if (aol$AOL[row] == 2){
            TNK2A = TNK2A + 1
          }
          if (aol$AOL[row] == 3){
            TNK2B = TNK2B + 1
          }
          if (aol$AOL[row] == 4){
            TNK3 = TNK3 + 1
          }
        }
      }
      CTRLTOT = CTRL0 + CTRL1 + CTRL2A + CTRL2B + CTRL3
      TNKTOT = TNK0 + TNK1 + TNK2A + TNK2B + TNK3

      ctrlVals = c(CTRL0, CTRL1, CTRL2A, CTRL2B, CTRL3)
      tnkVals = c(TNK0, TNK1, TNK2A, TNK2B, TNK3)

      ctrldf = as.data.frame(ctrlVals)
      rownames(ctrldf) <- c("0", "1", "2A", "2B", "3")
      ctrldf <- t(ctrldf)
      barplot(ctrldf, main = paste("AOL Scores - Control Group (Site", sitename,")", "\n n =", CTRLTOT), xlab = "AOL SCORE", axis.lty = 1,
              ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(ctrldf, na.rm = T)))))

      tnkdf = as.data.frame(tnkVals)
      rownames(tnkdf) <- c("0", "1", "2A", "2B", "3")
      tnkdf <- t(tnkdf)
      barplot(tnkdf, main = paste("AOL Scores - TNK Group (Site", sitename,")", "\n n =", TNKTOT), xlab = "AOL SCORE", axis.lty = 1,
              ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(tnkdf, na.rm = T)))))

      #
      #
      #
      #
      #

      dev.off()
      print(paste("Finished Site ",site," PDF Successfully!", sep = ""))

      #
      #
      #
      #
      #
    }
  }
  if (typeof(site) == "character" && exists("frame")){
    Table1 <- frame
    site <- gsub("[[:punct:]]", "", site)
    site <- trimws(site, which = "both")
    site <- tolower(site)
    site <- gsub(" ","",site)

    #site vectors by country
    UK = c(70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,87,88,89)
    CAN = c(1,2,3,4,5,6,7,8,9,10,11,13,15,16,17,18,20,21,24,25,26,42,47,60,61,91,92,93,94,95,96)
    SPAIN = c(43,44,45,46)
    IRELAND = c(40,41)
    AUSTRIA = c(12,14)
    AUSTRALIA = c(30,31,32,33,34,35,36,37,38)

    #Lists of Possible names
    ukNames = list("uk","unitedkingdom","theuk","theunitedkingdom","bojosplayground")
    canNames = list("can","canada","ca","eh")
    spNames = list("spain","sp","spa","spn","esp","es","espana","espagna","kingdomofspain")
    irelandNames = list("ire","ireland","irelnd","irlnd","ir","eire","irl")
    austriaNames = list("austria","aut","at")
    australiaNames = list("australia","aus","aussi","au","crickey","gdaymate")
    #usNames = list("us","usa","unitedstates","unitedstatesofamerica","theunitedstates","theunitedstatesofamerica","theus","theusa","trumpland")



    #site list/name logic
    siteList = vector()
    nation = ""

    if (any(ukNames == site)){
      siteList = UK
      nation = "United Kingdom"
    }
    if (any(canNames == site)){
      siteList = CAN
      nation = "Canada"
    }
    if (any(spNames == site)){
      siteList = SPAIN
      nation = "Spain"
    }
    if (any(irelandNames == site)){
      siteList = IRELAND
      nation = "Ireland"
    }
    if (any(austriaNames == site)){
      siteList = AUSTRIA
      nation = "Austria"
    }
    if (any(australiaNames == site)){
      siteList = AUSTRALIA
      nation = "Australia"
    }

    #if country name exists:
    if (length(siteList) > 0){
      master<-Table1[FALSE, ]
      for (i in siteList){
        upper = (i*1000)+999
        lower = i*1000
        for (row in 1:nrow(Table1)){
          for (j in lower:upper){
            if (Table1$ptid[row] == j){
              master <- rbind(master,Table1[row, ])
            }
          }
        }
      }
      #Less than 10 enrolled in this country
      if (nrow(master) < 10){
        sitename = nation
        n = paste("(n = ",as.character(length(master$ptid)),")", sep = "")
        print(paste(sitename," has too few enrollees ",n,"."," You need at least 10 enrollees to generate graphs.",sep = ""))
      } else {
        #Builds PDF for the country
        setwd(output)
        sitename = nation
        n = paste("(n = ",as.character(length(master$ptid)),")", sep = "")
        nControl = paste("(n = ",as.character(length(master$ptid)-sum(master$treatment)),")", sep = "")
        nTNK = paste("(n = ",as.character(sum(master$treatment)),")", sep = "")
        pdftitle = paste(sitename, sep = "")
        pdfdate = Sys.Date()
        pdf(paste(pdftitle, pdfdate, ".pdf"), paper = "letter", pagecentre = T)

        #
        #
        #
        #
        #

        #########################################################
        #subjects recruited by month - boxplot
        DateTable = master
        DateTable$rdm_date <- as.Date(DateTable$rdm_date, format="%Y/%m/%d")
        tab <- table(cut(DateTable$rdm_date, 'month'))
        DatesByMonth <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'),
                                   Frequency=as.vector(tab))
        boxplot(DatesByMonth$Frequency,main = paste("Subjects Recruited by Month - ",sitename,"\n",n),
                ylab = "Number of Subjects", axis.lty = 1, horizontal = F)
        text(y=fivenum(DatesByMonth$Frequency), labels = fivenum(DatesByMonth$Frequency), x=1.25)

        #########################################################
        #Imaging Modality

        #piechart
        ImgTable = master
        imgmod <- table(ImgTable$image_rdm)
        for (row in 1:nrow(imgmod)) {
          if (rownames(imgmod)[row] == "1"){
            rownames(imgmod)[row] <- "spCTA"
          }
          if (rownames(imgmod)[row] == "2"){
            rownames(imgmod)[row] <- "mCTA"
          }
          if (rownames(imgmod)[row] == "3"){
            rownames(imgmod)[row] <- "CTP"
          }
          if (rownames(imgmod)[row] == "4"){
            rownames(imgmod)[row] <- "MRA"
          }
        }
        piepercent<- (round(imgmod/sum(imgmod), 5)*100)
        piepercent<-signif(piepercent, digits = 3)
        piepercent <- as.character(piepercent)
        percent = "%"
        for (character in 1:length(piepercent)){
          piepercent[character] <- paste(piepercent[character], percent)
        }
        cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
        pie(imgmod, main = paste("Imaging Modality Used to Randomize - ",sitename,"\n",n), labels = piepercent, clockwise = T,col = cols)
        legend("topright", c("spCTA","mCTA","CTP","MRA"), fill = cols)

        #stacked barplot
        ImgTable = master
        imgmod <- table(ImgTable$image_rdm)
        for (row in 1:nrow(imgmod)) {
          if (rownames(imgmod)[row] == "1"){
            rownames(imgmod)[row] <- "spCTA"
          }
          if (rownames(imgmod)[row] == "2"){
            rownames(imgmod)[row] <- "mCTA"
          }
          if (rownames(imgmod)[row] == "3"){
            rownames(imgmod)[row] <- "CTP"
          }
          if (rownames(imgmod)[row] == "4"){
            rownames(imgmod)[row] <- "MRA"
          }
        }
        imgmod<-t(imgmod)
        imgmod<-t(imgmod)
        cols = c("#E0F3DB", "#A8DDB5", "#4EB3D3", "#0868AC")
        xs<-barplot(imgmod, legend =F,col = cols,
                    ylim = range(pretty(c(0,length(master$ptid)))),
                    main = paste("Imaging Modality Used to Randomize - ",sitename,"\n",n))
        yvals<-apply(imgmod,2,cumsum)
        yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:nrow(imgmod),]/2
        text(xs,yv2, rownames(yvals), cex = 1.0)

        #########################################################
        #TNK vs Control
        TNK = sum(master$treatment)/length(master$ptid)
        CONTROL = 1-TNK
        x<-c(TNK,CONTROL)
        piepercent<- round(100*x/sum(x), 5)
        piepercent<-signif(piepercent, digits = 3)
        piepercent <- as.character(piepercent)
        percent = "%"
        for (character in 1:length(piepercent)){
          piepercent[character] <- paste(piepercent[character], percent)
        }
        cols = c("#E0F3DB", "#4EB3D3")
        pie(x, labels = piepercent, main = paste("TNK vs Control - ",sitename,"\n",n), clockwise = T, col = cols)
        legend("topright", c("TNK","Control"), fill = cols)

        #########################################################
        #Time of Onset to TNK/Control
        onsettoTNK<-vector()
        onsettoCTRL <- vector()
        for (row in 1:nrow(master)){
          if (master$treatment[row] == 1 && master$ta_tnk_date_1stdose[row] != "" && !is.na(master$ta_tnk_date_1stdose[row]) && !is.null(master$ta_tnk_date_1stdose[row]) && master$rdm_onset_date[row] != "" && !is.na(master$rdm_onset_date[row]) && !is.null(master$rdm_onset_date[row])){
            if (master$rdm_onset_date[row] == master$ta_tnk_date_1stdose[row]){
              difference4 = (master$ta_tnk_time_1stdose[row] - master$rdm_onset_time[row])/100
              onsettoTNK <- c(onsettoTNK, difference4)
            } else {
              difference4 = ((2400-master$rdm_onset_time[row])+as.integer(as.character(master$ta_tnk_time_1stdose[row]))/100)
              onsettoTNK <- c(onsettoTNK, difference4)
            }
          }
          if (master$treatment[row] == 0 && master$ta_c_date_1stdose[row] != "" && !is.na(master$ta_c_date_1stdose[row]) && !is.null(master$ta_c_date_1stdose[row]) && master$rdm_onset_date[row] != "" && !is.na(master$rdm_onset_date[row]) && !is.null(master$rdm_onset_date[row])){
            if (master$rdm_onset_date[row] == master$ta_c_date_1stdose[row]){
              difference5 = (as.integer(as.character(master$ta_c_time_1stdose[row])) - master$rdm_onset_time[row])/100
              onsettoCTRL <- c(onsettoCTRL, difference5)
            } else {
              difference5 = ((2400-master$rdm_onset_time[row])+as.integer(as.character(master$ta_c_time_1stdose[row]))/100)
              onsettoCTRL <- c(onsettoCTRL, difference5)
            }
          }
        }

        for (i in 1:length(onsettoCTRL)){
          if (!is.na(onsettoCTRL[i]) && onsettoCTRL[i] < 0){
            onsettoCTRL[i] <- NA
          }
        }

        aa = data.frame(group = paste("\n","Time from Onset to Control","\n",nControl), value = onsettoCTRL)
        bb = data.frame(group = paste("\n","Time from Onset to TNK","\n",nTNK), value = onsettoTNK)
        aabb = rbind(aa,bb)
        boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Onset to TNK/Control - ",sitename),
                ylab = "Time (Hours)", ylim = c(0,20), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
        text(y=fivenum(onsettoCTRL), labels = fivenum(onsettoCTRL), x=1.5)
        text(y=fivenum(onsettoTNK), labels = fivenum(onsettoTNK), x= 2.5)

        #########################################################
        #Time of Onset to Randomization
        timeonsettocontrol<-vector()
        for (row in 1:nrow(master)){
          if (master$rdm_date[row] == master$rdm_onset_date[row]){
            difference = (master$rdm_time[row]-master$rdm_onset_time[row])/100
            timeonsettocontrol <- c(timeonsettocontrol,difference)
          } else {
            difference = ((2400-master$rdm_onset_time[row])+master$rdm_time[row])/100
            timeonsettocontrol <- c(timeonsettocontrol,difference)
          }
        }
        master$timeonsettocontrol<-timeonsettocontrol

        boxplot(timeonsettocontrol, na.rm=T, main = paste("Time from Onset to Randomization - ",sitename,"\n",n),
                ylab = "Time (Hours)", horizontal = F, staplewex = 1)
        text(y=fivenum(timeonsettocontrol), labels = fivenum(timeonsettocontrol), x=1.25)

        #########################################################
        #Time from CT to TNK/CTRL
        timeBLCTtoTNK<-vector()
        for (row in 1:nrow(master)){
          if (master$treatment[row] == 1 && !is.null(master$bl_ct_date.x[row]) && !is.na(master$bl_ct_date.x[row]) && master$bl_ct_date.x[row] != "" && !is.null(master$ta_tnk_date_1stdose[row]) && !is.na(master$ta_tnk_date_1stdose[row]) && master$ta_tnk_date_1stdose[row] != ""){
            if (master$bl_ct_date.x[row] == master$ta_tnk_date_1stdose[row]){
              difference2 = (as.integer(master$ta_tnk_time_1stdose[row])-as.integer(master$bl_ct_time.x[row]))/100
              timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
            } else {
              difference2 = ((2400-as.integer(master$bl_ct_time.x[row]))+as.integer(master$ta_tnk_time_1stdose[row]))/100
              timeBLCTtoTNK <- c(timeBLCTtoTNK, difference2)
            }
          }
        }

        timeBLCTtoCTRL<-vector()
        for (row in 1:nrow(master)){
          if (master$treatment[row] == 0 && !is.null(master$bl_ct_date.x[row]) && !is.na(master$bl_ct_date.x[row]) && (master$bl_ct_date.x[row] != "") && !is.null(master$ta_c_date_1stdose[row]) && !is.na(master$ta_c_date_1stdose[row]) && (master$ta_c_date_1stdose[row] != "") && (master$ta_c_time_1stdose[row] != "") && !is.null(master$ta_c_time_1stdose) && !is.na(master$ta_c_time_1stdose[row])){
            if (master$bl_ct_date.x[row] == master$ta_c_date_1stdose[row]){
              difference3 = (as.integer(as.character(master$ta_c_time_1stdose[row]))-as.integer(master$bl_ct_time.x[row]))/100
              timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
            } else if (master$bl_ct_date.x[row] != master$ta_c_date_1stdose[row]) {
              difference3 = ((2400-as.integer(master$bl_ct_time.x[row]))+as.integer(as.character(master$ta_c_time_1stdose[row])))/100
              timeBLCTtoCTRL <- c(timeBLCTtoCTRL, difference3)
            }
          }
        }

        for (i in 1:length(timeBLCTtoCTRL)){
          if (!is.na(timeBLCTtoCTRL[i]) && timeBLCTtoCTRL[i] < 0){
            timeBLCTtoCTRL[i] <- NA
          }
        }

        aa = data.frame(group = paste("\n","Time from Baseline CT to Control","\n",nControl), value = timeBLCTtoCTRL)
        bb = data.frame(group = paste("\n","Time from Baseline CT to TNK","\n",nTNK), value = timeBLCTtoTNK)
        aabb = rbind(aa,bb)
        boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Baseline CT to TNK/Control - ",sitename),
                ylab = "Time (Hours)", ylim = c(0,4), staplewex = 1, las = 1, xlab = NULL, par(mgp = c(3,1.5,0)))
        text(y=fivenum(timeBLCTtoCTRL), labels = fivenum(timeBLCTtoCTRL), x=1.5)
        text(y=fivenum(timeBLCTtoTNK), labels = fivenum(timeBLCTtoTNK), x= 2.5)

        #########################################################
        #NIHSS
        x<-as.vector(master$rdm_nihss)
        remove<-c(99)
        x<-x [! x %in% remove]#gets rid of incorrect values
        boxplot(x, na.rm=T, main = paste("NIHSS Total Score (Baseline) - ",sitename,"\n",n), ylab = "Score", horizontal = F, staplewex = 1, las = 1)
        text(y=fivenum(x), labels = fivenum(x), x=1.25)

        #########################################################
        #AGE
        age_vec <- as.data.frame(master$rdm_dob)
        age_vec<-as.Date(paste(as.character(age_vec$`master$rdm_dob`), '01'), format = '%Y%m%d')
        z<-c((Sys.Date()-age_vec)/365)
        z<-signif(z, digits = 3)
        z<-as.numeric(z)
        boxplot(z, na.rm = T, ylab = "Age (Years)", horizontal = F, staplewex = 1, main = paste("Age of TEMPO-2 Enrollees - ",sitename,"\n",n))
        text(y=fivenum(z), labels = fivenum(z), x = 1.25)

        #########################################################
        #SEX
        SEX_M = sum(master$rdm_sex)/length(master$ptid)
        SEX_F = 1-SEX_M
        y <- c(SEX_F,SEX_M)
        piepercent<- round(100*y/sum(y), 5)
        piepercent<-signif(piepercent, digits = 3)
        piepercent <- as.character(piepercent)
        percent = "%"
        for (character in 1:length(piepercent)){
          piepercent[character] <- paste(piepercent[character], percent)
        }
        cols = c("#E0F3DB", "#4EB3D3")
        pie(y, labels = piepercent, main = paste("Sex of TEMPO-2 Enrollees - ",sitename,"\n",n),
            clockwise = T, col = cols)
        legend("topright", c("Female","Male"), fill = cols)

        #########################################################
        #occlusion location - base data
        ICA = 0
        M1 = 0
        M2 = 0
        M3 = 0
        VA = 0
        P1 = 0
        P2 = 0
        A1 = 0
        A2 = 0
        PICA = 0
        BA = 0
        for (row in 1:nrow(master)){
          if (!is.na(master$occl_location[row])){
            if (master$occl_location[row] == 1 || master$occl_location[row] == 2){
              ICA <- ICA + 1
            }
            if (master$occl_location[row] == 3 || master$occl_location[row] == 4){
              M1 <- M1 + 1
            }
            if (master$occl_location[row] == 5 || master$occl_location[row] == 6){
              M2 <- M2 + 1
            }
            if (master$occl_location[row] == 7 || master$occl_location[row] == 8){
              M3 <- M3 + 1
            }
            if (master$occl_location[row] == 9 || master$occl_location[row] == 10){
              VA <- VA + 1
            }
            if (master$occl_location[row] == 11 || master$occl_location[row] == 12){
              P1 <- P1 + 1
            }
            if (master$occl_location[row] == 13 || master$occl_location[row] == 14){
              P2 <- P2 + 1
            }
            if (master$occl_location[row] == 15 || master$occl_location[row] == 16){
              A1 <- A1 + 1
            }
            if (master$occl_location[row] == 17 || master$occl_location[row] == 18){
              A2 <- A2 + 1
            }
            if (master$occl_location[row] == 19 || master$occl_location[row] == 20){
              PICA <- PICA + 1
            }
            if (master$occl_location[row] == 99){
              BA <- BA + 1
            }
          }
        }
        #stacked barplot
        occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
        taball<-as.data.frame(occlocation)
        rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
        newRowNames = vector()
        removeList = vector()
        for(row in rownames(taball)){
          if (taball[row,] != 0){
            newRowNames<-c(newRowNames, row)
          }
        }
        for(row in 1:nrow(taball)){
          if (taball[row,] == 0){
            removeList<-c(removeList, as.integer(row))
          }
        }
        taball<-taball[-c(removeList),]
        taball <- as.data.frame(taball)
        rownames(taball)<-newRowNames
        taball<-t(taball)
        taball<-t(taball)
        cols = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081")
        xs<-barplot(taball, legend =F,col = cols,
                    ylim = range(pretty(c(0,length(master$ptid)))),
                    main = paste("Occlusion Location - ",sitename,"\n",n), ylab = "Number of Subjects")
        yvals<-apply(taball,2,cumsum)
        yv2<-(rbind(yvals,0)+rbind(0,yvals))[1:nrow(taball),]/2
        text(xs,yv2, rownames(yvals), cex = .6)

        #standard barplot
        occlocation<-c(ICA,M1,M2,M3,VA,P1,P2,A1,A2,PICA,BA)
        taball<-as.data.frame(occlocation)
        rownames(taball) <- c("ICA", "M1","M2","M3","VA","P1","P2","A1","A2","PICA","BA")
        taball<-t(taball)
        barplot(taball, main = paste("Occlusion Location - ",sitename,"\n",n), xlab = "Location", axis.lty = 1,
                ylab = "Number of Cases", las = 1, cex.names = 0.8, ylim = range(pretty(c(0,max(occlocation, na.rm = T)))))

        #########################################################
        #Randomization time to treatment
        timeRDMtoTNK<-vector()
        for (row in 1:nrow(master)){
          if (master$treatment[row] == 1 && !is.null(master$rdm_date[row]) && !is.na(master$rdm_date[row]) && master$rdm_date[row] != "" && !is.null(master$ta_tnk_date_1stdose[row]) && !is.na(master$ta_tnk_date_1stdose[row]) && master$ta_tnk_date_1stdose[row] != ""){
            if (master$rdm_date[row] == master$ta_tnk_date_1stdose[row]){
              difference2 = (as.integer(master$ta_tnk_time_1stdose[row])-as.integer(master$rdm_time[row]))/100
              timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
            } else {
              difference2 = ((2400-as.integer(master$rdm_time[row]))+as.integer(master$ta_tnk_time_1stdose[row]))/100
              timeRDMtoTNK <- c(timeRDMtoTNK, difference2)
            }
          }
        }
        for (i in 1:length(timeRDMtoTNK)){
          if (!is.na(timeRDMtoTNK[i]) && timeRDMtoTNK[i] < 0){
            timeRDMtoTNK[i] <- NA
          }
        }

        timeRDMtoCTRL<-vector()
        for (row in 1:nrow(master)){
          if (master$treatment[row] == 0 && !is.null(master$rdm_date[row]) && !is.na(master$rdm_date[row]) && (master$rdm_date[row] != "") && !is.null(master$ta_c_date_1stdose[row]) && !is.na(master$ta_c_date_1stdose[row]) && (master$ta_c_date_1stdose[row] != "") && (master$ta_c_time_1stdose[row] != "") && !is.null(master$ta_c_time_1stdose) && !is.na(master$ta_c_time_1stdose[row])){
            if (master$rdm_date[row] == master$ta_c_date_1stdose[row]){
              difference3 = (as.integer(as.character(master$ta_c_time_1stdose[row]))-as.integer(master$rdm_time[row]))/100
              timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
            } else if (master$rdm_date[row] != master$ta_c_date_1stdose[row]) {
              difference3 = ((2400-as.integer(master$rdm_time[row]))+as.integer(as.character(master$ta_c_time_1stdose[row])))/100
              timeRDMtoCTRL <- c(timeRDMtoCTRL, difference3)
            }
          }
        }
        for (i in 1:length(timeRDMtoCTRL)){
          if (!is.na(timeRDMtoCTRL[i]) && timeRDMtoCTRL[i] < 0){
            timeRDMtoCTRL[i] <- NA
          }
        }

        timeRDMtoTNK<-timeRDMtoTNK*60
        timeRDMtoCTRL<-timeRDMtoCTRL*60

        aa = data.frame(group = paste("\n","Randomization to Control","\n",nControl), value = timeRDMtoCTRL)
        bb = data.frame(group = paste("\n","Randomization to TNK","\n",nTNK), value = timeRDMtoTNK)
        aabb = rbind(aa,bb)
        boxplot(aabb$value~aabb$group, na.rm = T, main = paste("Time from Randomization to TNK/Control - ",sitename),
                ylab = "Time (Minutes)", ylim = range(pretty(c(0,max(timeRDMtoTNK, na.rm = T)))), staplewex = 1, las = 1,
                xlab = NULL, par(mgp = c(3,1.5,0)))
        text(y=fivenum(timeRDMtoCTRL), labels = fivenum(timeRDMtoCTRL), x=1.5)
        text(y=fivenum(timeRDMtoTNK), labels = fivenum(timeRDMtoTNK), x= 2.5)

        #################################################################
        #AOL SCORE FIGURES - PER SITE
        nr <- nrow(master)
        aol <- (data.frame(matrix(ncol = 4, nrow = nr)))
        colnames(aol) = c("PTID", "SUBJID", "TREAT", "AOL")
        aol$PTID = master$ptid
        aol$TREAT = master$treatment
        for (row in 1:nrow(aol)) {
          for (r in 1:nrow(p4)){
            if (aol$PTID[row] == p4$usubjid[r]){
              aol$SUBJID[row] <- p4$usubjid[r]
              aol$AOL[row] <- p4$fastresc_aolscore[r]
            }
          }
        }
        CTRL0 = 0
        CTRL1 = 0
        CTRL2A = 0
        CTRL2B = 0
        CTRL3 = 0

        TNK0 = 0
        TNK1 = 0
        TNK2A = 0
        TNK2B = 0
        TNK3 = 0
        for (row in 1:nrow(aol)){
          if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 0){
            if (aol$AOL[row] == 0){
              CTRL0 = CTRL0 + 1
            }
            if (aol$AOL[row] == 1){
              CTRL1 = CTRL1 + 1
            }
            if (aol$AOL[row] == 2){
              CTRL2A = CTRL2A + 1
            }
            if (aol$AOL[row] == 3){
              CTRL2B = CTRL2B + 1
            }
            if (aol$AOL[row] == 4){
              CTRL3 = CTRL3 + 1
            }
          }
          if (!is.na(aol$SUBJID[row]) && !is.na(aol$AOL[row]) && aol$TREAT[row] == 1){
            if (aol$AOL[row] == 0){
              TNK0 = TNK0 + 1
            }
            if (aol$AOL[row] == 1){
              TNK1 = TNK1 + 1
            }
            if (aol$AOL[row] == 2){
              TNK2A = TNK2A + 1
            }
            if (aol$AOL[row] == 3){
              TNK2B = TNK2B + 1
            }
            if (aol$AOL[row] == 4){
              TNK3 = TNK3 + 1
            }
          }
        }
        CTRLTOT = CTRL0 + CTRL1 + CTRL2A + CTRL2B + CTRL3
        TNKTOT = TNK0 + TNK1 + TNK2A + TNK2B + TNK3

        ctrlVals = c(CTRL0, CTRL1, CTRL2A, CTRL2B, CTRL3)
        tnkVals = c(TNK0, TNK1, TNK2A, TNK2B, TNK3)

        ctrldf = as.data.frame(ctrlVals)
        rownames(ctrldf) <- c("0", "1", "2A", "2B", "3")
        ctrldf <- t(ctrldf)
        barplot(ctrldf, main = paste("AOL Scores - Control Group (Site", sitename,")", "\n n =", CTRLTOT), xlab = "AOL SCORE", axis.lty = 1,
                ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(ctrldf, na.rm = T)))))

        tnkdf = as.data.frame(tnkVals)
        rownames(tnkdf) <- c("0", "1", "2A", "2B", "3")
        tnkdf <- t(tnkdf)
        barplot(tnkdf, main = paste("AOL Scores - TNK Group (Site", sitename,")", "\n n =", TNKTOT), xlab = "AOL SCORE", axis.lty = 1,
                ylab = "NUMBER OF CASES", las = 1, cex.names = 0.8, ylim = range(pretty(c(0, max(tnkdf, na.rm = T)))))

        #
        #
        #
        #
        #

        dev.off()
        print(paste("Finished ",sitename," PDF Successfully!", sep = ""))

        #
        #
        #
        #
        #
      }
    }
    #ALL Special Case
    if (site == "all"){
      ALL <- c(0:18,20,21,24:26,30:38,40:47,60,61,70:85,87:89,91:96,99)
      for (i in ALL){
        Tempo2(frame,site = i)
      }
    }
    #invalid entry
    if (length(siteList) == 0 && site != "all"){
      print("You have entered an invalid site name. Type '?Tempo2' in the console or check the documentation for usage instructions.")
    }
  }
  #Invalid Type
  if (typeof(site) != "integer" && typeof(site) != "double" && typeof(site) != "character" && exists("frame")){
    print("You have entered an invalid site name. Type '?Tempo2' in the console or check the documentation for usage instructions.")
  }
}

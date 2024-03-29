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
  # Plate 1
  fileSelector = rstudioapi::showQuestion(title = "Enter Randomization CSV", message = "Please select your randomization CSV file (Plate 1).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate1 <<- rstudioapi::selectFile("Select your Randomization.CSV file")
    outPath <<- dirname(plate1)
    plate1 <<- read.csv(plate1, header = T)
    plate1 <<- as.data.frame(plate1)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a randomization file (plate 1 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 2
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 2 CSV", message = "Please select your Plate 2 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate2 <<- rstudioapi::selectFile("Select your Plate Two.CSV file")
    plate2 <<- read.csv(plate2, header = T)
    plate2 <<- as.data.frame(plate2)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 2 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 3
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 3 CSV", message = "Please select your Plate 3 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate3 <<- rstudioapi::selectFile("Select your Plate Three.CSV file")
    plate3 <<- read.csv(plate3, header = T)
    plate3 <<- as.data.frame(plate3)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 3 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 4
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 4 CSV", message = "Please select your Plate 4 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate4 <<- rstudioapi::selectFile("Select your Plate Four.CSV file")
    plate4 <<- read.csv(plate4, header = T)
    plate4 <<- as.data.frame(plate4)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 4 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 5
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 5 CSV", message = "Please select your Plate 5 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate5 <<- rstudioapi::selectFile("Select your Plate Five.CSV file")
    plate5 <<- read.csv(plate5, header = T)
    plate5 <<- as.data.frame(plate5)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 5 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 6
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 6 CSV", message = "Please select your Plate 6 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate6 <<- rstudioapi::selectFile("Select your Plate Six.CSV file")
    plate6 <<- read.csv(plate6, header = T)
    plate6 <<- as.data.frame(plate6)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 6 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 7
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 7 CSV", message = "Please select your Plate 7 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate7 <<- rstudioapi::selectFile("Select your Plate Seven.CSV file")
    plate7 <<- read.csv(plate7, header = T)
    plate7 <<- as.data.frame(plate7)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 7 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 8
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 8 CSV", message = "Please select your Plate 8 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate8 <<- rstudioapi::selectFile("Select your Plate Eight.CSV file")
    plate8 <<- read.csv(plate8, header = T)
    plate8 <<- as.data.frame(plate8)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 8 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 9
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 9 CSV", message = "Please select your Plate 9 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate9 <<- rstudioapi::selectFile("Select your Plate Nine.CSV file")
    plate9 <<- read.csv(plate9, header = T)
    plate9 <<- as.data.frame(plate9)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 9 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 10
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 10 CSV", message = "Please select your Plate 10 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate10 <<- rstudioapi::selectFile("Select your Plate Ten.CSV file")
    plate10 <<- read.csv(plate10, header = T)
    plate10 <<- as.data.frame(plate10)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 10 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 11
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 11 CSV", message = "Please select your Plate 11 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate11 <<- rstudioapi::selectFile("Select your Plate Eleven.CSV file")
    plate11 <<- read.csv(plate11, header = T)
    plate11 <<- as.data.frame(plate11)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 11 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 12
  fileSelector = rstudioapi::showQuestion(title = "Enter Baseline CT/CTA Site Assessment CSV", message = "Please select your Baseline CT/CTA Site Assessment CSV File (Plate 12).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate12 <<- rstudioapi::selectFile("Select your Baseline CT/CTA .CSV file")
    plate12 <<- read.csv(plate12, header = T)
    plate12 <<- as.data.frame(plate12)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Baseline CT/CTA Site Assesment form (plate 12 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 13
  fileSelector = rstudioapi::showQuestion(title = "Enter Treatment Assignment CSV", message = "Please select a Treatment Assignment CSV File (Plate 13).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate13 <<- rstudioapi::selectFile("Select your Treatment Assignment .CSV file")
    plate13 <<- read.csv(plate13, header = T)
    plate13 <<- as.data.frame(plate13)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Treatment Assignment file (plate 13 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 14
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 14 CSV", message = "Please select your Plate 14 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate14 <<- rstudioapi::selectFile("Select your Plate Fourteen.CSV file")
    plate14 <<- read.csv(plate14, header = T)
    plate14 <<- as.data.frame(plate14)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 14 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 15
  fileSelector = rstudioapi::showQuestion(title = "Enter Plate 15 CSV", message = "Please select your Plate 15 CSV file",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate15 <<- rstudioapi::selectFile("Select your Plate Fifteen.CSV file")
    plate15 <<- read.csv(plate15, header = T)
    plate15 <<- as.data.frame(plate15)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose a Plate 15 CSV file. Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  # Plate 16
  fileSelector = rstudioapi::showQuestion(title = "Enter Imaging Form CSV", message = "Please select your Imaging Form CSV file (Plate 16).",
                                          ok = NULL, cancel = NULL)
  if (fileSelector == TRUE){
    plate16 <<- rstudioapi::selectFile("Select your Plate 16.CSV file")
    plate16 <<- read.csv(plate16, header = T)
    plate16 <<- as.data.frame(plate16)
  } else {
    rstudioapi::showDialog(title = "Warning!", message = "You must choose an imaging file (plate 16 in DFExplore). Please run the assemble() function again.")
    break;
  }
  ######################################################################################
  #Return to Caller
  rstudioapi::showDialog(title = "Success", message = "Finished Successfully.")
}

generate <- function(){
  #code starts here
  #
  #
  #Generating the master table with initial NA values
  numRows = nrow(plate1)
  monitoringReport <<- data.frame(matrix(ncol = 22, nrow = numRows))
  colnames(monitoringReport) <<- c('PTID',"rand <12 hr", "D&T Rand Match","D&T Onset < Rand","D&T Onset match", "Sex Match","NIHSS match",
                                  "Dose ml", "Dose mg", "D&T Stroke Onset","D&T Informed Consent","Smoke?","D&T > Onset, <6h rdm", "Pregnancy Test",
                                  "BP SYS < 185", "BP DIA < 110","BL NIHSS Match", "Followup 24H", "Followup 5D", "Followup 90D", "mRS BL <= 2",
                                  "mRS 90D < BL")
  ptids = plate1$ptid
  monitoringReport$PTID <<- ptids
  #
  #
  #
  #
  #Make day 0 NIHSS
  day0NIHSS <<- plate10[FALSE, ]
  for (ptid in ptids){
    for (row in 1:nrow(plate10)){
      if (plate10$ptid[row] == ptid){
        day0NIHSS <<- rbind(day0NIHSS, plate10[row, ])
        break
      }
    }
  }
  #begin looping of rows
  for (row in 1:nrow(monitoringReport)){
    n = monitoringReport$PTID[row]
    monitoringReport$`rand <12 hr`[row] = rdm12h(n)
    monitoringReport$`D&T Onset < Rand`[row] = onsetPriorRdm(n)
    monitoringReport$`D&T Onset match`[row] = onsetMatch(n)
    monitoringReport$`Sex Match`[row] = sexMatch(n)
    monitoringReport$`NIHSS match`[row] = nihssMatch(n)
    monitoringReport$`Dose ml`[row] = doseml(n)
    monitoringReport$`Dose mg`[row] = dosemg(n)
    monitoringReport$`D&T Stroke Onset`[row] = dtStrokeOnset(n)
    monitoringReport$`Smoke?`[row]= smoking(n)
    monitoringReport$`D&T > Onset, <6h rdm`[row]=dtOnsetRand(n)
    monitoringReport$`Pregnancy Test`[row] = pregTest(n)
    monitoringReport$`BP SYS < 185`[row]= bpSYS(n)
    monitoringReport$`BP DIA < 110`[row]= bpDIA(n)
    monitoringReport$`D&T Rand Match`[row] = dtRandMatch(n)
    monitoringReport$`D&T Informed Consent`[row] = dtInformedConsent(n)
    monitoringReport$`BL NIHSS Match`[row] = blNIHSSMatch(n)
    monitoringReport$`Followup 24H`[row] = followUp24H(n)
    monitoringReport$`Followup 5D`[row] = followUp5D(n)
    monitoringReport$`Followup 90D`[row] = followUp90D(n)
    monitoringReport$`mRS BL <= 2`[row] = mrsBLCheck(n)
    monitoringReport$`mRS 90D < BL`[row] = mrs90DCheck(n)
  }

  #output Excel file
  if(require("openxlsx")){
    print("openxlsx is loaded correctly")
  } else {
    print("Trying to Install openxlsx")
    install.packages("openxlsx")
    if(require("openxlsx")){
      print("openxlsx installed and loaded")
    } else {
      stop("Could not install openxlsx")
    }
  }


  wb <- createWorkbook()
  addWorksheet(wb, "Sheet 1")

  cs1 <- createStyle(bgFill = "green")
  cs2 <- createStyle(bgFill = "red")

  writeData(wb, "Sheet 1", monitoringReport, colNames = TRUE)
  conditionalFormatting(wb, "Sheet 1", cols = 1:22+1, rows = 1:nrow(monitoringReport)+1, rule = "Y", style = cs1, type = "contains")
  conditionalFormatting(wb, "Sheet 1", cols = 1:22+1, rows = 1:nrow(monitoringReport)+1, rule = "N", style = cs2, type = "contains")

  excelFile = "/Monitoring Report"
  excelDate = Sys.Date()
  interimfilename = paste(excelFile, excelDate)
  filename = paste(interimfilename, ".xlsx", sep = "")
  extension = paste(outPath, filename, sep = "")

  saveWorkbook(wb, file = extension, overwrite = TRUE)

}

rdm12h <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      rdmDate = plate1$rdm_date[row]
      rdmTime = plate1$rdm_time[row]
      onsetDate = plate1$rdm_onset_date[row]
      onsetTime = plate1$rdm_onset_time[row]
      timeDifference = 0
      if (rdmDate == onsetDate){
        timeDifference = rdmTime - onsetTime
      } else {
        timeDifference = ((2400 - onsetTime) + rdmTime)
      }
      if (timeDifference < 1200){
        return('Y')
      } else {
        return('N')
      }
    }
    if(!any(plate1$ptid == n)){
      return(NA)
    }
  }
}

onsetPriorRdm <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      p1RdmDate = plate1$rdm_date[row]
      p1RdmTime = plate1$rdm_time[row]
      p1OnsetDate = plate1$rdm_onset_date[row]
      p1OnsetTime = plate1$rdm_onset_time[row]
    }
  }
  if (as.POSIXct(p1RdmDate) > as.POSIXct(p1OnsetDate)){
    return('Y')
  }
  if (p1RdmDate == p1OnsetDate){
    if (p1RdmTime > p1OnsetTime){
      return('Y')
    } else {
      return('N')
    }
  }
  if (as.POSIXct(p1RdmDate) < as.POSIXct(p1OnsetDate)){
    return('N')
  }
}

onsetMatch <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      p1OnsetDate = plate1$rdm_onset_date[row]
      p1OnsetTime = plate1$rdm_onset_time[row]
    }
  }
  for (row in 1:nrow(plate4)){
    if (plate4$ptid[row] == n){
      p4OnsetDate = plate4$date_onset[row]
      p4OnsetTime = plate4$time_onset[row]
    }
  }
  if(!any(plate4$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (!is.null(p4OnsetDate) && !is.null(p1OnsetDate) && p1OnsetDate == p4OnsetDate){
    if (p1OnsetTime == p4OnsetTime){
      return('Y')
    } else {
      return('N')
    }
  }
  if (!is.null(p4OnsetDate) && !is.null(p1OnsetDate) && p1OnsetDate != p4OnsetDate){
    return('N')
  }
  if (is.null(p4OnsetDate) || is.null(p1OnsetDate)){
    return(NA)
  }

}

sexMatch <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      sex1 = plate1$rdm_sex[row]
    }
  }
  for (row in 1:nrow(plate4)){
    if (plate4$ptid[row] == n){
      sex2 = plate4$sex[row]
    }
  }
  if(!any(plate4$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (sex1 == sex2){
    return('Y')
  } else {
    return('N')
  }
}

nihssMatch <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      p1nihss = plate1$rdm_nihss[row]
    }
  }
  for (row in 1:nrow(day0NIHSS)){
    if (day0NIHSS$ptid[row] == n){
      day0nihssScore = day0NIHSS$NIHSS_Totscore[row]
    }
  }
  if(!any(day0NIHSS$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (p1nihss == day0nihssScore){
    return('Y')
  } else {
    return('N')
  }
}

doseml <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      dml1 = plate1$ta_tnk_dose_ml_rndm[row]
    }
  }
  for (row in 1:nrow(plate13)){
    if (plate13$ptid[row] == n){
      dml2 = plate13$ta_tnk_dose_ml[row]
    }
  }
  if(!any(plate13$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (!is.na(dml1) && !is.na(dml2)){
    if (dml1 == dml2){
      return('Y')
    }
    if (dml1 != dml2){
      return('N')
    }
  } else {
    return(NA)
  }
}

dosemg <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      dmg1 = plate1$ta_tnk_dose_mg_rndm[row]
    }
  }
  for (row in 1:nrow(plate13)){
    if (plate13$ptid[row] == n){
      dmg2 = plate13$ta_tnk_dose_mg[row]
    }
  }
  if(!any(plate13$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (!is.na(dmg1) && !is.na(dmg2)){
    if (dmg1 == dmg2){
      return('Y')
    }
    if (dmg1 != dmg2){
      return('N')
    }
  } else {
    return(NA)
  }
}

dtStrokeOnset <- function(n){
  for (row in 1:nrow(plate4)){
    if (plate4$ptid[row] == n){
      dateOnset = plate4$date_onset[row]
      timeOnset = plate4$time_onset[row]
      dateConsent = plate4$date_consent[row]
      timeConsent = plate4$time_consent[row]
    }
  }
  if(!any(plate4$ptid == n)){
    return(NA)
  }
  if (as.POSIXct(dateConsent) > as.POSIXct(dateOnset)){
    return('Y')
  }
  if(as.POSIXct(dateConsent) == as.POSIXct(dateOnset)){
    if (timeConsent > timeOnset){
      return('Y')
    }
    if (timeConsent < timeOnset) {
      return('N')
    }
  }
  if(as.POSIXct(dateConsent) < as.POSIXct(dateOnset)){
    return('N')
  }
}

smoking <- function(n){
  for (row in 1:nrow(plate5)){
    if (plate5$ptid[row] == n){
      smoker = plate5$smoking_hx[row]
      status = plate5$smoking_status[row]
    }
  }
  if(!any(plate5$ptid == n)){
    return(NA)
  }
  if (smoker == 1){
    if (status == 1 || status == 2){
      return('Y')
    } else {
      return('N')
    }
  }
  if (smoker == 0){
    if (status == 99){
      return('Y')
    } else {
      return('N')
    }
  }
}

dtOnsetRand <- function(n){
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      dateOnset = plate1$rdm_onset_date[row]
      timeOnset = plate1$rdm_onset_time[row]
      dateRDM = plate1$rdm_date[row]
      timeRDM = plate1$rdm_time[row]
    }
  }
  for (row in 1:nrow(plate7)){
    if (plate7$ptid[row] == n){
      date7 = plate7$date_bld[row]
      time7 = plate7$time_bld[row]
    }
  }
  if(!exists("time7") || !exists("timeRDM")){
    return(NA)
  }
  if(time7 == "*"){
    return(NA)
  }
  if(!any(plate7$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (as.POSIXct(date7) > as.POSIXct(dateOnset)){
    #do things
    if (as.POSIXct(date7) == as.POSIXct(dateRDM)){
      if ((as.integer(as.character(time7)) - as.integer(as.character(timeRDM))) <= 600){
        return('Y')
      }
      if ((as.integer(as.character(time7)) - as.integer(as.character(timeRDM))) > 600){
        return('N')
      }
    }
    if (as.POSIXct(date7) > as.POSIXct(dateRDM)){
      timeDifference = ((2400 - as.integer(as.character(timeRDM))) + as.integer(as.character(time7)))
      if (!is.na(timeDifference) && timeDifference <= 600){
        return('Y')
      }
      if(!is.na(timeDifference) && timeDifference > 600) {
        return('N')
      }
    }
  }
  if (as.POSIXct(date7) < as.POSIXct(dateOnset)) {
    #change to if eqal to dateOnset
    return('N')
  }
  #??????????????????????????????????????????????????????????????????????????????????????????????????????????????????
  if (as.POSIXct(date7) == as.POSIXct(dateOnset)){
    if (as.integer(as.character(time7)) > as.integer(as.character(timeOnset))){
      if (as.POSIXct(date7) == as.POSIXct(dateRDM)){
        if ((as.integer(as.character(time7)) - as.integer(as.character(timeRDM))) <= 600){
          return('Y')
        }
        if ((as.integer(as.character(time7)) - as.integer(as.character(timeRDM))) > 600){
          return('N')
        }
      }
      if (as.POSIXct(date7) > as.POSIXct(dateRDM)){
        timeDifference = ((2400 - as.integer(as.character(timeRDM))) + as.integer(as.character(time7)))
        if (!is.na(timeDifference) && timeDifference <= 600){
          return('Y')
        }
        if(!is.na(timeDifference) && timeDifference > 600) {
          return('N')
        }
      }
      if (as.POSIXct(date7) < as.POSIXct(dateRDM)){
        return('N')
      }
    }
    if (as.integer(as.character(time7)) <= as.integer(as.character(timeOnset))){
      return('N')
    }
  }
  #?????????????????????????????????????????????????????????????????????????????????????????????????????????????????
  if((is.na(time7) || is.na(date7) || is.na(timeRDM) || is.na(dateRDM) || is.na(timeOnset) || is.na(dateOnset))){
    return(NA)
  }
}

pregTest <- function(n){
  for (row in 1:nrow(plate7)){
    if (plate7$ptid[row] == n){
      testVal = plate7$preg_test[row]
    }
  }
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      sexVal = plate1$rdm_sex[row]
    }
  }
  if(!any(plate7$ptid == n) || !any(plate1$ptid == n)){
    return(NA)
  }
  if (sexVal == 0){
    if (testVal == 0){
      return('Y')
    } else {
      return('N')
    }
  }
  if (sexVal == 1){
    if (testVal == 1 || testVal == 2){
      return('Y')
    } else {
      return('N')
    }
  }
}

dtRandMatch <- function(n){
  #plate 1 vs plate 4
  for (row in 1:nrow(plate1)){
    if (plate1$ptid[row] == n){
      onsetDate = plate1$rdm_onset_date[row]
      onsetTime = plate1$rdm_onset_time[row]
    }
    if(!any(plate1$ptid == n)){
      return(NA)
    }
  }
  for (row in 1:nrow(plate4)){
    if(plate4$ptid[row] == n){
      onsetDate4 = plate4$date_onset[row]
      onsetTime4 = plate4$time_onset[row]
    }
    if(!any(plate1$ptid == n)){
      return(NA)
    }
  }
  if (!exists("onsetDate") || !exists("onsetDate4")){
    return(NA)
  }
  if (is.na(onsetDate) || is.na(onsetDate4)){
    return(NA)
  }
  if (onsetDate == onsetDate4 && onsetTime == onsetTime4){
    return('Y')
  } else {
    return('N')
  }
}

bpDIA <- function(n){
  for (row in 1:nrow(plate13)){
    if (plate13$ptid[row] == n){
      if (as.integer(as.character(plate13$ta_dbp[row])) >= 110 && !is.na(as.integer(as.character(plate13$ta_dbp[row])))){
        return('N')
      } else {
        return('Y')
      }
    }
    if(!any(plate13$ptid == n)){
      return(NA)
    }
  }
}

bpSYS <- function(n){
  for (row in 1:nrow(plate13)){
    if (plate13$ptid[row] == n){
      if (as.integer(as.character(plate13$ta_sbp[row])) >= 185 && !is.na(as.integer(as.character(plate13$ta_sbp[row])))){
        return('N')
      } else {
        return('Y')
      }
    }
    if(!any(plate13$ptid == n)){
      return(NA)
    }
  }
}

dtInformedConsent <- function(n){
  #plate 4 vs plate 4
  for (row in 1:nrow(plate4)){
    if (plate4$ptid[row] == n){
      dConsent = plate4$date_consent[row]
      tConsent = plate4$time_consent[row]
      dOnset = plate4$date_onset[row]
      tOnset = plate4$time_onset[row]
    }
    # if(!any(plate4$ptid) == n){
    #   return(NA)
    # }
  }
  if(!exists("dConsent") || !exists("tConsent") || !exists("dOnset") || !exists("tOnset")){
    return(NA)
  }
  if (as.POSIXct(dConsent) == as.POSIXct(dOnset)){
    if (tConsent > tOnset){
      return('Y')
    }
    if (tConsent <= tOnset){
      return('N')
    }
  }
  if (as.POSIXct(dConsent) > as.POSIXct(dOnset)){
    return('Y')
  }
  if (as.POSIXct(dConsent) < as.POSIXct(dOnset)){
    return('N')
  }
}

blNIHSSMatch <- function(n){
  for (row in 1:nrow(plate10)){
    if (plate10$ptid[row] == n){
      if (plate10$nihss2_visit_type[row] == 2){
        blNIHSS10 = plate10$NIHSS_Totscore[row]
      }
    }
  }
  for (row in 1:nrow(plate1)){
    if(plate1$ptid[row] == n){
      blNIHSS1 = plate1$rdm_nihss[row]
    }
  }
  if(!exists("blNIHSS10") || !exists("blNIHSS1")){
    return(NA)
  }
  if(blNIHSS1 == blNIHSS10){
    return('Y')
  } else {
    return('N')
  }
}

followUp24H <- function(n){
  for (row in 1:nrow(plate10)){
    if (plate10$ptid[row] == n){
      if (plate10$nihss2_visit_type[row] == 2){
        nihssBL = plate10$NIHSS_Totscore[row]
        nihssBL = as.numeric(nihssBL)
      }
      if (plate10$nihss2_visit_type[row] == 3){
        nihssCurrent = plate10$NIHSS_Totscore[row]
        nihssCurrent = as.numeric(nihssCurrent)
      }
    }
  }
  if (!exists("nihssBL") || !exists("nihssCurrent")){
    return(NA)
  }
  if(is.na(nihssBL) || is.na(nihssCurrent)){
    return(NA)
  }
  if ((nihssCurrent - nihssBL) >= 2){
    return('N')
  }
  if ((nihssCurrent - nihssBL) < 2){
    return('Y')
  }
}

followUp5D <- function(n){
  for (row in 1:nrow(plate10)){
    if (plate10$ptid[row] == n){
      if (plate10$nihss2_visit_type[row] == 2){
        nihssBL = plate10$NIHSS_Totscore[row]
        nihssBL = as.numeric(nihssBL)
      }
      if (plate10$nihss2_visit_type[row] == 4){
        nihssCurrent = plate10$NIHSS_Totscore[row]
        nihssCurrent = as.numeric(nihssCurrent)
      }
    }
  }
  if (!exists("nihssBL") || !exists("nihssCurrent")){
    return(NA)
  }
  if(is.na(nihssBL) || is.na(nihssCurrent)){
    return(NA)
  }
  if ((nihssCurrent - nihssBL) >= 2){
    return('N')
  }
  if ((nihssCurrent - nihssBL) < 2){
    return('Y')
  }
}

followUp90D <- function(n){
  for (row in 1:nrow(plate10)){
    if (plate10$ptid[row] == n){
      if (plate10$nihss2_visit_type[row] == 2){
        nihssBL = plate10$NIHSS_Totscore[row]
        nihssBL = as.numeric(nihssBL)
      }
      if (plate10$nihss2_visit_type[row] == 5){
        nihssCurrent = plate10$NIHSS_Totscore[row]
        nihssCurrent = as.numeric(nihssCurrent)
      }
    }
  }
  if (!exists("nihssBL") || !exists("nihssCurrent")){
    return(NA)
  }
  if(is.na(nihssBL) || is.na(nihssCurrent)){
    return(NA)
  }
  if ((nihssCurrent - nihssBL) >= 2){
    return('N')
  }
  if ((nihssCurrent - nihssBL) < 2){
    return('Y')
  }
}

mrsBLCheck <- function(n){
  for (row in 1:nrow(plate11)){
    if (plate11$ptid[row] == n){
      if (plate11$mrs_visit_type[row] == 2){
        mrsBL = plate11$MRS_Totscore[row]
      }
    }
  }
  if (!exists("mrsBL")){
    return(NA)
  }
  if (is.na(mrsBL)){
    return(NA)
  }
  if (mrsBL <= 2){
    return('Y')
  }
  if (mrsBL > 2){
    return('N')
  }
}

mrs90DCheck <- function(n){
  for (row in 1:nrow(plate11)){
    if (plate11$ptid[row] == n){
      if (plate11$mrs_visit_type[row] == 2){
        mrsBL = plate11$MRS_Totscore[row]
      }
      if (plate11$mrs_visit_type[row] == 5){
        mrs90D = plate11$MRS_Totscore[row]
      }
    }
  }
  if (!exists("mrsBL") || !exists("mrs90D")){
    return(NA)
  }
  if (is.na(mrsBL) || is.na(mrs90D)){
    return(NA)
  }
  if (mrs90D >= mrsBL){
    return('Y')
  }
  if (mrs90D < mrsBL){
    return('N')
  }
}

#This describes the household heads, not necessarily the patients

getmode <- function(x){
  uniqx <- unique(x)
  return(uniqx[which.max(tabulate(match(x, uniqx)))])
}

makeDemoKey <- function(){
  key <- list()
  key[["Age"]][["Values"]][[1]] <- 0:49
  key[["Age"]][["Values"]][[2]] <- 50:59
  key[["Age"]][["Values"]][[3]] <- 60:69
  key[["Age"]][["Values"]][[4]] <- 70:150
  key[["Age"]][["Levels"]] <- c("<50","50-59","60-69","70+")
  #not sure how to treat missing ages although there currently are none

  key[["Sex"]][["Values"]] <- 1:2; key[["Sex"]][["Levels"]] <- c("Male","Female")
  
  key[["Race"]][["Values"]] <- 1:4; key[["Race"]][["Levels"]] <- c("White","Black","Asian","Other")
  
  key[["Marital"]][["Values"]] <- 1:4; key[["Marital"]][["Levels"]] <- c("Married","Widowed","Divorced/Separated","Single")
  
  key[["RentOwn"]][["Values"]] <- 1:3; key[["RentOwn"]][["Levels"]] <- c("Owner","Renter","Other")
  
  key[["Region"]][["Values"]] <- 1:4; key[["Region"]][["Levels"]] <- c("Midwest","Northeast","South","West")
  
  key[["HHSize"]][["Values"]] <- 1:8; key[["HHSize"]][["Levels"]] <- c("One Person","Two People","Three People","Four People","Five People","Six People","Seven People","Eight or More People")
  
  key[["HHIncome"]][["Values"]] <- 1:12; key[["HHIncome"]][["Levels"]] <- c("$0,000-$9,999 per yr",
                                                            "$10,000-$11,999 per yr",
                                                            "$12,000-$14,999 per yr",
                                                            "$15,000-$19,999 per yr",
                                                            "$20,000-$24,999 per yr",
                                                            "$25,000-$34,999 per yr",
                                                            "$35,000-$44,999 per yr",
                                                            "$45,000-$49,999 per yr",
                                                            "$50,000-$59,999 per yr",
                                                            "$60,000-$69,999 per yr",
                                                            "$70,000-$99,999 per yr",
                                                            "$100,000+ per yr")
  
  key[["Educ"]][["Values"]] <- c(1:7,99); key[["Educ"]][["Levels"]] <- c("Grade School","Some High School","Graduated High School","Some College","Graduated College","Post Graduate School","No Household Head Present","NA")
  
  return(key)
}

useDemoKey <- function(descr,key){ #converts values in descr dataframe to factors
  descr2 <- data.frame(matrix(nrow = nrow(descr),ncol = ncol(descr)))
  colnames(descr2) <- colnames(descr)
  descr2$panelid <- descr$panelid
  for(colname in names(descr)[-1]){            #loop thru each column except panelids
    for(i in 1:length(descr[[colname]])){      #loop thru each element in the column
      for(j in 1:length(key[[colname]][["Values"]])){ #loop thru each key in the list and compare to element
        x <- descr[[colname]][i]
        if(colname == "Age"){
          if(x %in% key[[colname]][["Values"]][[j]]){descr2[[colname]][i] <- key[[colname]][["Levels"]][j]}
        }else{
          if(x == key[[colname]][["Values"]][j]){descr2[[colname]][i] <- key[[colname]][["Levels"]][j]}
        }
      }
      descr2[[colname]] <- factor(descr2[[colname]],levels = key[[colname]][["Levels"]])
    }
  }
  return(descr2)
}

descrSample <- function(demo = NA,trans = NA, panelids = NA){
  #these are for previously specified HHsizes AND Rx_Brands (from getPanelIDs)

  if(is.na(panelids)){
    ids <- sort(unique(trans$panelid))
  }else{ ids <- panelids}
  
  demo.sub <- demo[demo$panelid %in% ids,]
  
  descr <- data.frame(matrix(nrow = length(ids), ncol=10))
  colnames(descr) <- c("panelid","Age","Head","Race","Marital","RentOwn","Region","HHSize","HHIncome","Educ")

  descr <- demo.sub %>% group_by(panelid) %>% summarize(
    Head = (head),
    MBday = getmode(male_head_birth),
    FBday = getmode(female_head_birth), #either male or female is head. both are never missing.
    Race = getmode(race),
    Marital = getmode(marital),
    RentOwn = getmode(rentown),
    Region = getmode(region),
    HHSize = getmode(hhsize),
    HHIncome = getmode(hhinc),
    EducM = getmode(med),
    EducF = getmode(fed)
  )
  descr <- as.data.frame(descr)

  educ <- rep(NA,dim(descr)[1])
  bday <- rep(NA,dim(descr)[1])
  for(i in 1:dim(descr)[1]){ #the gender loop
    if(descr$Head[i] == 1){
      yr <- substr(as.character(descr$MBday[i]),nchar(descr$MBday[i])-3,nchar(descr$MBday[i])) #the year is the last four characters of the string
      mo <- substr(as.character(descr$MBday[i]),1,nchar(descr$MBday[i])-4) #the month is the last four characters of the string
      educ[i] <- descr$EducM[i]
    }else if(descr$Head[i] == 2){
      yr <- substr(as.character(descr$FBday[i]),nchar(descr$FBday[i])-3,nchar(descr$FBday[i])) 
      mo <- substr(as.character(descr$FBday[i]),1,nchar(descr$FBday[i])-4)
      educ[i] <- descr$EducF[i]
    }else{
      yr <- "0000"; mo <- "00"
    }
    bday[i] <- (paste(yr, "-", mo, "-01",sep=""))
  }

  descr$Age <- as.integer((as.Date("2012-12-30") - as.Date(bday))/365.25) #Dec 30, 2012 is the last measurement day in all the data
  descr$MBday <- NULL; descr$FBday <- NULL;
  descr$EducM <- NULL; descr$EducF <- NULL
  descr$Educ <- educ
  descr$Sex <- descr$Head; descr$Head <- NULL
  
  return(descr)
}

viewDescrSample <- function(descr,DemoKey){
  descr <- useDemoKey(descr, DemoKey)
  x <- list() #creating a list of tables to display
  for(i in 2:length(names(descr))){ #for each column except panelids
    col <- (names(descr)[i])
    x[[col]] <- data.frame(round(table(descr[[col]])/length(descr[[col]]),2))
    colnames(x[[col]])[1] <- col
  }
  return(x)
}

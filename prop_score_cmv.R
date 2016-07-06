findMatches <- function(server = "SQLServer_IRI", panelids = NA, Brands = NA, Conditions = NA,demo = NA, rx = NA,
                        remove_cols = NA){
 

  #call medprofiler10, do some column editing
  ch <- odbcConnect(server)
  mp10 <- sqlFetch(ch,"MEDPROFILER10")
  mp11 <- sqlFetch(ch,"MEDPROFILER11")
  mp12 <- sqlFetch(ch,"MEDPROFILER12")

  #Subset columns of mp10, 11, 12, to columns that are present in all 3
  names10 <- intersect(names(mp10),names(mp11))
  names10 <- intersect(names10, names(mp12))
  
  #subset mp11 to panelids not in mp10, rbind to mp10
  missing_ids <- panelids[panelids %in% mp10$panelID]
  for(id in missing_ids){
    if(id %in% mp11$panelID){
      
    }else if(id %in% mp12$panelID){
      
    }
  }
  
  #subset mp12 to ids not in mp10, 11. Rbind to mp10, 11
  
  medprof$panelid <- medprof$panelID
  medprof$panelID <- NULL
  medprof$MemberID <- NULL
  medprof$BirthDate <- NULL
  medprof$Ht_In <- medprof$Height_Ft*12 + medprof$Height_In
  medprof$Height_Ft <- NULL; medprof$Height_In <- NULL
  
  #For example, if we are looking for matches with diabetes,
  #we want to remove the medprof columns that ask "Do you have diabetes?"
  medprof <- medprof[ , !(colnames(medprof) %in% remove_cols)]  
 
  
  #some people who reported themselves as single-person households have multiple members in medprof
  #we are getting rid of these
  demo_1 <- demo[demo$hhsize == 1, ]
  med_1 <- medprof[medprof$panelid %in% demo_1$panelid, ]
  
  duplicated <- duplicated(med_1$panelid)
  med_1 <- med_1[!med_1$panelid %in% med_1$panelid[duplicated] , ]
  medprof <- medprof[medprof$panelid %in% med_1$panelid, ]
  
  #Brand Condition Matrix
  Matches <- data.frame(panelid = unique(demo$panelid))
  #only use IDs that have taken specified brands; uses all IDs if none are specified
  if(!is.na(Brands)){
    brandids <- rx$panelid[rx$Rx_Brand %in% Brands]
    BrandMatch <- (Matches$panelid %in% brandids)
  }else{
    BrandMatch <- rep(TRUE,(dim(Matches)[1]))
  }
  
  #only use IDs that have specified conditions; uses all IDs if none are specified
  if(!is.na(Conditions)){
    condids <- rx$panelid[rx$Medical_Condition %in% Conditions]
    CondMatch <- (Matches$panelid %in% condids)
  }else{
    CondMatch <- rep(TRUE,(dim(Matches)[1]))
  }
  
  if(!is.na(panelids)){
    IDMatch <- Matches$panelid %in% panelids
  }else{
    IDMatch <- rep(TRUE,dim(Matches)[1])
  }
    
  
  #Match=1 when a person meets all of the desired brand and condition requirements
  Matches$Match <- as.numeric(BrandMatch & CondMatch & IDMatch)

  

  
  
  
  #this may be flawed merging practice. We will investigate further.
###########################################################################################################################3  
  #merge demo info, medprof, and Match column by panelid (demo + medprof = demo_mp)
  #keep only matches
  Matches_m <- Matches[Matches$Match == 1, ]
  temp <- merge(medprof, Matches_m,by="panelid", all = T)
  temp$Match[is.na(temp$Match)] <- 0
  demo_mp <- merge(temp,demo_1,by = "panelid")

###########################################################################################################################3  
  
  
  
  
  
  

  
  
  
  #some special columns that need to be handled differently than survey questions
  continuous <- c("Age","Ht_In","Weight")
  projections <- grep("projection", colnames(demo_mp),value = T)
  birth <- grep("birth",colnames(demo_mp),value = T)
  relationship <- grep("relationship",colnames(demo_mp),value = T)
  too_many_levels <- c("panelid","state","zipcode","statecounty","blockGroup","marketid")
  
  #remove these
  demo_mp <- demo_mp[,!names(demo_mp) %in% c("hhid", projections,birth,relationship)]
  
  #make all the non-numeric columns ordered factors
  for(col in names(demo_mp)){
    if(!(col %in% continuous) & !(col %in% too_many_levels)){
      demo_mp[[col]] <- factor(demo_mp[[col]],ordered = T)
    }
  }
  
  remove <- c("zipcode", "marketid", "Question1", "state", "statecounty", "hhsize", "ac")
  demo_mp_s <- demo_mp[ , !colnames(demo_mp) %in% remove]
  d <- mice(data = demo_mp_s, m = 1, maxit = 5 , method = "pmm", seed = 10)
  
  return(demo_mp)
  
  
  
}




addPScore <- function(demo_mp){

  #just a quick fix to get rid of NAs
  demo_mp$Weight[is.na(demo_mp$Weight)] <- mean(demo_mp$Weight,na.rm =T)
  demo_mp$Ht_In[is.na(demo_mp$Ht_In)] <- mean(demo_mp$Ht_In,na.rm =T)

  #apply matching algorithm to columns (except the ones with too many levels, and Match)
  too_many_levels <- c("panelid","state","zipcode","statecounty","blockGroup","marketid") 
  omitcols <- !names(demo_mp) %in% too_many_levels
  name <- names(demo_mp[,omitcols])
  name <- name[name != "Match"]
  covar <- paste(name, collapse = " + ")
  mod <- paste("match <- matchit(formula = Match ~", covar, ", data = demo_mp[,omitcols] )")
  matches <- eval(parse(text = mod))
  
  #add p-score column to merged dataset
  model <- glm(formula = Match ~ ., data = demo_mp[,omitcols] ,family = "binomial")
  pred <- predict.glm(model,demo_mp,"response")
  demo_mp$Control <- as.numeric(rownames(demo_mp) %in% matches$match.matrix)
  demo_mp$PScore <- pred

  return(demo_mp)
}


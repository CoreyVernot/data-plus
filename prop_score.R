#binarification cutoffs have inclusive upper bound: [0,x] (x,5] are the two categories
#if zero_is_na then bounds are [1,x] (x,5]

findMatches <- function(server = "SQLServer_IRI", panelids = NA, Brands = NA, Conditions = NA,
                        demo = NA, rx = NA, remove_cols = NA){
 

  #call medprofiler10, do some column editing
  ch <- odbcConnect(server)
  mp10 <- sqlFetch(ch,"MEDPROFILER10")
  mp11 <- sqlFetch(ch,"MEDPROFILER11")
  mp12 <- sqlFetch(ch,"MEDPROFILER12")

  #Subset columns of mp10, 11, 12, to columns that are present in all 3
  names10 <- intersect(names(mp10),names(mp11))
  names10 <- intersect(names10, names(mp12))
  mp10 <- mp10[,names(mp10) %in% names10]
  mp11 <- mp11[,names(mp11) %in% names10]
  mp12 <- mp12[,names(mp12) %in% names10]
  
  #subset mp11 to panelids not in mp10, rbind to mp10
  #repeat with mp12
  mp11_b <- mp11[!mp11$panelID %in% mp10$panelID,]
  mp10_n <- rbind(mp10, mp11_b)
  
  mp12_b <- mp12[!mp12$panelID %in% mp10_n$panelID,]
  mp10 <- rbind(mp10_n,mp12_b)

  medprof <- mp10
  medprof$panelid <- medprof$panelID
  medprof$panelID <- NULL
  medprof$MemberID <- NULL
  medprof$BirthDate <- NULL
  
  #heights
  medprof$Ht_In <- rep(NA,dim(medprof)[1])
  medprof$Height_Ft <- replace(medprof$Height_Ft, medprof$Height_Ft < 0 | medprof$Height_Ft > 6, NA) 
  #the heights of ridiculously tall people and negative people are set to NA to be imputed later
  medprof$Height_In <- replace(medprof$Height_In, medprof$Height_In < 0, NA)
  medprof$Ht_In <- medprof$Height_Ft*12 + medprof$Height_In
  medprof$Ht_In <- replace(medprof$Ht_In, medprof$Ht_In < 5, NA)
  #the heights of ridiculously short people are also set to NA
  medprof$Height_Ft <- NULL; medprof$Height_In <- NULL
  
  #For example, if we are looking for matches with diabetes,
  #we want to remove the medprof columns that ask "Do you have diabetes?"
  cat("removed from covariates:",remove_cols[remove_cols %in% names(medprof)],"\n",sep="\t")
  medprof <- medprof[ , !(names(medprof) %in% remove_cols)]  
 
  
  #some people who reported themselves as single-person households have multiple members in medprof
  #we are getting rid of these
  demo_1 <- demo[demo$hhsize == 1, ]
  med_1 <- medprof[medprof$panelid %in% demo_1$panelid, ]
  
  duplicated <- duplicated(med_1$panelid)
  med_1 <- med_1[!med_1$panelid %in% med_1$panelid[duplicated] , ]
  medprof <- medprof[medprof$panelid %in% med_1$panelid, ]
  
  #Brand Condition Matrix
  Matches <- data.frame(panelid = unique(demo_1$panelid))
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
  #we should put mice code right above this chunk
  
  Matches_m <- Matches[Matches$Match == 1,]
  temp <- merge(medprof,Matches_m,by="panelid", all = T)
  temp$Match[is.na(temp$Match)] <- 0
  demo_mp <- merge(temp,demo_1,by = "panelid")

###########################################################################################################################3  

  
  
  
  
  

  
  
  
  #some special columns that need to be handled differently than survey questions
  continuous <- c("Age","Ht_In","Weight")
  projections <- grep("projection", colnames(demo_mp),value = T)
  birth <- grep("birth",colnames(demo_mp),value = T)
  relationship <- grep("relationship",colnames(demo_mp),value = T)
  too_many_levels <- c("panelid","state","zipcode","statecounty","blockGroup","marketid")
  occ <- c("focc","mocc")
  unordered <- c("head","sex","cats","dogs","hhtype","marital","hisp","race","region" ,"rentown", "mocc","focc","memp","femp")

  #remove these
  demo_mp <- demo_mp[,!names(demo_mp) %in% c("hhid", "ac", too_many_levels[-1], projections,birth,relationship)]
  
  #we need to impute continuous variables
  #just for testing
#  demo_mp$Weight[is.na(demo_mp$Weight)] <- mean(demo_mp$Weight,na.rm =T)
#  demo_mp$Ht_In[is.na(demo_mp$Ht_In)] <- mean(demo_mp$Ht_In,na.rm =T)
#  demo_mp$Age[is.na(demo_mp$Age)] <- mean(demo_mp$Age,na.rm =T)
  
  #make all the non-numeric columns ordered factors
  for(col in names(demo_mp)){
    if(col %in% unordered){
      demo_mp[[col]] <- factor(demo_mp[[col]],ordered = F)
    }
    else if(!(col %in% continuous) & !(col %in% too_many_levels)){
      demo_mp[[col]] <- factor(demo_mp[[col]],ordered = T)
    }
  }
  return(demo_mp)
}
print(dim(demo_mp_fxn))
for(r in 1:dim(demo_mp_fxn[1])){
  if(NA %in% demo_mp_fxn[r,]){demo_mp_fxn <- demo_mp_fxn[-i,]}
}
addPScore <- function(demo_mp){

  #apply matching algorithm to columns (except the ones with too many levels, and Match)
  too_many_levels <- c("panelid","state","zipcode","statecounty","blockGroup","marketid") 
  one_level <- colnames(demo_mp)[lapply(demo_mp, function(x){return(length(unique(x)))}) == 1]
  one_level <- one_level[one_level != "Match"]
  omitcols <- !names(demo_mp) %in% c(too_many_levels, one_level)
  name <- names(demo_mp[,omitcols])
  name <- name[name != "Match"]
  covar <- paste(name, collapse = " + ")
  mod <- paste("match <- matchit(formula = Match ~", covar, ", data = demo_mp[,omitcols] )")
  matches <- eval(parse(text = mod))
  mm <- matches$match.matrix
  
  #add p-score column to merged dataset
  model <- glm(formula = Match ~ ., data = demo_mp[,omitcols] ,family = "binomial")
  pred <- predict.glm(model,demo_mp,"response")

  demo_mp$Control <- as.numeric(rownames(demo_mp) %in% mm)
  demo_mp$PScore <- pred
  
  #panelid of nearest neighbor match for person in control
  key_df <- data.frame(panelid = demo_mp[rownames(mm), ]$panelid, 
                       key = demo_mp[mm, ]$panelid)
  dim(key_df)
  dim(demo_mp)
  mean(demo_mp$panelid[demo_mp$Match == 1] %in% key_df$panelid)
  mean(key_df$panelid %in% demo_mp$panelid[demo_mp$Match == 1])
  mean(demo_mp$panelid[demo_mp$Control == 1] %in% key_df$key)
  mean(key_df$key %in% demo_mp$panelid[demo_mp$Control == 1])
  
  demo_mp_r <- merge(demo_mp,key_df,by="panelid", all.x =T)
  
  return(list(Data = demo_mp_r, Model = model))
}

demo_mp_fxn <- findMatches(panelids = panelids, demo=demo,rx=rx,remove_cols = c("DiabetesI","DiabetesII"
,"Low_sugar_diet"))

didPscoreWork <- function(demo_mp_r){}

tab <- data.frame(matrix(ncol = 2, nrow = dim(demo_mp_r)[2]))
for(i in 1:dim(demo_mp_r)[2]){ #loop thru each covariate (columns)
  i <- 64
  col <- (demo_mp_r[[names(demo_mp_r)[i]]])
  print(col)
  tab[i,1] <- t.test(col[demo_mp_r$Match == 1],
                     col[demo_mp_r$Match != 1])$test.statistic #compare matches to original
  tab[i,2] <- t.test(col[demo_mp_r$Match == 0],
                     col[demo_mp_r$Control == 1])$test.statistic #compare matches to controls
}
tab
sum(as.numeric(demo_mp_r$Match[demo_mp_r$Control==1]))

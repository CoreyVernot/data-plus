
####DON'T USE THIS CODE!!!!!!!!!1 ####
library(RODBC)
getRx <- function(years = c(10,11,12), server = "SQLServer_IRI" ){
  ch <- odbcConnect(server)
  rx <- data.frame()
  allRx <- paste("Rx",years,sep="")
  for(i in 1:length(years)){
    rx_i <- sqlFetch(ch,allRx[i])
    rx <- rbind(rx, rx_i)
  }
  #rx_sub <- data.frame(rx$panelid,rx$Birth_Year,rx$Sex,rx$Week,rx$Rx_Brand)
  #colnames(rx_sub) <- c("panelid","Birth_Year","Sex","Week","Rx_Brand")
  return(rx)
} 

getDemo <- function(server = "SQLServer_IRI"){
  ch <- odbcConnect(server)
  demo <- sqlFetch(ch, "DEMO")
  #demo_sub <- data.frame(demo$panelid,demo$hhsize)
  #colnames(demo_sub) <- c("panelid","hhsize")
  return(demo)
}

getPanelIDs <- function(..., rx, demo,HHSizes=1){
  allBrands <- list(...)
  #the drugs are vectors of brands that contain a certain drug of interest
  #ID <- rx$panelid[rx$Rx_Brand %in% Drug1,]  #we'll want to return ID's (panel or HH?) of ppl who took this drug and are in a certain household size
  panelIDs <- list()
  demoids <- demo$panelid[demo$hhsize %in% HHSizes]
  
  #cleaning brand names
  
  #eliminate all whitespace and non-character/number things
  rx$Rx_Brand_Print <- rx$Rx_Brand
  rx$Rx_Brand <- tolower(rx$Rx_Brand)
  rx$Rx_Brand <- gsub("[^[:alnum:]]", "", rx$Rx_Brand)
  allBrands <- lapply( allBrands, function(x){ tolower(gsub("[^[:alnum:]]", "", x))})

  for(i in 1:length(allBrands)){
    brands <- allBrands[[i]]
    expr <- paste("(", paste(brands, collapse = "|"), ")", sep = "")
    index <- grep(expr, rx$Rx_Brand)
    cat("Drug brands in database matching drug set ",i,":", "\n",sep="")
    cat(unique(as.vector((rx$Rx_Brand_Print[index]))),"\n","\n",sep="    ")
    rxids <- unique(rx$panelid[index])
    ids <- rxids[rxids %in% demoids]
    panelIDs$IDs[[i]] <- ids
    panelIDs$Brands[[i]] <- unique(as.vector((rx$Rx_Brand_Print[index])))
  }
  return(panelIDs)
}
#end####


#TESTING STUFF
B11 <- "Metformin"; B12 <- "Plavix"
B21 <- "ALLOPURINOL"; B22 <- "COSOPT"
D1 <- c(B11,B12)
D2 <- c(B21,B22)
D3 <- c("not in dataset")

server <- "SQLServer_IRI"
rx <- (getRx(c(10,11,12), server))
demo <- (getDemo(server))
panelids <- getPanelIDs(D1,D2,D3, rx=rx, demo=demo)
rm(B11,B12,B22,B21,D1,D2,D3,server)

#source: www.diabetes.org/living-with-diabetes/treatment-and-care/medication/oral-medications/what-are-my-options.html?referrer=https://google.com/
#http://www.webmd.com/diabetes/sulfonylureas-for-type-2-diabetes
sulfonylureas <-  c("Diabinese", "Glucotrol", "Micronase","Glynase", "Diabeta",
                    "Amaryl", "chlorpropamide", "glimepiride", "glipizide", "glyburide",
                    "tolazamide", "tolbutamide")
#associated with increased appetite/weight!!! (because it increases insulin production)
#http://www.diabetesselfmanagement.com/diabetes-resources/definitions/sulfonylureas/
#https://books.google.com/books?id=KhPSBQAAQBAJ&pg=PA357&lpg=PA357&dq=sulfonylureas+insulin+%22appetite%22&source=bl&ots=Ncb7ny2Q0X&sig=kqDw1osroR5dO8KP6GMOY-W4u6o&hl=en&sa=X&ved=0ahUKEwj2saXky-TNAhXC6iYKHUQcBwgQ6AEIKjAC#v=onepage&q=sulfonylureas%20insulin%20%22appetite%22&f=false



biguanides <- c("Fortamet", "Glucophage", "Glumetza", "Riomet", "Obimet",
                  "Dianben", "Diaformin", "Siofor", "Metfogamma", "Janumet",
                  "Kazano", "Invokamet", "Xigduo", "Synjardy", "Metaglip" ,
                  "Jentaduo" , "Actoplus", "Prandimet", "Avandamet", "Kombiglyze", 
                  "Glumetza", "Metformin")
#associated with decreased appetite/weight!!!
#the pdf corey emailed us



meglitinides <- c("Prandin","Starlix")
#associated with weight gain (because it increases insulin production)
#http://www.webmd.com/diabetes/meglitinides-for-type-2-diabetes


thiazolidinediones <- c("Avandia", "ACTOS", "Rezulin")
#associated with weight gain!!!
#http://www.nytimes.com/health/guides/disease/type-2-diabetes/medications.html



dpp_4_inhibitors <- c("Januvia","Onglyza","Tradjenta","Nesina")
#neutral
#http://care.diabetesjournals.org/content/34/Supplement_2/S276


sglt_2_inhibitors <- c("SGLT2","Invokana","Farxiga")
#weight loss?
#http://care.diabetesjournals.org/content/38/3/352


alpha_glucosidase_inhibitors <- c("Precose","Glyset")
#These last two aren't that big anyways.


bile_acid_sequestrants <- c("Welchol")
#I'll probably get to them later.


oral_combination_therapy <- NA
#Why did I even include this one?


insulin <- NA 
#not even a drug, but a therapy. It is associated with weight gain though, in case you were curious
#http://www.nytimes.com/health/guides/disease/type-2-diabetes/medications.html



Diabetes_IDs <- getPanelIDs(sulfonylureas, biguanides, meglitinides, thiazolidinediones,
                           dpp4_inhibitors, sglt2_inhibitors, alpha_glucosidase_inhibitors,
                           bile_acid_sequestrants, oral_combination_therapy,demo=demo,rx=rx)

length(Diabetes_IDs[[1]][[1]])

#Does every RxID appear in purchase data?
#des08 <- sqlFetch(ch, "BAK_DES08")
#mean(panelids[[1]] %in% (des08$panelid)) #76%
#mean(panelids[[2]] %in% (des08$panelid)) #78%

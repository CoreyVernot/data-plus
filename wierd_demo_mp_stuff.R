demo <- getDemo()
ch <- odbcConnect("SQLServer_IRI")
mp10 <- sqlFetch(ch,"MEDPROFILER10")

rx <- getRx()

demo_1 <- demo[demo$hhsize == 1, ]
med_1 <- mp10[mp10$panelID %in% demo_1$panelid, ]

length(unique(med_1$panelID))
nrow(med_1)
duplicated <- duplicated(med_1$panelID)
med_1$dup <- med_1$panelID %in% med_1$panelID[duplicated]
med_1 <- med_1[!med_1$dup, ]



remove_cols <- c("DiabetesI", "DiabetesII", "Diabetes_Concern", "Low_sugar_diet", "Low_carb_diet")





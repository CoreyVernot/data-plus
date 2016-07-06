de_id <- function(data, colnames = NA, id_key = read.csv("id_key.csv")){
	data_keep <- merge(data, id_key, by="panelid") #merge by panelid
	data_keep <- data_keep[,colnames(data_keep) != "panelid"] #remove panelid column
	if(!is.na(colnames)){
		colnames <- c(colnames,"new_id") 		#add "new_id" column
		data_keep <- data_keep[, colnames(data_keep) %in% colnames]
	}
	return(data_keep)
}
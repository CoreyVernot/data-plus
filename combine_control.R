combine_control<- function(trans_cl, control_cl){
  control_cl$k <- "control"
  control_cl$k_zero_timeunit <- "control"
  control_cl <- control_cl[! control_cl$panelid %in% trans_cl$panelid, ] # keep only control individuals that are not in our treatment group
  trans_cl_m <- trans_cl[, colnames(trans_cl) %in% colnames(control_cl)]
  control_cl <- control_cl[, colnames(control_cl) %in% colnames(trans_cl_m)]
  Trans_cl <- rbind(trans_cl_m, control_cl)
  return(Trans_cl)
}

combine_taking <- function(trans_cl, taking, control_cl){
  trans_take <- merge(trans_cl, taking, by = c("panelid", "timeunit"), all.x = T)
  
  control_cl$k <- "control"
  control_cl$k_zero_timeunit <- "control"
  control_cl <- control_cl[! control_cl$panelid %in% trans_cl$panelid, ] # keep only control individuals that are not in our treatment group
  trans_cl_m <- trans_cl[, colnames(trans_cl) %in% colnames(control_cl)]
  control_cl <- control_cl[, colnames(control_cl) %in% colnames(trans_cl_m)]
  Trans_cl <- rbind(trans_cl_m, control_cl)
  return(Trans_cl)
}

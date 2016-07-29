getDemo <- function(server = "SQLServer_IRI"){
  ch <- odbcConnect(server)
  demo <- sqlFetch(ch, "DEMO")
  return(demo)
}

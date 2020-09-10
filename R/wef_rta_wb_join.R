##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' @title
##' @param data
##' @param 
wef_rta_wb_join <- function(data,
                            data_new_path) {
  # read new data
  data_new <- read_excel(data_new_path) %>% 
    mutate_all(as.character) %>% 
    mutate(values = as.numeric(values))
  # join data
  data <- data %>% 
    bind_rows(data_new)
  # return data
  return(data)

}

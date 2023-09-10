#' data ophalen van github of uit de data map
#' 
get_data <- function(){
        
        # haal data van de github ----
        dt <- readr::read_csv2(
                "https://raw.githubusercontent.com/mhschel/lerarentekort/master/data_files/stage_opdracht_dataset.csv")
        
        # print data ----
        dt   %>% 
                filter(
                        ll_lr_ratio > 5 & ll_lr_ratio < 50
                )
}
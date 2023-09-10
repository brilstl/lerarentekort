#' toevoegen van geografische dimensie aan de data
#' 
#' @description 
#' 
#' Door een aantal registratie fouten in de data kan de statcode niet worden gebruikt
#' hierdoor wordt middels de naam van de gemeente opnieuw een koppeling worden gemaakt
#' met de geografische grenzen
#' 
#' @param x data.frame met de variabel gemeete

get_geo <- function(x){
        
        # haal geo data op van pdok ----
        geo_dt <- st_read(
                "https://service.pdok.nl/cbs/gebiedsindelingen/2019/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json",
                quiet = TRUE
        )
        
        # hernoem drie missende gemeente ----
        x <- x %>% 
                mutate(gemeente = case_when(
                        gemeente == "NUENEN C.A." ~ toupper("Nuenen, Gerwen en Nederwetten"),
                        gemeente == "SUDWEST-FRYSLAN" ~ toupper("Súdwest-Fryslân"),
                        gemeente == "NOARDEAST-FRYSLAN" ~ toupper("Noardeast-Fryslân"),
                        TRUE ~ gemeente
                )
                )
        
        # koppel geo data ----
        x <- 
                x %>% 
                left_join(geo_dt %>% 
                                  mutate(statnaam = toupper(statnaam)), 
                          by = c("gemeente" = "statnaam"))
        
        # print geo ----
        x
}


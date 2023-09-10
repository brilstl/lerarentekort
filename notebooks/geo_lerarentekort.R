# libraries ----
library(tidyverse)
library(ggtext)
library(sf)

# functies inladen ----
invisible(
        lapply(list.files("R", full.names = TRUE), source)
)

# haal data op ----
dt <- get_data() 

# bereken gemiddelde ll_lr_ratio ----
dt_geo <- 
        dt %>% 
        filter(jaar == 2019) %>% 
        group_by(gemeente) %>% 
        summarise(
                across(
                        ll_lr_ratio,
                        mean,
                        na.rm = TRUE
                )
        ) %>% 
        ungroup

# maak quantiele ---
dt_geo <- 
        dt_geo %>% 
        mutate(quant = cut_number(ll_lr_ratio, 3))
# plot maken ----
dt_geo %>% 
        get_geo() %>% 
        st_as_sf() %>% 
        ggplot(
                aes(
                        fill = quant,
                )
        ) +
        geom_sf(colour = "transparent", size = 1) +
        scale_fill_manual(values = rev(c("#004699", "#707ebb", "#b8bcdd")),
                          labels = c("11 tot 18", "18 tot 19", "19 tot 26"),
                        ) +
        theme_void() + 
        theme(strip.background = element_blank(),
              legend.title = element_markdown(size = 21),
              legend.text = element_markdown(size = 21),
              strip.text = element_textbox(
                      size = 21,
                      color = "white", fill = "#D33F49", box.color = "#D33F49",
                      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)), 
              legend.position = "right") +
        labs(fill = "leerling-leraarratio")

# save plot ----
ggsave("img/ll_lr_ratio_geo.png")

# packages ----
library(tidyverse)
library(ggtext)

# functies inladen ----
invisible(
        lapply(list.files("R", full.names = TRUE), source)
)

# data ophalen ----
dt <- get_data()

# data transformatie ----
dt_ratio <- 
        dt %>%
        group_by(jaar) %>% 
        summarise(
                across(
                        c(leerlingen, onderwijzers, ll_lr_ratio), 
                        mean, 
                        na.rm = TRUE
                        )
        ) %>% 
        pivot_longer(
                leerlingen:ll_lr_ratio
        ) %>% 
        mutate(
                name = case_when(
                        name == "ll_lr_ratio" ~ "leerling-leraarratio",
                        TRUE ~ name
                ),
                name = as.factor(name),
                name = forcats::fct_relevel(name, "leerling-leraarratio", after = Inf)
        )

# plot ----

dt_ratio %>% 
        ggplot(
                aes(
                        x = jaar,
                        y = value,
                        colour = name
                )) +
        scale_x_continuous(breaks = seq(2006,2019, 1)) +
        geom_line(size = .9) +
        scale_colour_manual(values = c("#D33F49", "darkblue", "darkgreen")) +
        facet_wrap(vars(name), scales = "free_y", ncol = 1) +
        scale_y_continuous(labels = function(x) prettyNum(x, decimal.mark = ",")) +
        theme_pres() +
        labs(x = "jaartal", y = "gemiddelde\naantallen")

# save plot ----
ggsave("img/ll_lr_ratio.png")

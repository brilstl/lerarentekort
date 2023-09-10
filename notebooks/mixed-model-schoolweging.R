# libraries ----
library(tidyverse)
library(ggtext)
library(lme4)


# functies inladen ----
invisible(
        lapply(list.files("R", full.names = TRUE), source)
)

# haal data op ----
dt <- get_data() 

# data prep ----
dt_lm <- 
        dt %>% 
        filter(jaar > 2016) %>% 
        transmute(
                jaar, 
                ll_lr_ratio, 
                quant_weging = cut_number(schoolweging, 3),
                gemeente,
                brin)

# mixed model regressie ----

lm_ratio <- 
        lmer(ll_lr_ratio ~ gemeente*quant_weging + jaar + (1|brin), data = dt_lm)


# casus data ----
casus_data <- 
        tibble(
        jaar = 2019,
        gemeente = c(rep("AMSTERDAM",3), rep("ALMERE",3), rep("ROTTERDAM",3)),
        quant_weging = rep(unique(
                dt_lm$quant_weging
                )[!is.na(unique(dt_lm$quant_weging))], 3)
        
)

# gewogen gemiddelde na controle voor jaar en schoolweging
casus <- predict(lm_ratio, newdata = casus_data, re.form = NA, se.fit = TRUE) #predict(lm_ratio, casus_data, se.fit = TRUE)

# plot ----
casus_data %>% 
        mutate(.fitted = casus) %>% 
        ggplot(
                aes(
                        x = .fitted,
                        y = gemeente,
                        fill = quant_weging
                )) +
        scale_fill_manual(values = c("#D33F49", "darkblue", "darkgreen"),
                          labels = c("laag", "midden", "hoog")) +
        geom_col(position = position_dodge()) +
        theme_pres() +
        theme(
                legend.position = "right"
        ) +
        guides(fill = guide_legend(reverse = T)) +
        labs(x = "leerling-leraarratio", y = "", fill = "schoolweging")

# save plot ----
ggsave("img/schoolweging.png")

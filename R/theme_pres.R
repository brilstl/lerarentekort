#' thema voor de presentatie in ggplot2
#' 
theme_pres <- function(){
        
        theme_bw() +
                theme(legend.position = "none",
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(colour = "grey85", size = .5),
                      panel.border = element_blank(),
                      axis.line.x = element_line(size = .8),
                      axis.ticks.y = element_line(colour = 'white'),
                      plot.caption = element_markdown(lineheight = 1.2, size = 12),
                      plot.background = element_blank(),
                      panel.background = element_blank(),
                      legend.title = element_markdown(size = 21),
                      legend.text = element_markdown(size = 21),
                      strip.background = element_blank(),
                      axis.text = element_markdown(size = 21),
                      axis.title = element_markdown(size = 21),
                      strip.text = element_textbox(
                              size = 25,
                              color = "black", fill = "grey85", box.color = "grey85",
                              halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
                              padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)))
        
}
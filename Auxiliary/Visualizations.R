### Tree, Conditional Variable Importance

CVI_vis <- function(){
    
    my_data <- tibble(x_1 = runif(200, 0, 10),
                      x_2 = runif(200, 0, 10))                       
    
    p1 <- ggplot() +
        ggtitle("Partition created by Tree") +
        geom_point(data = my_data, aes(x = x_1, y = x_2)) +
        xlim(0, 10) + ylim(0, 10) +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 0, ymax = 3), fill = 'red', alpha = 0.4) +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 3, ymax = 10), fill = 'green', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 8, ymin = 0, ymax = 5), fill = 'blue', alpha = 0.4) +
        geom_rect(aes(xmin = 8, xmax = 10, ymin = 0, ymax = 5), fill = 'darkgrey', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 10, ymin = 5, ymax = 7.5), fill = 'purple', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 10, ymin = 7.5, ymax = 10), fill = 'yellow', alpha = 0.4) +
        theme_light() +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p2 <- ggplot() +
        ggtitle("Resulting Regions for in-group Permutation") +
        geom_point(data = my_data, aes(x = x_1, y = x_2)) +
        theme_light() +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 0, ymax = 3), fill = 'red', alpha = 0.4) +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 3, ymax = 5), fill = 'cyan', alpha = 0.4) +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 5, ymax = 7.5), fill = 'chocolate4', alpha = 0.4) +
        geom_rect(aes(xmin = 0, xmax = 4, ymin = 7.5, ymax = 10), fill = 'chartreuse', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 8, ymin = 0, ymax = 3), fill = 'blue', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 8, ymin = 3, ymax = 5), fill = 'darkseagreen', alpha = 0.4) +
        geom_rect(aes(xmin = 8, xmax = 10, ymin = 0, ymax = 3), fill = 'darkgrey', alpha = 0.4) +
        geom_rect(aes(xmin = 8, xmax = 10, ymin = 3, ymax = 5), fill = 'azure3', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 8, ymin = 5, ymax = 7.5), fill = 'purple', alpha = 0.4) +
        geom_rect(aes(xmin = 4, xmax = 8, ymin = 7.5, ymax = 10), fill = 'yellow', alpha = 0.4) +
        geom_rect(aes(xmin = 8, xmax = 10, ymin = 5, ymax = 7.5), fill = 'darkslategray4', alpha = 0.4) +
        geom_rect(aes(xmin = 8, xmax = 10, ymin = 7.5, ymax = 10), fill = 'deeppink3', alpha = 0.4) +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
        
    
    p1 + p2
}
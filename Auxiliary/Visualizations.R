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

### Visualization of MSE Increase% Importance Measure reported by
### randomForest() for Simulation 1

CART_rf_INC_MSE_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/INC_MSE_CART_rf.RDS"))) %>% 
        gather(value = "MSE_INC", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("MSE INC% for different Predictors") +
        geom_violin(aes(x = Predictor, y = MSE_INC, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.025, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

CART_rf_INC_NP_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/INC_NP_CART_rf.RDS"))) %>% 
        gather(value = "NP_INC", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Node Purity INC% for different Predictors") +
        geom_violin(aes(x = Predictor, y = NP_INC, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 2.5, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

CIT_rf_UNCOND_IMP_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/UNCOND_IMP_CIT_rf.RDS"))) %>% 
        gather(value = "Unconditional Importance", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Unconditional Importance generated by cforest() for different Predictors") +
        geom_violin(aes(x = Predictor, y = `Unconditional Importance`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.025, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

Altm_pval_INC_MSE_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/Altm_INC_MSE_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Altmann et al. (2010) p-value for MSE_INC%") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

Altm_pval_INC_NP_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/Altm_INC_NP_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Altmann et al. (2010) p-value for NP_INC%") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

Altm_pval_UNCOND_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/Altm_UNCOND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Altmann et al. (2010) p-value for Unconditional Variable Importance") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

HU_pval_INC_MSE_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/HU_INC_MSE_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for MSE_INC%") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

HU_pval_INC_NP_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/HU_INC_NP_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for NP_INC%") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

HU_pval_UNCOND_vis <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_1/Summary/HU_UNCOND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for Unconditional Variable Importance") +
        geom_violin(aes(x = Predictor, y = `p-value`, fill = Predictor),
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x = 1.5, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

Comparison_pval_vis <- function(){
    set.seed(42)
    data <- data.frame(variable = runif(1000))
    
    p <- ggplot(data = data) +
        ggtitle("Comparison for uniform distribution") +
        geom_violin(aes(x = 1, y = variable), fill = 'grey',
                    draw_quantiles = c(0.25, 0.5, 0.75), trim = TRUE) +
        annotate("text", x =0.7, y = 0.85, label = "Horizontal Lines added for \n 25%, 50% and 75% percentile",
                 size = 6, family="Serif") +
        theme_light() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"))
    
    p
}

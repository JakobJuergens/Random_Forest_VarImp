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

### Simulation 1

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

### Visualization of NP Increase% Importance Measure reported by
### randomForest() for Simulation 1

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

### Visualization of Uncodnitional Importance Measure reported by
### cforest() for Simulation 1

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

### Simulation 2


### Visualization of MSE Increase% Importance Measure reported by
### randomForest() for Simulation 1

CART_rf_INC_MSE_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/INC_MSE_CART_rf.RDS"))) %>% 
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

### Visualization of NP Increase% Importance Measure reported by
### randomForest() for Simulation 1

CART_rf_INC_NP_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/INC_NP_CART_rf.RDS"))) %>% 
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

### Visualization of Uncodnitional Importance Measure reported by
### cforest() for Simulation 1

CIT_rf_UNCOND_IMP_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/UNCOND_IMP_CIT_rf.RDS"))) %>% 
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

CIT_rf_COND_IMP_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/COND_IMP_CIT_rf.RDS"))) %>% 
        gather(value = "Unconditional Importance", 
               key = "Predictor")
    
    p <- ggplot(data = data) +
        ggtitle("Conditional Importance generated by cforest() for different Predictors") +
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

Altm_pval_INC_MSE_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/Altm_INC_MSE_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Altmann et al. (2010) p-value for MSE_INC% - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Altmann et al. (2010) p-value for MSE_INC% - uninformative predictors") +
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
    
    p1 + p2

}

Altm_pval_INC_NP_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/Altm_INC_NP_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Altmann et al. (2010) p-value for NP_INC% - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Altmann et al. (2010) p-value for NP_INC% - uninformative predictors") +
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
    
    p1 + p2
}

Altm_pval_UNCOND_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/Altm_UNCOND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Altmann et al. (2010) p-value for Unconditional Variable Importance - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Altmann et al. (2010) p-value for Unconditional Variable Importance - uninformative predictors") +
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
    
    p1 + p2
}

Altm_pval_COND_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/Altm_COND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Altmann et al. (2010) p-value for Conditional Variable Importance - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Altmann et al. (2010) p-value for Conditional Variable Importance - uninformative predictors") +
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
    
    p1 + p2
    
}

HU_pval_INC_MSE_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/HU_INC_MSE_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for MSE_INC% - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for MSE_INC% - uninformative predictors") +
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
    
    p1 + p2
    
}

HU_pval_INC_NP_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/HU_INC_NP_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for NP_INC% - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for NP_INC% - uninformative predictors") +
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
    
    p1 + p2

}

HU_pval_UNCOND_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/HU_UNCOND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for Unconditional Variable Importance - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for Unconditional Variable Importance - uninformative predictors") +
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
    
    p1 + p2
    
}

HU_pval_COND_vis_2 <- function(){
    data <- as.data.frame(t(readRDS(file = "Data/Simulation_2/Summary/HU_COND_pval.RDS"))) %>% 
        gather(value = "p-value", 
               key = "Predictor")
    
    informative <- data %>% 
        filter(Predictor %in% c("X_1", "X_3", "X_5", "X_7"))
    
    uninformative <- data %>% 
        filter(Predictor %in% c("X_2", "X_4", "X_6", "X_8"))
    
    p1 <- ggplot(data = informative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for Conditional Variable Importance - informative predictors") +
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
    
    p2 <- ggplot(data = uninformative) +
        ggtitle("Hapfelmeier and Ulm (2013) p-value for Conditional Variable Importance - uninformative predictors") +
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
    
    p1 + p2

}

### Application Visualizations

appl_CART_rf_VARIMP_vis <- function(){
    data <- cbind(c('chol', 'stab.glu', 'hdl', 'ratio', 'location', 'age',
                    'gender', 'height', 'weight', 'frame', 'bp.1s', 'bp.1d', 
                    'waist', 'hip', 'time.ppn'),
                  readRDS("Data/Application/importances.RDS")[, 1:2]) %>% 
        gather(key = 'Method', value = 'Value', -1)
    
    names(data)[1] <- 'Predictor'
    
    p <- ggplot(data = data) +
        ggtitle("CART RF - Variable Importances") +
        geom_point(aes(x = Predictor, y = Value)) +
        theme_light() +
        coord_flip() +
        facet_grid(cols = vars(Method), scales = "free") +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"),
              strip.text.x = element_text(size=12, family="Serif"))
    
    p
}

appl_CIT_rf_VARIMP_vis <- function(){
    data <- cbind(c('chol', 'stab.glu', 'hdl', 'ratio', 'location', 'age',
                    'gender', 'height', 'weight', 'frame', 'bp.1s', 'bp.1d', 
                    'waist', 'hip', 'time.ppn'),
                  readRDS("Data/Application/importances.RDS")[, 3:4]) %>% 
        gather(key = 'Method', value = 'Value', -1)
    
    names(data)[1] <- 'Predictor'
    
    p <- ggplot(data = data) +
        ggtitle("CIT RF - Variable Importances") +
        geom_point(aes(x = Predictor, y = Value)) +
        theme_light() +
        coord_flip() +
        facet_grid(cols = vars(Method), scales = "free") +
        theme(legend.position = "none") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"),
              strip.text.x = element_text(size=12, family="Serif"))
    
    p
}

appl_Altm_vis <- function(){
    data <- cbind(c('chol', 'stab.glu', 'hdl', 'ratio', 'location', 'age',
                    'gender', 'height', 'weight', 'frame', 'bp.1s', 'bp.1d', 
                     'waist', 'hip', 'time.ppn'),
        readRDS("Data/Application/Altm_p_values.RDS")) %>% 
        gather(key = 'Method', value = 'p-value', -1)
    names(data)[1] <- 'Predictor'
    
    p <- ggplot(data = data) +
        ggtitle("p-values generated by Altmann et al. (2010) method") +
        geom_point(aes(x = Predictor, y = `p-value`, col = Method, shape = Method), 
                   size = 3, position = position_jitter(w = 0.35, h = 0)) +
        geom_hline(yintercept = 0.05, col = "red", alpha = 0.5) +
        coord_flip() +
        theme_light() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"),
              legend.title = element_text(size=12, family="Serif"),
              legend.text = element_text(size=12, family="Serif"))
    
    p
}

appl_HU_vis <- function(){
    data <- cbind(c('chol', 'stab.glu', 'hdl', 'ratio', 'location', 'age',
                    'gender', 'height', 'weight', 'frame', 'bp.1s', 'bp.1d', 
                    'waist', 'hip', 'time.ppn'),
                  readRDS("Data/Application/HU_p_values.RDS")) %>% 
        gather(key = 'Method', value = 'p-value', -1)
    names(data)[1] <- 'Predictor'
    
    p <- ggplot(data = data) +
        ggtitle("p-values generated by Hapfelmeier and Ulm (2013) method") +
        geom_point(aes(x = Predictor, y = `p-value`, col = Method, shape = Method), 
                   size = 3, position = position_jitter(w = 0.35, h = 0)) +
        geom_hline(yintercept = 0.05, col = "red", alpha = 0.5) +
        coord_flip() +
        theme_light() +
        theme(legend.position = "bottom") +
        theme(plot.title = element_text(size=20, family="Serif"),
              axis.title.x = element_text(size=16, family="Serif"),
              axis.title.y = element_text(size=16, family="Serif"),
              axis.text.x = element_text(size=12, family="Serif"),
              axis.text.y = element_text(size=12, family="Serif"),
              legend.title = element_text(size=12, family="Serif"),
              legend.text = element_text(size=12, family="Serif"))
    
    p
}

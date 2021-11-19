library(plyr)
library(dplyr)
library(tidyverse)
library(patchwork)
library(ggplot2)
library(ggnewscale)

# Final Missing Function
plot_missing <- function(data_name, percent=TRUE){
  tmp <- data.frame(is.na(data_name)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup() %>%
    select(-count)
  
  # sort variable names
  name_order <- names(sort(colSums(tmp),decreasing = TRUE))
  
  # find the middle variable name
  mid_var_idx <- round(length(name_order)/2)
  
  # data for the main plot
  tidymissing <- data.frame(is.na(data_name)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup() %>%
    rownames_to_column("id") %>%
    select(-count) %>%
    gather(key, value, -id) %>%
    mutate(id = factor(id, levels = min(as.numeric(id)):max(as.numeric(id)))) %>%
    mutate(missing = ifelse(value=="TRUE", "Yes", "No")) %>%
    mutate(key = factor(key, levels = c(name_order)))
  
  # data for filter the complete cases
  tmp_filter <- data.frame(is.na(data_name)) %>%
    group_by_all() %>%
    count(name = "cnt", sort = TRUE) %>%
    ungroup() %>%
    select(-cnt)%>% 
    mutate_if(is.logical, as.numeric) %>%
    mutate(rowsum = rowSums(.))%>%
    rownames_to_column("id")
  
  # show which row is the completed case
  c_c <- tmp_filter$id[tmp_filter$rowsum==0] %>% as.numeric(.)
  
  # adjust alpha
  n_var <- nrow(tmp_filter)
  
  alpha_list <- rep(0.6, n_var)
  
  for (i in c_c){
    alpha_list[i]<-1
  }
  
  c_c_fct <- as.factor(c_c)
  
  # the main plot
  p1 <- ggplot(tidymissing) + 
    geom_tile(aes(x = key, y = fct_rev(id), fill=missing, alpha = id), color = "white") + 
    scale_fill_manual(values=c("Yes"="mediumpurple3", "No"="grey")) +
    scale_alpha_manual(values = alpha_list, guide="none") +
    annotate(geom="text", x=mid_var_idx, y=c_c_fct, label="complete cases") +
    theme(legend.position="none", panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("variable") +
    ylab("missing pattern")
  
  # data for the right side plot
  tmp2 <- data.frame(is.na(data_name)) %>%
    group_by_all() %>%
    count(name = "cnt", sort = TRUE) %>%
    ungroup() %>%
    rownames_to_column("id") %>%
    mutate(id = factor(id, levels = min(as.numeric(id)):max(as.numeric(id))))
  
  tmp2$percent <- tmp2$cnt/sum(tmp2$cnt)*100
  
  # right side bar plot
  if (percent==TRUE){
    p2 <- ggplot(tmp2)+
      geom_col(aes(fct_rev(id), percent, alpha = id), fill = "cornflowerblue") +
      scale_alpha_manual(values = alpha_list, guide="none") +
      coord_flip() + theme_bw() + 
      ylab("% rows") +
      ylim(0, 100) +
      theme(legend.position="none",
            panel.grid.major.y = element_blank(),
            axis.title.y=element_blank())
  }else{
    p2 <- ggplot(tmp2)+
      geom_col(aes(fct_rev(id), cnt, alpha = id), fill = "cornflowerblue") +
      scale_alpha_manual(values = alpha_list, guide="none") +
      coord_flip() + theme_bw() + 
      ylab("row count") +
      theme(legend.position="none",
            panel.grid.major.y = element_blank(),
            axis.title.y=element_blank())
  }
  
  # data for the top plot
  tmp3 <- data.frame(is.na(data_name))
  
  tmp4 <- data.frame(colSums(tmp3)) %>%
    rownames_to_column("id") %>%
    mutate(id = factor(id, levels = c(name_order)))
  
  tmp4$percent <- (tmp4$colSums.tmp3./nrow(data_name))*100
  
  # top bar plot
  if (percent==TRUE){
    p3 <- ggplot(tmp4, aes(factor(id), percent)) +
      geom_col(fill = "cornflowerblue", alpha = 0.6) + theme_bw() +
      ylab("% rows\n missing:") + 
      ylim(0, 100) +
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x=element_blank())
  }else{
    p3 <- ggplot(tmp4, aes(factor(id), colSums.tmp3.)) +
      geom_col(fill = "cornflowerblue", alpha = 0.6) + theme_bw() +
      ylab("num rows\n missing:") + 
      theme(axis.title.x=element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.major.x=element_blank())
  }
  
  # blank plot
  p4 = ggplot()+ theme_minimal()
  
  # Final Combined Plots
  if (percent==TRUE){
    p3 + p4 + p1 + p2 + plot_layout(widths = c(4,1), heights = c(1,3)) +
      plot_annotation(title = "Missing Value Patterns (Percent)")
  }else{
    p3 + p4 + p1 + p2 + plot_layout(widths = c(4,1), heights = c(1,3)) +
      plot_annotation(title = "Missing Value Patterns (Counts)")
  }
  
}
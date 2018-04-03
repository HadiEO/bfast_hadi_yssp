plotSeasonality <- function(df, legend.pos, showX, yLab) {
  # 2000-2004
  df_00_04 <- df %>% dplyr::filter(year >= 2000, year <= 2004) %>% 
    group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%    # mean or median? ***
    ungroup() %>% 
    mutate(period = "2000-2004")
  
  # 2005-2009
  df_05_09 <- df %>% dplyr::filter(year >= 2005, year <= 2009) %>% 
    group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%    # mean or median? ***
    ungroup() %>% 
    mutate(period = "2005-2009")
  
  # 2010-2014
  df_10_14 <- df %>% dplyr::filter(year >= 2010, year <= 2014) %>% 
    group_by(doy) %>% summarise(median_value = median(value, na.rm = TRUE)) %>%    # mean or median? ***
    ungroup() %>% 
    mutate(period = "2010-2014")
  
  # Merge the three periods
  df_3periods <- bind_rows(df_00_04, df_05_09, df_10_14)
  
  # Make dummy date for plotting purpose
  df_3periods <- df_3periods %>% 
    mutate(dummy_date = as.Date(doy, origin = "2014-12-31"))
  
  # To plot Apr to Mar, add 365 days to Jan-Mar
df_3periods[which(df_3periods$doy <= 90), "dummy_date"] <-
  df_3periods[which(df_3periods$doy <= 90), "dummy_date"] + 365
  
  
  # Plot
  plot_df_3periods <- ggplot(df_3periods, aes(x = dummy_date, y = median_value, col = period, shape = period)) +
    geom_point(na.rm = TRUE, size = 1) +
    theme_bw() + labs(y = yLab, x = "") + scale_y_continuous(limits = c(-0.2, 0.6)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b", date_minor_breaks = "1 month",
                 limits = c(as.Date("2015-03-21"), as.Date("2016-04-10")),
                 expand = c(0,0)) +
    scale_colour_manual(name = "Period", values = c("magenta", "chartreuse4", "dodgerblue"),
                        labels = c("2000-2004   ", 
                                   "2005-2009   ",
                                   "2010-2014   ")) +
    scale_shape_manual(name = "Period", values = c(1, 7, 2),
                       labels = c("2000-2004   ", 
                                  "2005-2009   ",
                                  "2010-2014   "))
  
  if(showX) {
    plot_df_3periods <- plot_df_3periods + 
      theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), 
          axis.text = element_text(size = 8), axis.title.x = element_text(size = 10,face = "bold"), 
          axis.title.y = element_text(size = 10,face="bold"), 
          legend.text = element_text(size = 8), legend.title = element_text(size = 7), 
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.position = legend.pos) +
      guides(col = guide_legend(nrow = 1, override.aes = list(size=1.5)), title = NULL, title.position = NULL)
  } else {
    plot_df_3periods <- plot_df_3periods + 
      theme(axis.line = element_line(colour = "black"), panel.background = element_blank(), 
            axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 10, face = "bold"), 
            legend.text = element_text(size = 8), legend.title = element_text(size = 7), 
            legend.background = element_rect(fill = "transparent"),
            legend.key = element_rect(fill = NA, colour = NA),
            legend.position = legend.pos) +
      guides(col = guide_legend(nrow = 1, override.aes = list(size=1.5)), title = NULL, title.position = NULL)
  }
  
  return(plot_df_3periods)
}

iga_dz_pyramid = function(x){

  iga_dz %>%
    ggplot(aes(x = age_gp, y = ifelse(gender == 2, -n, n), fill = gender)) +
    geom_bar(stat = "identity", width = 0.8) +
    scale_fill_manual(values = c("#4e79a7", "#f28e2c"), labels = c("Male", "Female")) +
    labs(title = "Population Pyramid", x = "Age Group", y = "Population (thousands)", fill = "Gender") +
    coord_flip() +
    theme_minimal() +
    scale_y_continuous(labels = function(x) {
      ifelse(x < 0, paste0("", scales::comma_format()(abs(x) / 1000), "K"), paste0(scales::comma_format()(x / 1000), "K"))
    }, expand = c(0, 0)) +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(family = "Times New Roman", size = 15, face = "bold"),
      axis.title = element_text(family = "Times New Roman", size = 13, face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(), 
      legend.position = "bottom",
      legend.justification = "center"
    ) +
    facet_wrap(new_code ~., nrow=8)
}

#########################################################################

create_bar_chart_categorical <- function(
    dataset, 
    x_col, 
    y_col, 
    chart_title = "Bar Chart", 
    sub_title = "",
    y_axis_labels = scales::comma,
    add_maximum = FALSE, 
    add_minimum = FALSE,
    add_mean = FALSE, 
    add_median = FALSE,
    add_sd = FALSE, 
    add_second_maximum = FALSE,
    add_trendline = FALSE, 
    extra_line = NULL,
    horizontal_adjustment_max = 0.5,
    vertical_adjustment_max = -1,
    console_print_out_title = "Data Summary",
    chart_file_name = NULL) 
{
  
  # Call base_bar_chart without x_scale
  base_bar_chart(
    dataset, 
    x_col, 
    y_col, 
    chart_title, 
    sub_title,
    y_axis_labels, 
    add_maximum, 
    add_minimum, 
    add_mean,
    add_median, 
    add_sd, 
    add_second_maximum, 
    add_trendline,
    extra_line, 
    horizontal_adjustment_max, 
    vertical_adjustment_max,
    console_print_out_title, 
    chart_file_name
  )
}

#########################################################################

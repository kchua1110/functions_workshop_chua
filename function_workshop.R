## Function Workshop
## Nov 14, 2023
## Kristine Joy Chua

## Repeating yourself is bad b/c (1) waste of time, (2) open to risky errors, (3) asthetics, minor point


# Create vector airtemp
airtemp <- c(212, 20.3, 78, 32)

# Manual way to convert F --> C
celsius1 <- (airtemp[1] - 32) * 5/9

# Write function
#' Converting temperature values from Fahrenheit to Celsius
#'
#' @param fahr Numeric or numeric vector in degrees Fahrenheit
#'
#' @return Numeric or numeric vector in degrees Celsius
#' @export
#'
#' @examples
#' fahr_to_celsius(32)
#' fahr_to_celsius(c(32, 212,72)
#' 
fahr_to_celsius <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  
  return (celsius)
}

# Instead of writing the manual conversion, can use function
celsius4 <- fahr_to_celsius(airtemp[1])

# Can use function to get the entire vector with the converted temp
celsius <- fahr_to_celsius(airtemp)

# Exercise: Celsius to Farenheit
celsius_to_fahr <- function (celsius) {
  fahr <- (celsius * 9/5) + 32
  
  return (fahr)
}

fahr <- celsius_to_fahr(celsius)
airtemp == fahr

## Create a new function to convert temperatures to celsius and kelvin

convert_temps <- function(fahr) {
  celsius <- (fahr - 32) * 5/9
  kelvin <- celsius + 273.15
  return (list(fahr = fahr, celsius = celsius, kelvin = kelvin))
}

## Creating dataframe using function

temps_df <- data.frame(convert_temps(seq(-100, 100, 10))) # the data used is a list using values -100 to 100 in increments of 10

temp_df_test <- data.frame(convert_temps(fahr))


## Creating function with custom ggplot theme
custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    text             = ggplot2::element_text(family = 'Helvetica', 
                                             color = 'gray30', 
                                             size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), 
                                             hjust = 0.5, 
                                             face = 'bold'),
    panel.background = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', 
                                             linewidth = 0.25),
    legend.position  = 'right',
    legend.key       = ggplot2::element_rect(colour = NA, 
                                             fill = NA),
    axis.ticks       = ggplot2::element_blank(),
    axis.line        = ggplot2::element_blank()
  )
}

library(ggplot2)

ggplot(temps_df, mapping = aes(x = fahr, y = celsius, color = kelvin)) +
  geom_point() +
  custom_theme(10) # can overwrite the base size number, changes the font

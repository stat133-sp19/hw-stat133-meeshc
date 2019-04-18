library(shiny)
library(reshape)
library(ggplot2)

# functions for savings modalities
#' @title Future Value
#' @description Computes the future value of an investment
#' @param amount initial invested amount
#' @param rate annual rate of return
#' @param years number of years
#' @return numeric value of investment in dollars
future_value <- function(amount, rate, years) {
  return(amount * (1 + rate)^years)
}
#' @title Annuity
#' @description Computes the future value of annuity after depositing a certain amount of money at the end of each year
#' @param contrib contribution, the amount deposited at the end of each year
#' @param rate annual rate of return
#' @param years number of years
annuity <- function(contrib, rate, years) {
  return(contrib * (((1 + rate)^years) - 1) / rate)
}
#' @title Growing Annuity
#' @description Computes the future value of growing annuity, money deposited each year that increases by a certain amount
#' @param contrib contributed amount
#' @rate annual rate of return
#' @growth annual growth rate
#' @years number of years
growing_annuity <- function(contrib, rate, growth, years) {
  return(contrib * ((((1 + rate)^years) - (1 + growth)^years) / (rate - growth)))
}

# Generate graph for saving-investing modalities over the course of ten years
generate_modalities <- function(year, amount, contrib, rate, growth) {
  years <- 0:year
  no_contrib <- 1:(year + 1)
  fixed_contrib <- 1:(year + 1)
  growing_contrib <- 1:(year + 1)
  rate <- rate / 100
  growth <- growth / 100
  for (i in years) {
    no_contrib[i + 1] <- future_value(amount, rate, i)
    fixed_contrib[i + 1] <- annuity(contrib, rate, i) + no_contrib[i + 1]
    growing_contrib[i + 1] <- growing_annuity(contrib, rate, growth, i) + no_contrib[i + 1]
  }
  modalities <- data.frame(years, no_contrib, fixed_contrib, growing_contrib)
  return(modalities)
}

function(input, output) {
  
  output$tbl <- renderPrint({
    modalities <- generate_modalities(input$years, input$initial, input$contrib, input$rate, input$growth)
    modalities 
  })
  
  colors <- c('#EC2424', '#1ED62F', '#42B1F6')
  
  legend_vals <- c('no_contrib', 'fixed_contrib', 'growing_contrib')
  
  output$plot <- renderPlot({
    modalities <- generate_modalities(input$years, input$initial, input$contrib, input$rate, input$growth)
    if (input$facet == FALSE) {
      years <- 0:input$years
      ggplot(data = modalities) +
        geom_line(aes(x = years, y = no_contrib, color = colors[2])) +
        geom_line(aes(x = years, y = fixed_contrib, color = colors[3])) + 
        geom_line(aes(x = years, y = growing_contrib, color = colors[1])) + labs(x = "year", y = "value", title = "Three Modes of Investing") +
        scale_colour_discrete(name = "variable", labels = c("no_contrib", "fixed_contrib", "growing_contrib"))
    } else {
      melted <- melt(modalities, id = c("years"))
      ggplot(data = melted) + geom_line(aes(x = years, y = value)) +
        geom_area(aes(x = years, y = value)) +
        facet_wrap(~variable) + aes(fill = variable, color = variableins) + xlab('year') + ylab('value') + ylim(0, 100000)
    }
  })
}
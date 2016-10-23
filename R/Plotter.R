#' Function to generate an interactive data plotter
#' @import shiny
#' @param data a data frame to be plotted
#' @return launches a shiny app that can help quickly visualize data. Has an interactive x and y components, and can toggle between scatter and box plots
#' @export
#'

plotter <- function(data) {
	shinyApp(
		ui = fluidPage(
			# Application title
			titlePanel("Data Viz"),
			fluidRow(column(4, uiOutput('x_axis')),
			column(4, uiOutput('y_axis')),
            column(4, checkboxGroupInput(inputId = 'plot_type', label = 'Plot Type', choices = c('Scatter', 'Boxplot'), selected = 'Scatter', inline = T))
			),
			hr(),
			# Plot Plot
			plotOutput("Plot1")
			),
		server = function(input, output) {
			output$x_axis <- renderUI({
				selectInput("x_axis", "Select x-axis variable", names(data))
			})
			output$y_axis <- renderUI({
				selectInput("y_axis", "Select y-axis variable", names(data))
			})

			plot_item <- reactive({
				validate(
					need(input$x_axis,  'Select an x-axis variable.'),
					need(input$y_axis, 'Select a y-axis  variable.')
					)
					plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = input$x_axis, y = input$y_axis))
					return(
						plot)
				})
			plot_item_final <- reactive({
				plot  <- plot_item()
				if ('Boxplot' %in% input$plot_type) {
					plot <- plot +  ggplot2::geom_boxplot()
				}
				if ('Scatter' %in% input$plot_type) {
					plot <- plot + ggplot2::geom_point(size = 4)
				}
				plot <- plot + EJC_theme() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

				return(plot)
			})
			output$Plot1 <- renderPlot({plot_item_final()})
		}
	)
}

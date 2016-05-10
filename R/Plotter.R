#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

plotter <- function(data) {
	require(shiny)
	require(ggplot2)
	require(cowplot)
	require(DT)
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
				selectInput("x_axis", "Select x-axis variable", names(data()))
			})
			output$y_axis <- renderUI({
				selectInput("y_axis", "Select y-axis variable", names(data()))
			})
			
			plot_item <- reactive({
				validate( 
					need(input$x_axis,  'Select an x-axis variable.'), 
					need(input$y_axis, 'Select a y-axis  variable.')
					) 
					plot <- ggplot(data(),aes_string(x = input$x_axis, y = input$y_axis))
					return(
						plot) 
				}) 
			plot_item_final <- reactive({
				plot  <- plot_item() 
				if ('Boxplot' %in% input$plot_type) {
					plot <- plot +  geom_boxplot()
				} 
				if ('Scatter' %in% input$plot_type) {
					plot <- plot + geom_point(size = 4) 
				} 
				plot <- plot + EJC_theme() + theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) 
				
				return(plot) 
			}) 
			output$Plot1 <- renderPlot({plot_item_final()})
		}
	)
}

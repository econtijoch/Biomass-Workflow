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
	        fluidRow(
	          column(
	            3,
	            uiOutput('plot_color_by')
	          ),
	          column(3,
	                 uiOutput('plot_x'),
	                 uiOutput('plot_y')),
	          column(3,
	                 uiOutput('plot_facet_row'),
	                 uiOutput('plot_facet_col')),
	          column(
	            3,
	            sliderInput(
	              "point_size",
	              "Point size:",
	              min = 2,
	              max = 10,
	              value = 6,
	              step = 1
	            ),
	            checkboxGroupInput(
	              inputId = 'plot_type',
	              label = 'Plot Type',
	              choices = c('Scatter', 'Boxplot', "Mean + SEM"),
	              selected = 'Scatter',
	              inline = T
	            )
	          )
	        ),
			
			hr(),
			# Plot Plot
			plotOutput("Plot1")
			),
			
		server = function(input, output) {
			
	        output$plot_x <- renderUI({
	          selectInput('plot_x', 'X', names(data))
	        })
	        output$plot_y <- renderUI({
	          selectInput(
	            'plot_y',
	            'Y',
	            names(data),
	            names(data)[[2]]
	          )
	        })
	        output$plot_color_by <- renderUI({
	          selectInput('plot_color_by',
	                      'Color By',
	                      c('None', names(data))
	                      )
	        })
	        output$plot_facet_row <- renderUI({
	          selectInput('facet_row', 'Facet Row', c(None = '.', names(data)))
	        })
	        output$plot_facet_col <- renderUI({
	          selectInput('facet_col', 'Facet Column', c(None = '.', names(data)))
	        })

	        plot_inputs <- reactive({
	          if (!is.null(input$plot_x) && !is.null(input$plot_y)) {
	            output <-
	              paste(
	                input$plot_x,
	                input$plot_y,
	                input$plot_color_by,
	                input$facet_row,
	                input$facet_col,
	                input$point_size,
	                input$plot_type
	              )
	          }
	          else {
	            output <- NULL
	          }
	          return(output)
	        })
			
			
	        plot_item_final <- eventReactive(plot_inputs(), {
	          xval <- input$plot_x
	          yval <- input$plot_y
	          plot <-
	            ggplot2::ggplot(data = data, ggplot2::aes_string(x = xval, y = yval))
	          if ('Mean + SEM' %in% input$plot_type ) {
	            plot <- plot + BiomassWorkflow::geom_mean_sem()
	          } else if ('Boxplot' %in% input$plot_type) {
	            plot <- plot + ggplot2::geom_boxplot()
	          } else if ('Scatter' %in% input$plot_type) {
	            plot <- plot + ggplot2::geom_point(size = input$point_size)
	          }

	          p <- plot +
	            ggplot2::theme_classic() +
	            ggplot2::theme(
	              axis.text = ggplot2::element_text(size = 14, face = "bold"),
	              axis.title = ggplot2::element_text(size = 16, face = "bold"),
	              axis.line.x = ggplot2::element_line(size = 1, color = 'black'),
	              axis.line.y = ggplot2::element_line(size = 1, color = 'black'),
	              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
	              axis.ticks = ggplot2::element_line(size = 1.5)
	            )


	          if (!is.null(input$plot_color_by)) {
	            if (input$plot_color_by != 'None') {
	              p <-
	                p + ggplot2::aes_string(color = input$plot_color_by) + ggplot2::scale_color_manual(values = BiomassWorkflow::EJC_colors)
	            }
	          }

	          if (!is.null(input$facet_row) & !is.null(input$facet_col)) {
	            facets <- paste(input$facet_row, '~', input$facet_col, sep = " ")
	            if (facets != '. ~ .') {
	              p <- p + ggplot2::facet_grid(facets)
	            }
	          }
	          return(p)
	        })
			
			output$Plot1 <- renderPlot({plot_item_final()})
		}
	)
}

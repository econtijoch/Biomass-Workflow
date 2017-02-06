#' Shiny App that aids in vizualizing sequencing objects returned from individual_scale
#' @import shiny
#' @importFrom dplyr n
#' @param sequencing_object output object of individual_scale
#' @return launches shiny app that helps interactively visualize data
#' @export
#'

sequencing_data_viz <- function(sequencing_object) {
  shinyApp(# Define UI for application that draws a histogram
    ui <- navbarPage(title = "Sequencing Data Visualization",
                     # Application title
                     tabPanel(title = 'Ordination',
                              fluidRow(
                                column(
                                  3,
                                  selectInput(
                                    'distance_method',
                                    label = 'Select Distance Method',
                                    choices = c(
                                      'Euclidean' = "euclidean",
                                      'Jaccard' = "jaccard",
                                      'Bray-Curtis' = "bray",
                                      'Chao' = "chao",
                                      'Canberra' = "canberra",
                                      'Manhattan' = "manhattan",
                                      'Gower' = "gower",
                                      'AltGower' = "altGower",
                                      'Kulczynski' = "kulczynski",
                                      'Morisita' = "morisita",
                                      'Horn' = "horn",
                                      'Binomial' = "binomial",
                                      'Cao' = "cao",
                                      'Mountford' = "mountford",
                                      'Raup' = "raup"
                                    ),
                                    selected = "euclidean"
                                  ),
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
                              # Sidebar with a slider input for number of bins
                              hr(),
                              fluidRow(column(3, offset = 3, h4('Relative')),
                                       column(3, offset = 3, h4('Absolute'))),
                              fluidRow(column(6,
                                              plotOutput(
                                                "data_plot_relative"
                                              )),
                                       column(6,
                                              plotOutput(
                                                'data_plot_absolute'
                                              ))),
                              fluidRow(column(6,
                                              downloadButton('download_relative_viz_plot', label = 'Download Relative Abundance Plot')),
                                       column(6,
                                              downloadButton('download_absolute_viz_plot', label = 'Download Absolute Abundance Plot'))),
                              fluidRow(column(6,
                                              plotOutput(
                                                "spree_plot_relative"
                                              )),
                                       column(6,
                                              plotOutput(
                                                'spree_plot_absolute'
                                              ))),
                              
                              hr(),
                              fluidRow(
                                column(
                                  4,
                                  offset = 1,
                                  h4("Percent Explained: Relative"),
                                  tableOutput("pct_explained_table_relative")
                                ),
                                column(
                                  4,
                                  offset = 2,
                                  h4("Percent Explained: Absolute"),
                                  tableOutput("pct_explained_table_absolute")
                                )
                              )
                     ),
                     tabPanel(title = 'Abundance',
                              fluidRow(column(3, offset = 1,
                                              selectInput(inputId = 'taxonomic_depth', label = 'Select Taxonomic Depth', choices = c("Phylum", "Class", "Order", "Family", "Genus", "Species")),
                                              uiOutput('select_grouping_x_variable')),
                                       column(3,
                                              uiOutput('plot_facet_row_abundance'),
                                              uiOutput('plot_facet_col_abundance')),
                                       column(3,
                                              helpText('Download plots as R objects'),
                                              downloadButton('download_relative_bar_plot', label = 'Relative Abundance'),
                                              hr(),
                                              downloadButton('download_absolute_bar_plot', label = 'Absolute Abundance'))),
                              hr(),
                              column(12, plotOutput('relative_abundance_plot', hover = 'relative_plot_hover')),
                              column(12, plotOutput('absolute_abundance_plot', hover = 'absolute_plot_hover'))
                              
                              
                     )
    ),
    
    
    
    # Define server logic required to draw a histogram
    server <- function(input, output, session) {
      metadata <- sequencing_object[['sample_metadata']]
      
      ordination_results <- reactive({
        output_table <-
          BiomassWorkflow::ordinate_sequencing(sequencing_object = sequencing_object,
                                               method = input$distance_method)
        return(output_table)
      })
      
      data_viz_table_relative <- reactive({
        output_table <- ordination_results()[['relative_plotting_data']]
        return(output_table)
      })
      
      
      
      data_viz_table_absolute <- reactive({
        output_table <- ordination_results()[['absolute_plotting_data']]
        return(output_table)
      })
      
      
      
      percent_explained_table_relative <- reactive({
        eig <- ordination_results()[['relative_ordination_output']]$eig
        PC <- paste("PC", 1:10, sep = "")
        PercentExplained <- BiomassWorkflow::percent(eig / sum(eig))[1:10]
        df <- data.frame(PC, PercentExplained)
        return(df)
      })
      
      output$pct_explained_table_relative <- renderTable({
        percent_explained_table_relative()
      })
      
      percent_explained_table_absolute <- reactive({
        eig <- ordination_results()[['absolute_ordination_output']]$eig
        PC <- paste("PC", 1:10, sep = "")
        PercentExplained <- BiomassWorkflow::percent(eig / sum(eig))[1:10]
        df <- data.frame(PC, PercentExplained)
        return(df)
      })
      output$pct_explained_table_absolute <- renderTable({
        percent_explained_table_absolute()
      })
      
      output$plot_x <- renderUI({
        selectInput('plot_x', 'X', names(data_viz_table_relative()))
      })
      output$plot_y <- renderUI({
        selectInput(
          'plot_y',
          'Y',
          names(data_viz_table_relative()),
          names(data_viz_table_relative())[[2]]
        )
      })
      output$plot_color_by <- renderUI({
        selectInput('plot_color_by',
                    'Color By',
                    c('None', names(metadata)),
                    selected = input$comparison)
      })
      output$plot_facet_row <- renderUI({
        selectInput('facet_row', 'Facet Row', c(None = '.', names(metadata)))
      })
      output$plot_facet_col <- renderUI({
        selectInput('facet_col', 'Facet Column', c(None = '.', names(metadata)))
      })
      
      plot_inputs <- reactive({
        if (!is.null(input$plot_x) && !is.null(input$plot_y)) {
          output <-
            paste(
              input$distance_method,
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
      
      spree_plot_relative <- eventReactive(plot_inputs() , {
        x_spree <-
          paste("PC", 1:10, sep = " ")
        y_spree <-
          ((
            ordination_results()[['relative_ordination_output']]$eig / sum(ordination_results()[['relative_ordination_output']]$eig)
          ) * 100)[1:10]
        
        spree_data <- data.frame(x_spree, y_spree)
        spree_data$x_spree <-
          factor(spree_data$x_spree, levels = paste("PC", 1:10, sep = " "))
        
        p <-
          ggplot2::ggplot(data = spree_data[1:10, ], ggplot2::aes(x = x_spree, y = y_spree)) + ggplot2::geom_bar(stat = 'identity') + ggplot2::labs(x = "", y = "Percent Explained") + ggplot2::theme_classic() + ggplot2::coord_cartesian(ylim = c(0, 100)) + ggplot2::theme(
            axis.text = ggplot2::element_text(size = 14, face = "bold"),
            axis.title = ggplot2::element_text(size = 16, face = "bold"),
            axis.line.x = ggplot2::element_line(size = 1, color = 'black'),
            axis.line.y = ggplot2::element_line(size = 1, color = 'black'),
            axis.ticks = ggplot2::element_line(size = 1.5)
          ) + ggplot2::scale_y_continuous(expand = c(0, 0))
        return(p)
      })
      output$spree_plot_relative <- renderPlot({
        spree_plot_relative()
      })
      
      spree_plot_absolute <- eventReactive(plot_inputs() , {
        x_spree <-
          paste("PC", 1:10, sep = " ")
        y_spree <-
          ((
            ordination_results()[['absolute_ordination_output']]$eig / sum(ordination_results()[['absolute_ordination_output']]$eig)
          ) * 100)[1:10]
        
        spree_data <- data.frame(x_spree, y_spree)
        spree_data$x_spree <-
          factor(spree_data$x_spree, levels = paste("PC", 1:10, sep = " "))
        
        p <-
          ggplot2::ggplot(data = spree_data[1:10, ], ggplot2::aes(x = x_spree, y = y_spree)) + ggplot2::geom_bar(stat = 'identity') + ggplot2::labs(x = "", y = "Percent Explained") + ggplot2::theme_classic() + ggplot2::coord_cartesian(ylim = c(0, 100)) + ggplot2::theme(
            axis.text = ggplot2::element_text(size = 14, face = "bold"),
            axis.title = ggplot2::element_text(size = 16, face = "bold"),
            axis.line.x = ggplot2::element_line(size = 1, color = 'black'),
            axis.line.y = ggplot2::element_line(size = 1, color = 'black'),
            axis.ticks = ggplot2::element_line(size = 1.5)
          ) + ggplot2::scale_y_continuous(expand = c(0, 0))
        return(p)
      })
      output$spree_plot_absolute <- renderPlot({
        spree_plot_absolute()
      })
      
      data_viz_plot_relative <- eventReactive(plot_inputs(), {
        xval <- input$plot_x
        yval <- input$plot_y
        plot <-
          ggplot2::ggplot(data = data_viz_table_relative(), ggplot2::aes_string(x = xval, y = yval))
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
      output$data_plot_relative <- renderPlot({
        print(data_viz_plot_relative())
      })
      
      
      
      output$download_relative_viz_plot <- downloadHandler(
        filename = function() {'Relative Abundance Data Plot.rds'},
        content = function(file) {
          saveRDS(file = file, object = data_viz_plot_relative())
        }
      )
      
      data_viz_plot_absolute <- eventReactive(plot_inputs(), {
        xval <- input$plot_x
        yval <- input$plot_y
        plot <-
          ggplot2::ggplot(data = data_viz_table_absolute(), ggplot2::aes_string(x = xval, y = yval))
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
      output$data_plot_absolute <- renderPlot({
        data_viz_plot_absolute()
      })
      
      output$download_relative_viz_plot <- downloadHandler(
        filename = function() {'Absolute Abundance Data Plot.rds'},
        content = function(file) {
          saveRDS(file = file, object = data_viz_plot_absolute())
        }
      )
      
      ### Abundance Viz
      ###
      
      output$select_grouping_x_variable <- renderUI(
        selectInput('grouping_x_variable', 'Select sample grouping',names(metadata))
      )
      output$plot_facet_row_abundance <- renderUI({
        selectInput('facet_row_abundance', 'Facet Row', c(None = '.', names(metadata)))
      })
      output$plot_facet_col_abundance <- renderUI({
        selectInput('facet_col_abundance', 'Facet Column', c(None = '.', names(metadata)))
      })
      
      
      abundance_plot_data_relative <- reactive({
        validate(need(!is.null(input$taxonomic_depth) & !is.null(input$grouping_x_variable) & !is.null(input$facet_row_abundance) & !is.null(input$facet_col_abundance), message = 'Select Taxonomic Depth and Grouping Variable'))
        
        data <- dplyr::left_join(sequencing_object$melted_relative_by_taxonomy[[input$taxonomic_depth]], metadata)
        if (input$facet_row_abundance != "." & input$facet_col_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_row_abundance, input$facet_col_abundance)
        } else if (input$facet_col_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_col_abundance)
        } else if (input$facet_row_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_row_abundance)
        } else {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable)
        }
        output_data <- data %>% dplyr::summarize(mean_abundance = mean(abundance)/n()) %>% dplyr::left_join(., metadata)
        return(output_data)
      })
      
      relative_abundance_plot <- reactive({
        xval <- input$grouping_x_variable
        yval <- 'mean_abundance'
        filling <- 'long_label'
        taxa_label_pre <- stringr::str_split(abundance_plot_data_absolute()$long_label, pattern = '__', simplify = T)
        taxa_label <- as.character(taxa_label_pre[,ncol(taxa_label_pre)])
        
        plot <-
          ggplot2::ggplot(data = abundance_plot_data_relative(), ggplot2::aes_string(x = xval, y = yval, fill = filling)) + ggplot2::geom_bar(stat = 'identity') + ggplot2::scale_fill_manual(name = input$taxonomic_depth, breaks = abundance_plot_data_relative()$long_label,  labels = taxa_label, values = BiomassWorkflow::EJC_colors)  + ggplot2::coord_cartesian(expand = FALSE) + ggplot2::labs(y = 'Relative Abundance\n(percentage of mapped 16S reads)', x = NULL)
        
        
        
        if (!is.null(input$facet_row_abundance) & !is.null(input$facet_col_abundance)) {
          facets <- paste(input$facet_row_abundance, '~', input$facet_col_abundance, sep = " ")
          if (facets != '. ~ .') {
            plot <- plot + ggplot2::facet_grid(facets, scales = 'free')
          }
        }
        return(plot)
      })
      output$relative_abundance_plot <- renderPlot({
        relative_abundance_plot() + BiomassWorkflow::EJC_theme_tilted()
      })
      output$download_relative_bar_plot <- downloadHandler(
        filename = function() {'Relative Abundance Bar Plot.rds'},
        content = function(file) {
          saveRDS(file = file, object = relative_abundance_plot())
        }
      )
      
      abundance_plot_data_absolute <- reactive({
        validate(need(!is.null(input$taxonomic_depth) & !is.null(input$grouping_x_variable) & !is.null(input$facet_row_abundance) & !is.null(input$facet_col_abundance), message = 'Select Taxonomic Depth and Grouping Variable'))
        
        data <- dplyr::left_join(sequencing_object$melted_scaled_by_taxonomy[[input$taxonomic_depth]], metadata)
        if (input$facet_row_abundance != "." & input$facet_col_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_row_abundance, input$facet_col_abundance)
        } else if (input$facet_col_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_col_abundance)
        } else if (input$facet_row_abundance != ".") {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable, input$facet_row_abundance)
        } else {
          data <- data %>% dplyr::group_by_("long_label", "short_label", "Abundance_Type", input$taxonomic_depth, input$grouping_x_variable)
        }
        output_data <- data %>% dplyr::summarize(mean_abundance = mean(abundance)/n()) %>% dplyr::left_join(., metadata)
        return(output_data)
      })
      
      
      absolute_abundance_plot <- reactive({
        xval <- input$grouping_x_variable
        yval <- 'mean_abundance'
        filling <- 'long_label'
        taxa_label_pre <- stringr::str_split(abundance_plot_data_absolute()$long_label, pattern = '__', simplify = T)
        taxa_label <- as.character(taxa_label_pre[,ncol(taxa_label_pre)])
        plot <-
          ggplot2::ggplot(data = abundance_plot_data_absolute(), ggplot2::aes_string(x = xval, y = yval, fill = filling)) + ggplot2::geom_bar(stat = 'identity') + ggplot2::scale_fill_manual(name = input$taxonomic_depth, breaks = abundance_plot_data_absolute()$long_label,  labels = taxa_label, values = BiomassWorkflow::EJC_colors)  + ggplot2::coord_cartesian(expand = FALSE) + ggplot2::labs(y = 'Absolute Abundance\n(ug DNA per taxa per mg feces)', x = NULL)
        
        
        
        if (!is.null(input$facet_row_abundance) & !is.null(input$facet_col_abundance)) {
          facets <- paste(input$facet_row_abundance, '~', input$facet_col_abundance, sep = " ")
          if (facets != '. ~ .') {
            plot <- plot + ggplot2::facet_grid(facets, scales = 'free')
          }
        }
        return(plot)
      })
      output$absolute_abundance_plot <- renderPlot({absolute_abundance_plot() + BiomassWorkflow::EJC_theme_tilted()})
      
      output$download_absolute_bar_plot <- downloadHandler(
        filename = function() {'Absolute Abundance Bar Plot.rds'},
        content = function(file) {
          saveRDS(absolute_abundance_plot(), file = file)
        }
      )
      
      session$onSessionEnded(function() { stopApp() })
      
    })
}

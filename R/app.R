DimPals <- function(seurat_object = NULL) {
  library(shiny)
  library(dplyr)
  library(Seurat)
  library(colourpicker)  # For color picker input
  library(paletteer) # Package containing color palettes
  library(ggplot2)
  ui <- fluidPage(
    titlePanel("dim-pals"),
    sidebarLayout(
      sidebarPanel(
        tags$h3("Select Metadata Column to Color By"),
        selectInput("color_by_column", "Color UMAP By", choices = NULL),
        tags$hr(),
        tags$h3("Current Palette"),
        plotOutput("palette_preview", height = "150px"),
        actionButton("shuffle_palette_button", "Shuffle Colors"),
        tags$hr(),
        tags$h3("re-color individual groups"),
        selectInput("cluster_to_color","Select Group for Custom Color", choices = NULL),
        colourInput("col", "Select Group Color", "purple"),
        actionButton("assign_cluster_color_button", "Assign Group Color"),
        tags$hr(),
        tags$h3("Apply Palette to Groups"),
        tags$h4("Select Groups"),
        selectInput("grouping_column", "Select Grouping Column", choices = NULL),
        selectInput("group_value", "Select Group", choices = NULL),
        tags$h4("Palette"),
        selectInput("pal_types", "Select Palette Type", choices = c("Discrete","Continuous","Dynamic")),
        selectInput("pal_package", "Select Palette Package", choices = NULL),
        selectInput("pal_name", "Select Palette Name", choices = NULL),
        actionButton("assign_group_color_button", "Assign Palette"),
        tags$hr(),
        tags$h3("Plot Controls"),
        actionButton("plot_button","Refresh Plot"),
        checkboxInput("show_labels", "Show Labels", value = FALSE),
        checkboxInput("show_legend", "Show Legend", value = TRUE),
        checkboxInput("show_axes", "Show Axes", value = FALSE),
        tags$hr(),
        tags$h3("Export Plot"),
        numericInput("plot_width", "Width (inches)", value = 8, min = 2, max = 20, step = 0.5),
        numericInput("plot_height", "Height (inches)", value = 6, min = 2, max = 20, step = 0.5),
        numericInput("plot_dpi", "DPI", value = 300, min = 72, max = 600, step = 50),
        downloadButton("download_plot", "Download Plot"),
        tags$hr(),
        tags$h3("Save Palette"),
        textInput("palette_var_name", "Variable Name", value = "my_palette"),
        actionButton("save_palette_button", "Save to R Environment")
      ),
      mainPanel(
        plotOutput("umap_plot"),
        verbatimTextOutput("color_list")
      )
    )
  )

  server <- function(input, output, session){
    ##DEFINE REACTIVE VARIABLES
    seurat_object <- reactiveVal(seurat_object)
    palette <- reactiveVal(NULL)
    plotted <- reactiveVal(NULL)

    ##INITIALIZE COLOR BY COLUMN DROPDOWN
    observe({
      seurat <- seurat_object()
      if (!is.null(seurat)) {
        metadata_cols <- colnames(seurat@meta.data)
        updateSelectInput(session, "color_by_column", choices = metadata_cols,
                         selected = if("seurat_clusters" %in% metadata_cols) "seurat_clusters" else metadata_cols[1])
        updateSelectInput(session, "grouping_column", choices = metadata_cols)
      }
    })

    ##INITIALIZE PALETTE WHEN COLOR BY COLUMN CHANGES
    observeEvent(input$color_by_column, {
      seurat <- seurat_object()
      color_col <- input$color_by_column

      if (!is.null(seurat) && !is.null(color_col)) {
        unique_values <- unique(seurat@meta.data[[color_col]])
        initialized_palette <- rep("grey", length(unique_values))
        names(initialized_palette) <- as.character(sort(unique_values))
        palette(initialized_palette)

        # Update the cluster_to_color dropdown with values from the selected column
        updateSelectInput(session, "cluster_to_color", choices = sort(unique_values))

        # Reset plotted flag to trigger initial plot
        plotted(NULL)
      }
    })

    ##UPDATE THE GROUP SELECTION MENU
    observe({
      seurat <- seurat_object()
      grouping_column <- input$grouping_column
      if (!is.null(seurat) && !is.null(grouping_column)) {
        unique_values <- unique(seurat@meta.data[[grouping_column]])
        updateSelectInput(session, "group_value", choices = unique_values)
        seurat_object(seurat)
      }
    })

    # Observe event for updating palette choices based on the selected palette package
    observe({

      if (input$pal_types == "Discrete"){
        pkg_choices <- palettes_d_names %>% pull(package)
        updateSelectInput(session, "pal_package", choices = pkg_choices)
      }

      if (input$pal_types == "Continuous"){
        pkg_choices <- palettes_c_names %>% pull(package)
        updateSelectInput(session, "pal_package", choices = pkg_choices)
      }

      if (input$pal_types == "Dynamic"){
        pkg_choices <- palettes_dynamic_names %>% pull(package)
        updateSelectInput(session, "pal_package", choices = pkg_choices)
      }
    })

    ##update pal type selection
    observe({
      pkg_select <- input$pal_package

      if (input$pal_types == "Discrete" && !is.null(pkg_select)){
        pal_choices <- palettes_d_names %>% filter(package == pkg_select) %>% pull(palette)
        updateSelectInput(session, "pal_name", choices = pal_choices)
      }

      if (input$pal_types == "Continuous" && !is.null(pkg_select)){
        pal_choices <- palettes_c_names %>% filter(package == pkg_select) %>% pull(palette)
        updateSelectInput(session, "pal_name", choices = pal_choices)
      }

      if (input$pal_types == "Dynamic" && !is.null(pkg_select)){
        pal_choices <- palettes_dynamic_names %>% filter(package == pkg_select) %>% pull(palette)
        updateSelectInput(session, "pal_name", choices = pal_choices)
      }
    })


    ##SELECT CLUSTER RECOLOR BUTTON
    observeEvent(input$assign_cluster_color_button, {
      cluster <- input$cluster_to_color
      color <- input$col
      newpal <- palette()
      newpal[[paste(cluster)]] <- color
      palette(newpal)
    })

    ##GENERATE GROUP PALETTE BUTTON
    observeEvent(input$assign_group_color_button, {
      seurat <- seurat_object()
      color_col <- input$color_by_column
      grouping_col  <- input$grouping_column
      group_val  <- input$group_value
      package <- input$pal_package
      palette_name <- input$pal_name
      newpal <- isolate(palette())

      if(!is.null(seurat) && !is.null(color_col) && !is.null(grouping_col) && !is.null(group_val) && !is.null(package)){

        # Get the values from the color_by_column that match the grouping criteria
        group_items <- seurat@meta.data %>%
          select(all_of(c(color_col, grouping_col))) %>%
          unique() %>%
          filter(.data[[grouping_col]] == group_val) %>%
          pull(.data[[color_col]])


        pal_name <- paste0(package,"::",palette_name)

        tryCatch({
          if (input$pal_types == "Discrete" && !is.null(package)) {
            pal_vect <- paletteer::paletteer_d(pal_name, length(group_items), type = "continuous")
          }

          if (input$pal_types == "Continuous" && !is.null(package)) {
            pal_vect <- paletteer::paletteer_c(pal_name, length(group_items))
          }

          if (input$pal_types == "Dynamic" && !is.null(package)) {
            pal_vect <- paletteer::paletteer_dynamic(pal_name, length(group_items))
          }

          names(pal_vect) <- as.character(group_items)
          match_indices <- match(names(pal_vect), names(newpal))
          newpal[match_indices[!is.na(match_indices)]] <- pal_vect[!is.na(match_indices)]
          palette(newpal)

          grouping_col <- NULL
          group_val <- NULL
          package <- NULL
          palette_name <- NULL
          newpal <- NULL
        }, error = function(e) {
          # Display a warning without crashing the app
          showNotification("Selected palette has fewer colors than needed. Please select a different palette.", type = "warning")
        })
      }
    })

    ##DEFAULT UMAP PLOT GENERATION ON APP LAUNCH
    observe({
      pal <- palette()
      seurat <- seurat_object()
      color_col <- input$color_by_column
      if (is.null(plotted()) && !is.null(pal) && !is.null(color_col)){
        output$umap_plot <- renderPlot({
          p <- DimPlot(seurat, group.by = color_col, cols = pal) + ggtitle("")

          # Apply axes toggle (default is FALSE/off)
          if (!input$show_axes) {
            p <- p + NoAxes()
          }

          # Apply legend toggle (default is TRUE/on)
          if (!input$show_legend) {
            p <- p + theme(legend.position = "none")
          }

          p
        })
      }
    })

    ##BUTTON: GENERATE UMAP
    observeEvent(input$plot_button,{
      plotted(TRUE)
      seurat <- seurat_object()
      pal <- palette()
      color_col <- input$color_by_column
      output$umap_plot <- renderPlot({
        # Build base plot
        p <- DimPlot(seurat, group.by = color_col, cols = pal,
                    label = input$show_labels,
                    label.size = if(input$show_labels) 7 else NULL,
                    label.box = if(input$show_labels) TRUE else FALSE) +
          ggtitle("")

        # Apply axes toggle (default is FALSE/off)
        if (!input$show_axes) {
          p <- p + NoAxes()
        }

        # Apply legend toggle (default is TRUE/on)
        if (!input$show_legend) {
          p <- p + theme(legend.position = "none")
        }

        p
      })
    })

    ## VECTOR TEXT OUTPUT
    output$color_list <- renderPrint({
      cat("Copy and paste this palette for use in the seurat 'cols' argument", "\n","\n")
      cat("palette <- ","c(", paste(shQuote(unlist(palette())), collapse = ", "), ")\n")
    })

    ## PALETTE PREVIEW
    output$palette_preview <- renderPlot({
      pal <- palette()
      if (!is.null(pal)) {
        n <- length(pal)
        pal_df <- data.frame(
          group = names(pal),
          color = unname(pal),
          x = 1:n,
          y = 1
        )
        ggplot(pal_df, aes(x = x, y = y, fill = color)) +
          geom_tile(color = "white", linewidth = 2) +
          geom_text(aes(label = group), size = 3, fontface = "bold") +
          scale_fill_identity() +
          theme_void() +
          theme(plot.margin = margin(5, 5, 5, 5))
      }
    })

    ## SHUFFLE PALETTE COLORS
    observeEvent(input$shuffle_palette_button, {
      pal <- palette()
      if (!is.null(pal)) {
        # Keep the names, shuffle the colors
        shuffled_colors <- sample(unname(pal))
        names(shuffled_colors) <- names(pal)
        palette(shuffled_colors)
      }
    })

    ## SAVE PALETTE TO R ENVIRONMENT
    observeEvent(input$save_palette_button, {
      pal <- palette()
      var_name <- input$palette_var_name

      if (!is.null(pal) && nchar(var_name) > 0) {
        # Validate variable name
        if (!grepl("^[a-zA-Z][a-zA-Z0-9._]*$", var_name)) {
          showNotification("Invalid variable name. Use letters, numbers, dots, or underscores. Must start with a letter.",
                          type = "error", duration = 5)
          return()
        }

        tryCatch({
          # Assign to parent environment (the environment where DimPals was called from)
          assign(var_name, pal, envir = parent.frame(n = 2))
          showNotification(paste0("Palette saved as '", var_name, "' in your R environment!"),
                          type = "message", duration = 3)
        }, error = function(e) {
          showNotification(paste0("Error saving palette: ", e$message),
                          type = "error", duration = 5)
        })
      } else {
        showNotification("Please provide a valid variable name.", type = "warning", duration = 3)
      }
    })

    ## DOWNLOAD PLOT HANDLER
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("dimplot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        seurat <- seurat_object()
        pal <- palette()
        color_col <- input$color_by_column

        # Build the plot
        p <- DimPlot(seurat, group.by = color_col, cols = pal,
                    label = input$show_labels,
                    label.size = if(input$show_labels) 7 else NULL,
                    label.box = if(input$show_labels) TRUE else FALSE) +
          ggtitle("")

        # Apply axes
        if (!input$show_axes) {
          p <- p + NoAxes()
        }

        # Apply legend
        if (!input$show_legend) {
          p <- p + theme(legend.position = "none")
        }

        # Save the plot
        ggsave(file, plot = p,
               width = input$plot_width,
               height = input$plot_height,
               dpi = input$plot_dpi,
               units = "in")
      }
    )
  }

  shinyApp(ui,server)
}

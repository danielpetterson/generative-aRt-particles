library(shiny)
library(particles)
library(tidyverse)
library(magrittr)
library(ggforce)
library(gganimate)
library(transformr)
library(tidygraph)
library(colourpicker)
library(RColorBrewer)
library(jasmines)

## TODO Allow import of dataframe
## TODO Doubleclick polygon constraint

# Define UI for application that draws a histogram
ui <- fluidPage(
        titlePanel("Generative aRt with R"),
        fluidRow(
            column(3,
                   wellPanel(
                     selectInput("section",
                                 "Section",
                                 c("Generate Data", "Modify Forces/Constraints", "Display"),
                                 selected = "Generate Data"),
                     conditionalPanel(
                       condition = "input.section == 'Generate Data'",
                       
                       numericInput("seed",
                                    "Seed",
                                    1, min = 1, max = 1000),
                       selectInput("origin",
                                   "Origin Type",
                                   c("Single Point", "Within Range"),
                                   selected = "Single Point"),
                       sliderInput("xlims_data",
                                   "Range of Initial X Values",
                                   -1000, 1000, value = c(-100,100)),
                       sliderInput("ylims_data",
                                   "Range of Initial Y Values",
                                   -1000, 1000, value = c(-100,100)),
                       numericInput("n_sims",
                                    "Number of Simulations",
                                    5, min = 1, max = 35),
                       numericInput("max_dist_sims",
                                    "Maximum Distance Between Simulations",
                                    50000, min = 1, max = 10000000),
                       numericInput("n_particles",
                                    "Number of Particles per Simulation",
                                    20, min = 1, max = 500),
                       sliderInput("evolutions",
                                   "Number of Evolutions",
                                   1,300, 20, step = 1),
                       sliderInput("subset_evolutions",
                                   "Subset Evolutions",
                                   0, 300, value = c(0,300)),
                       numericInput("jit",
                                    "Jitter",
                                    20, min = 1, max = 500),
                       sliderInput("vel_dec",
                                   "Velocity Decay",
                                    0, 1, 0.3, step = 0.01)
                   ),
                   conditionalPanel(
                     condition = "input.section == 'Modify Forces/Constraints'",
                     selectInput("forces_and_limits", "Forces and Constraints:",
                               c("Random Force" = "f_rand",
                                 "Link Force" = "f_link",
                                 "Collision Force" = "f_coll",
                                 "Manybody Force" = "f_mb",
                                 "Mean Force" = "f_mean",
                                 "Attract to Point" = "f_point",
                                 "Velocity Constraint" = "vel_lims",
                                 "Polygon Constraint" = "poly_bounds"
                                 ),
                               selected = "Random Force"),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_rand'",
                     sliderInput("xlims_rand", "X-axis Bounds",
                                 -1000, 1000, value = c(-1000,1000), step = 1),
                     sliderInput("ylims_rand", "Y-axis Bounds",
                                 -1000, 1000, value = c(-1000,1000), step = 1)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_link'",
                     numericInput("link_str",
                                  "Strength",
                                  0, min = -500, max = 500),
                     numericInput("link_dist",
                                  "Distance",
                                  0, min = 0, max = 500)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_coll'",
                     numericInput("coll_str",
                                  "Strength",
                                  0, min = 0, max = 500),
                     numericInput("coll_rad",
                                  "Radius",
                                  0, min = 0, max = 500)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_mb'",
                     numericInput("mb_str",
                                  "Strength",
                                  0, min = -500, max = 500)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_mean'",
                     selectInput("mean_self",
                                 "Include Own Speed",
                                 c("Yes", "No"),
                                 selected = "No")),
                   conditionalPanel(
                     h5("Click on the image to choose a point"),
                     condition = "input.forces_and_limits == 'f_point'",
                     sliderInput("x_str",
                                 "X-Axis Strength",
                                 0, 10, value = 0, step = 0.1),
                     sliderInput("y_str",
                                 "Y-Axis Strength",
                                 0, 10, value = 0, step = 0.1)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'vel_lims'",
                     sliderInput("v_lims", "Velocity Boundaries",
                                 -1000, 1000, value = c(-1000,1000), step = 1)),
                   ),
                     conditionalPanel(
                       condition = "input.section == 'Display'",
                       selectInput("geom_type",
                                   "Geom Type",
                                   c("Point", "Curve", "Segment"),
                                   selected = "Point"),
                       colourInput("col_element", "Colour Palette", "white",
                                    showColour = "background"),
                       actionButton("col_select", "Add Colour"),
                       actionButton("col_clear", "Clear Palette"),
                       selectInput("col_factor",
                                   "Colour Factor",
                                   c("Time", "Particle", "Simulation"),
                                   selected = "Time"),
                        selectInput("coord_sys",
                                    "Coordinate System",
                                    c("Cartesian", "Flipped", "Polar X", "Polar Y"),
                                    selected = "Cartesian"),
                       sliderInput("alph_init", "Initial Opacity",
                                   0, 1, 0.8, step = 0.05),
                       sliderInput("alph_dec", "Opacity Decay",
                                   0, 1, 0.05, step = 0.001),
                       colourInput(
                         "backg_col", "Background Colour", "black",
                         showColour = "background"),
                       sliderInput("pathsize", "Size",
                                   0, 3, value = 0.2, step = 0.01),
                       sliderInput("xlims_disp", "X-axis Scale",
                                   -3, 3, value = c(-1,1), step = 0.001),
                       sliderInput("ylims_disp", "Y-axis Scale",
                                   -3, 3, value = c(-1,1), step = 0.001)
))),mainPanel(
  fluidRow(actionButton("gen_data", "Generate Data"), 
           actionButton("gen_image", "Update Output"),
           selectInput("img_anim",
                       "Type of Output",
                       c("Still Image", "Animation"),
                       selected = "Still Image")),
    textOutput('test'),
    #downloadButton('download', 'Download image'),
    dataTableOutput("data"),
    plotOutput("image",click="imageclick")
    )))


# Define server logic
server <- function(input, output) {
  
  generate_sim_data <- function(origin,
                                ns,
                                max_dist,
                                n,
                                evolutions,
                                evol_subset,
                                vel_dec,
                                xlims,
                                ylims,
                                alpha_init,
                                alpha_dec,
                                jit,
                                pathsize,
                                seed = NULL,
                                rand_x,
                                rand_y,
                                link_strength,
                                link_dist,
                                coll_strength,
                                coll_rad,
                                mb_strength,
                                mean_self_include,
                                x_force_str,
                                x_force_loc,
                                y_force_str,
                                y_force_loc,
                                vel_limits){
    l<-c()
    i=1
    
    if (mean_self_include == 'Yes') {
      self_include <- TRUE
    } else {
      self_include <- FALSE
    }
    
    isolate(set.seed(seed))
    
    while(i <= ns) {
      # Generate starting point coordinates
      if(origin == "Single Point") {
        loc <- isolate(runif(2, min = 0, max = max_dist))
        x <- isolate(runif(1, min = loc[1] - abs(xlims[1]), max = loc[1] + abs(xlims[2])))
        y <- isolate(runif(1, min = loc[2] - abs(ylims[1]), max = loc[2] + abs(ylims[2])))
      } else {
        x <- runif(n, min = xlims[1], max = xlims[2])
        y <- runif(n, min = ylims[1], max = ylims[2])
      }
      # Generate initial velocity of particle
      x_vel <- runif(n, min = -1, max = 1)
      y_vel <- runif(n, min = -1, max = 1)
      
      b <- create_empty(n) %>% 
        simulate(velocity_decay = vel_dec, setup = predefined_genesis(x,y,x_vel,y_vel)) %>% 
        wield(random_force, xmin=min(rand_x), xmax=max(rand_x), ymin=min(rand_y), ymax=max(rand_y)) %>%
        wield(link_force, strength=link_strength, distance=link_dist) %>%
        wield(collision_force, strength=coll_strength, radius=coll_rad) %>%
        wield(manybody_force, strength = mb_strength) %>%
        wield(mean_force, include_self = self_include) %>%
        wield(x_force, strength=x_force_str, x=x_force_loc) %>%
        wield(y_force, strength=y_force_str, y=y_force_loc) %>%
        impose(velocity_constraint, vmin=min(vel_limits), vmax=max(vel_limits)) %>%
        evolve(evolutions, record)
      l<-c(l,b)
      i=i+1
    }
    # Extract simulation history
    hist <- l[seq(6, length(l), 6)]
    list <- unlist(hist, recursive = FALSE)
    
    # Bind x,y coordinates by row
    traces <- data.frame(do.call(rbind, lapply(list, position)))
    names(traces) <- c('x', 'y')
    
    # Introduce jitter to avoid identical endpoints
    traces$x <- jitter(traces$x, jit)
    traces$y <- jitter(traces$y, jit)
    
    # Assign particle and simulation identifiers
    traces$particle <- as.factor(rep(1:n, evolutions))
    traces$sim <- as.factor(rep(1:ns, each=evolutions*n))
    traces$time <- rep(1:evolutions, each=n)
    traces$alpha <- alpha_init * (1 - alpha_dec)^(traces$time)
    
    traces_end <- traces %>%
      dplyr::mutate(time = time - 1) %>%
      dplyr::filter(time > 0)
    
    traces <- traces %>%
      dplyr::filter(time < max(time))
    
    traces$xend <- traces_end$x
    traces$yend <- traces_end$y
    traces$time <- as.factor(traces$time)
    traces$pathsize <- pathsize
    
    traces %<>% filter(as.numeric(time) >= min(evol_subset) & as.numeric(time) <= max(evol_subset))
    
    return(traces)
  }
    


    values <- reactiveValues()
    observeEvent(input$gen_data, {
        values$df <- isolate(reactive({generate_sim_data(input$origin,
                                                     input$n_sims,
                                                     input$max_dist_sims,
                                                     input$n_particles,
                                                     input$evolutions,
                                                     input$subset_evolutions,
                                                     input$vel_dec,
                                                     input$xlims_data,
                                                     input$ylims_data,
                                                     input$alph_init,
                                                     input$alph_dec,
                                                     input$jit,
                                                     input$pathsize,
                                                     input$seed,
                                                     input$xlims_rand,
                                                     input$ylims_rand,
                                                     input$link_str,
                                                     input$link_dist,
                                                     input$coll_str,
                                                     input$coll_rad,
                                                     input$mb_str,
                                                     input$mean_self,
                                                     input$x_str,
                                                     input$imageclick[["x"]],
                                                     input$y_str,
                                                     input$imageclick[["y"]],
                                                     input$v_lims
                                                    )}))
        values$colour_palette <- rep("white", length(values$df()$x))
        output$data <- renderDataTable(values$df(),options =list(pageLength = 5))
    })
    

    
    # Adds selected colour to vector of colours for palette generation
    observeEvent(input$col_select, {
      values$colour_palette_vec <- append(values$colour_palette_vec,as.character(input$col_element)) 
    })
    
    # Replaces palette with empty vector when "Clear Palette" button is hit 
    observeEvent(input$col_clear, {
      values$colour_palette_vec <- character()
    })
    
    # Generates image or animation when "Update Output" button is hit
    observeEvent(input$gen_image, {
      
      # Specifies what variable to change colour on
      if (input$col_factor == "Time") {
        colour_factor <-as.factor(values$df()$time)
        num_colours <- max(as.numeric(values$df()$time))
      } else if (input$col_factor == "Particle") {
        colour_factor <- as.factor(values$df()$particle)
        num_colours <- max(as.numeric(values$df()$particle))
      } else if (input$col_factor == "Simulation") {
        colour_factor <- as.factor(values$df()$sim)
        num_colours <- max(as.numeric(values$df()$sim))
      }
      
      if (length(values$colour_palette_vec)>=2) {
        # Creates function to interpolate colours from user defined palette
        palette_maker <- grDevices::colorRampPalette(values$colour_palette_vec)
        values$colour_palette <- palette_maker(num_colours)
      }
      
      # Basic ggplot parameters for output
      img_base<-ggplot2::ggplot(values$df()) +
        ggplot2::theme_void() + 
        ggplot2::theme(legend.position = 'none', panel.background = element_rect(fill = input$backg_col)) +
        ggplot2::xlim(min(values$df()$x)*abs(input$xlims_disp[1]),max(values$df()$x)*abs(input$xlims_disp[2])) +
        ggplot2::ylim(min(values$df()$y)*abs(input$ylims_disp[1]),max(values$df()$y)*abs(input$ylims_disp[2])) +
        ggplot2::scale_color_manual(values=values$colour_palette) # Assigns defined colour palette to ggplot object
      
      
      if (input$geom_type == "Point") {
           img <- img_base + ggplot2::geom_point(aes(x = x,
                         y = y,
                         colour = colour_factor,
                         alpha = alpha,
                         group = particle),
                         size = values$df()$pathsize)
      } else if (input$geom_type == "Curve") {
        img <- img_base + ggplot2::geom_curve(aes(x = x,
                         y = y,
                         colour = colour_factor,
                         alpha = alpha,
                         xend = xend,
                         yend = yend), 
                         size = values$df()$pathsize)
      } else if (input$geom_type == "Segment") {
        img <- img_base + ggplot2::geom_segment(aes(x = x,
                                        y = y,
                                        colour = colour_factor,
                                        alpha = alpha,
                                        xend = xend,
                                        yend = yend), 
                                        size = values$df()$pathsize[1])
      } 
      if (input$coord_sys == "Cartesian") {
        img_final <- img + ggplot2::coord_cartesian()
      } else if (input$coord_sys == "Flipped") {
        img_final <- img + ggplot2::coord_flip()
      } else if (input$coord_sys == "Polar X") {
        img_final <- img + ggplot2::coord_polar(theta="x")
      } else if (input$coord_sys == "Polar Y") {
        img_final <- img + ggplot2::coord_polar(theta="y")
      }
      
        
        
        
      if (input$img_anim == "Still Image") {
      output$image <- renderPlot({img_final}, height=1000, width=1000)
      } else if (input$img_anim == "Animation") {
        output$image <- renderImage({
          # A temp file to save the output.
          # This file will be removed later by renderImage
          
          outfile <- tempfile(fileext='.gif')
          
          # now make the animation
          img_anim = img_final +
            gganimate::transition_time(time = as.numeric(time))  +
            gganimate::ease_aes('linear') +
            gganimate::shadow_trail()
          
          anim_save("outfile.gif", animate(img_anim)) # New
          
          # Return a list containing the filename
          list(src = "outfile.gif",
               contentType = 'image/gif',
               width = 1000,
               height = 1000
          )}, deleteFile = TRUE)
        }
    
      
})}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(shinyBS)
library(particles)
library(tidyverse)
library(magrittr)
library(ggforce)
library(gganimate)
library(transformr)
library(tidygraph)
library(colourpicker)
library(RColorBrewer)
library(shinyalert)

# Define UI for application
ui <- fluidPage(
        useShinyalert(), # Displays information modal
        titlePanel("Generative aRt with R"),
        fluidRow(
            column(3,
                   wellPanel(
                     selectInput("section",
                                 "Section",
                                 c("Data", "Modify Forces/Constraints", "Display"),
                                 selected = "Data"),
                     conditionalPanel(
                       condition = "input.section == 'Data'",
                       
                       numericInput("seed",
                                    "Seed",
                                    1, min = 1, max = 1000),
                       bsTooltip("seed", "Sets the seed for the pseudorandom number generator",
                                 "right", options = list(container = "body")),
                       selectInput("origin",
                                   "Origin Type",
                                   c("Single Point", "Within Range"),
                                   selected = "Single Point"),
                       bsTooltip("origin", "Would you like your particles to start from a single point or within a predefined range?",
                                 "right", options = list(container = "body")),
                       conditionalPanel(
                         condition = "input.origin == 'Within Range'",
                       sliderInput("xlims_data",
                                   "Range of Initial X Values",
                                   -1000, 1000, value = c(-100,100)),
                       sliderInput("ylims_data",
                                   "Range of Initial Y Values",
                                   -1000, 1000, value = c(-100,100))),
                       numericInput("n_sims",
                                    "Number of Simulations",
                                    5, min = 1, max = 35),
                       bsTooltip("n_sims", "Select the number of simulations to plot",
                                 "right", options = list(container = "body")),
                       numericInput("max_dist_sims",
                                    "Maximum Distance Between Simulations",
                                    50000, min = 1, max = 10000000),
                       bsTooltip("max_dist_sims", "Set the maximum distance between simulation start points",
                                 "right", options = list(container = "body")),
                       numericInput("n_particles",
                                    "Number of Particles per Simulation",
                                    20, min = 1, max = 500),
                       bsTooltip("n_particles", "Set the number of particles per simulation",
                                 "right", options = list(container = "body")),
                       sliderInput("evolutions",
                                   "Number of Evolutions",
                                   1,300, 20, step = 1),
                       bsTooltip("evolutions", "Each evolution is one time step in which forces will act on a particle",
                                 "right", options = list(container = "body")),
                       sliderInput("subset_evolutions",
                                   "Subset Evolutions",
                                   0, 300, value = c(0,300)),
                       bsTooltip("subset_evolutions", "Modify this to only plot data from a smaller timeframe",
                                 "right", options = list(container = "body")),
                       numericInput("jit",
                                    "Jitter",
                                    20, min = 1, max = 500),
                       bsTooltip("jit", "Jitter shifts all particle locations slightly to avoid overlap",
                                 "right", options = list(container = "body")),
                       sliderInput("vel_dec",
                                   "Velocity Decay",
                                    0, 1, 0.3, step = 0.01),
                       bsTooltip("vel_dec", "Set the rate of speed decay",
                                 "right", options = list(container = "body"))
                   ),
                   conditionalPanel(
                     condition = "input.section == 'Modify Forces/Constraints'",
                     selectInput("forces_and_limits", "Forces and Constraints:",
                               c("Random Force" = "f_rand",
                                 "Collision Force" = "f_coll",
                                 "Manybody Force" = "f_mb",
                                 "Mean Force" = "f_mean",
                                 "Attract to Point" = "f_point",
                                 "Velocity Constraint" = "vel_lims"
                                 ),
                               selected = "Random Force"),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_rand'",
                     selectInput("rand_apply",
                                 "Apply Force?",
                                 c("Yes", "No"),
                                 selected = "Yes"),
                     sliderInput("xlims_rand", "X-axis Bounds",
                                 -1000, 1000, value = c(-1000,1000), step = 1),
                     sliderInput("ylims_rand", "Y-axis Bounds",
                                 -1000, 1000, value = c(-1000,1000), step = 1)),
                   conditionalPanel(
                     condition = "input.forces_and_limits == 'f_coll'",
                     sliderInput("coll_str",
                                  "Strength",
                                  0, 1, value = 0, step = 0.01),
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
                     selectInput("mf_apply",
                                 "Apply Force?",
                                 c("Yes", "No"),
                                 selected = "No"),
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
                                   c("Point", "Curve", "Segment", "Path"),
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
                                   -3, 3, value = c(-1,1), step = 0.001)),
                   

)),mainPanel(
  fluidRow(actionButton("gen_data", "Generate Data"), # Initial data is generated when this event is observed
           bsTooltip("gen_data", "Data must be generated before creating the image.",
                     "right", options = list(container = "body")),
           actionButton("show_data", "Show/Hide Data"), #Toggles Dataframe display
           actionButton("gen_image", "Update Output"), # Updates the image/animation
           bsTooltip("gen_image", "Click here to update the image.",
                     "right", options = list(container = "body")),
           actionButton("output_type", "Toggle Image/Animation"), # Toggles between output type. Animations take longer to render
           conditionalPanel(
             condition = "input.output_type%2 == 0",
             downloadButton('save', 'Download image')), # Download button appears when not set to output animation
           bsTooltip("save", "Download a copy of your masterpiece. Maybe print it out and put it on the fridge!",
                     "right", options = list(container = "body")),
           conditionalPanel(
             condition = "input.show_data%2 == 0",
            dataTableOutput("data")), # Table showing df
           plotOutput("image",click="imageclick") # Main image

    
    ))))


# Define server logic
server <- function(input, output) {
  
  # Modal that presents when the app opens
  shinyalert(
    title = "App Info",
    text = "This app uses data generated from a particle physics simulation to create images. 
     When you've selected the settings you want, hit 'Generate Data' followed by 'Update Image'. 
     Hover over any options for more information",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )
  
  # Creates data using all non-display parameters
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
                                seed,
                                rand_apply,
                                rand_x,
                                rand_y,
                                coll_strength,
                                coll_rad,
                                mb_strength,
                                mf_apply,
                                mean_self_include,
                                x_force_str,
                                x_force_loc,
                                y_force_str,
                                y_force_loc,
                                vel_limits){
    l<-c()
    i=1
    
    # Convert text input to bool for mean force parameter
    if (mean_self_include == 'Yes') {
      self_include <- TRUE
    } else {
      self_include <- FALSE
    }
    
    set.seed(seed)
    
    # Loop for each simulation
    while(i <= ns) {
      # Generate starting point coordinates
      if(origin == "Single Point") {
        loc <- isolate(runif(2, min = -max_dist/2, max = max_dist/2))
        x <- isolate(runif(1, min = loc[1] - abs(xlims[1]), max = loc[1] + abs(xlims[2])))
        y <- isolate(runif(1, min = loc[2] - abs(ylims[1]), max = loc[2] + abs(ylims[2])))
      } else {
        x <- isolate(runif(n, min = xlims[1], max = xlims[2]))
        y <- isolate(runif(n, min = ylims[1], max = ylims[2]))
      }
      # Generate initial velocity of particle
      x_vel <- runif(n, min = -1, max = 1)
      y_vel <- runif(n, min = -1, max = 1)
      
      b <- create_empty(n) %>% 
        simulate(velocity_decay = vel_dec, setup = predefined_genesis(x,y,x_vel,y_vel))
      
      if(rand_apply == 'Yes') {
        b %<>% wield(random_force, xmin=min(rand_x), xmax=max(rand_x), ymin=min(rand_y), ymax=max(rand_y))
      }
        b %<>% wield(collision_force, strength=coll_strength, radius=coll_rad) # Apply collision force
        b %<>% wield(manybody_force, strength = mb_strength) # Apply manybody force
      if(mf_apply == 'Yes') {
        b %<>% wield(mean_force, include_self = self_include) # Apply mean force
      }
        b %<>% wield(x_force, strength=x_force_str, x=x_force_loc) # The x component to the 'Attract to Point' force
        b %<>% wield(y_force, strength=y_force_str, y=y_force_loc) # The y component to the 'Attract to Point' force
        b %<>% impose(velocity_constraint, vmin=min(vel_limits), vmax=max(vel_limits)) # Impose upper and lower velocity bounds
        b %<>% evolve(evolutions, record) # Evolve over number of evolutions
      
        
      l<-c(l,b) # Append simulation object to list
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
    traces$alpha <- alpha_init * (1 - alpha_dec)^(traces$time-1)
    
    # Create xend and yend values
    traces_end <- traces %>%
      dplyr::mutate(time = time - 1) %>%
      dplyr::filter(time > 0)
    
    traces <- traces %>%
      dplyr::filter(time < max(time))
    
    traces$xend <- traces_end$x
    traces$yend <- traces_end$y
    traces$time <- as.factor(traces$time)
    traces$pathsize <- pathsize
    
    # Apply subsetting of time steps/evolutions
    traces %<>% filter(as.numeric(time) >= min(evol_subset) & as.numeric(time) <= max(evol_subset))
    
    return(traces)
  }
    


    values <- reactiveValues() # Create object for restoring reactive values
    
    # Generate and display data when button press observed
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
                                                     input$rand_apply,
                                                     input$xlims_rand,
                                                     input$ylims_rand,
                                                     input$coll_str,
                                                     input$coll_rad,
                                                     input$mb_str,
                                                     input$mf_apply,
                                                     input$mean_self,
                                                     input$x_str,
                                                     input$imageclick[["x"]],
                                                     input$y_str,
                                                     input$imageclick[["y"]],
                                                     input$v_lims
                                                    )}))
        # Set default palette to vector containing only white
        values$colour_palette <- rep("white", length(values$df()$x))
        # Display data table
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
    
    observeEvent(input$gen_data, {
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
        # Sets theme
        ggplot2::theme_void() + 
        # Removes legend and sets plot background colour
        ggplot2::theme(legend.position = 'none', panel.background = element_rect(fill = input$backg_col)) +
        # Sets xlim to the most extreme values observed across all simulations
        ggplot2::xlim(min(values$df()$x)*abs(input$xlims_disp[1]),max(values$df()$x)*abs(input$xlims_disp[2])) +
        # Sets ylim to the most extreme values observed across all simulations
        ggplot2::ylim(min(values$df()$y)*abs(input$ylims_disp[1]),max(values$df()$y)*abs(input$ylims_disp[2])) +
        # Assigns defined colour palette to ggplot object
        ggplot2::scale_color_manual(values=values$colour_palette) 
      
      # Append selected geom to img_base
      if (input$geom_type == "Point") {
           img <- img_base + 
             ggplot2::geom_point(aes(x = x,
                                     y = y,
                                     colour = colour_factor,
                                     alpha = alpha,
                                     group = particle),
                                     size = values$df()$pathsize)
      } else if (input$geom_type == "Curve") {
        img <- img_base + 
            ggplot2::geom_curve(aes(x = x,
                                    y = y,
                                    colour = colour_factor,
                                    alpha = alpha,
                                    xend = xend,
                                    yend = yend), 
                                    size = values$df()$pathsize)
      } else if (input$geom_type == "Segment") {
        img <- img_base + 
          ggplot2::geom_segment(aes(x = x,
                                    y = y,
                                    colour = colour_factor,
                                    alpha = alpha,
                                    xend = xend,
                                    yend = yend), 
                                    size = values$df()$pathsize[1])
      } else if (input$geom_type == "Path") {
        img <- img_base + 
          ggplot2::geom_path(aes(x = x,
                                 y = y,
                                 colour = colour_factor,
                                 alpha = alpha), 
                                 size = values$df()$pathsize[1])
      } 
      
      # Set coordinate system used to plot
      if (input$coord_sys == "Cartesian") {
        img_final <- img + ggplot2::coord_cartesian()
      } else if (input$coord_sys == "Flipped") {
        img_final <- img + ggplot2::coord_flip()
      } else if (input$coord_sys == "Polar X") {
        img_final <- img + ggplot2::coord_polar(theta="x")
      } else if (input$coord_sys == "Polar Y") {
        img_final <- img + ggplot2::coord_polar(theta="y")
      }
      
        
        
        
      if (input$output_type%%2==0) {
      output$image <- renderPlot({img_final}, height=800, width=800)
      } else  {
        output$image <- renderImage({
          
          # A temp file to save the output
          outfile <- tempfile(fileext='.gif')
          
          # Make the animation
          img_anim = img_final +
            gganimate::transition_time(time = as.numeric(time))  +
            gganimate::ease_aes('linear') +
            gganimate::shadow_wake(wake_length=0.5)
          
          anim_save("outfile.gif", animate(img_anim))
          
          # Return a list containing the filename
          list(src = "outfile.gif",
               contentType = 'image/gif',
               width = 800,
               height = 800
          )}, deleteFile = TRUE)
        }
      # Save image to file
      output$save <- downloadHandler(
        file = "aRtwork.png" , # Set default filename
        content = function(file) {
          ggsave(img_final, filename = file)
          png(file = file)
          dev.off()
        })
          
})})
}

# Run the application 
shinyApp(ui = ui, server = server)

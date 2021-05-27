library(shiny)
library(particles)
library(tidyverse)
library(tidygraph)
library(colourpicker)
library(vroom)

## TODO Allow import of dataframe
## TODO Generate custom palettes
## TODO Change geom_type
## TODO Apply forces with specified parameters

# Define UI for application that draws a histogram
ui <- fluidPage(
        titlePanel("Generative aRt with R"),
        fluidRow(
            column(3,
                   wellPanel(
                       h4("Data"),
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
                                    1, min = 1, max = 35),
                       numericInput("n_particles",
                                    "Number of Particles per Simulation",
                                    20, min = 1, max = 500),
                       sliderInput("evolutions",
                                   "Number of Evolutions",
                                   1,300, 20, step = 1),
                       numericInput("jit",
                                    "Jitter",
                                    20, min = 1, max = 500),
                       sliderInput("vel_dec",
                                   "Velocity Decay",
                                    0, 1, 0.3, step = 0.01),
                       actionButton("gen_data", "Generate Data")
                   ),
                   wellPanel(
                       h4("Display"),
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
                                   -3, 3, value = c(-1,1), step = 0.001),
                       actionButton("gen_image", "Generate Image")
)),mainPanel(
    downloadButton('downloadImage', 'Download image'),
    plotOutput("image"),
    dataTableOutput("data"))))



generate_sim_data <- function(origin,ns,n,evolutions,vel_dec,xlims,ylims,alpha_init,alpha_dec,jit,pathsize,seed = NULL){
    l<-c()
    i=1
    
    isolate(set.seed(seed))
    
    while(i <= ns) {
        # Generate starting point coordinates
      if(origin == "Single Point") {
        loc <- isolate(runif(2, min = -20000, max = 20000))
        x <- isolate(runif(1, min = loc[1] - abs(xlims[1]), max = loc[1] + abs(xlims[2])))
        y <- isolate(runif(1, min = loc[2] - abs(ylims[1]), max = loc[2] + abs(ylims[2])))
      } else {
#        loc <- runif(2, min = -20000, max = 20000)
        x <- runif(n, min = xlims[1], max = xlims[2])
        y <- runif(n, min = ylims[1], max = ylims[2])
      }
        # Generate initial velocity of particle
        x_vel <- runif(n, min = -1, max = 1)
        y_vel <- runif(n, min = -1, max = 1)
        
        b <- create_empty(n) %>% 
            simulate(velocity_decay = vel_dec, setup = predefined_genesis(x,y,x_vel,y_vel)) %>% 
           wield(link_force) %>%
           wield(manybody_force, strength = 0.5) %>%
           wield(mean_force, include_self = TRUE) %>%
           wield(random_force, strength = 10) %>%
           wield(center_force, sample(seq(-100:100)),sample(seq(-100:100)), strength = 100) %>%
           wield(collision_force, radius = runif(100, min = 0.1, 0.2), n_iter = 5) %>%
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
      
    return(traces)
}

# Define server logic
server <- function(input, output) {
  

    
    values <- reactiveValues()
    observeEvent(input$gen_data, {
        values$df <- isolate(reactive({generate_sim_data(input$origin,
                                                     input$n_sims,
                                                     input$n_particles,
                                                     input$evolutions,
                                                     input$vel_dec,
                                                     input$xlims_data,
                                                     input$ylims_data,
                                                     input$alph_init,
                                                     input$alph_dec,
                                                     input$jit,
                                                     input$pathsize,
                                                     input$seed)}))
        output$data <- renderDataTable(values$df())
    })
    

    
    observeEvent(input$gen_image, {
      img<-ggplot(values$df()) +
            # geom_point(aes(x = x,
            #                y = y,
            #                group = particle,
            #                colour = as.factor(time),
            #                alpha = alpha,
            #                xend = xend,
            #                yend = yend), size = values$df()$pathsize) + 
            geom_curve(aes(x = x,
                           y = y,
                           group = particle,
                           colour = as.factor(time),
                           alpha = alpha,
                           xend = xend,
                           yend = yend), size = values$df()$pathsize) +
            theme_void() + 
            theme(legend.position = 'none', panel.background = element_rect(fill = input$backg_col)) +
            xlim(min(values$df()$x)*abs(input$xlims_disp[1]),max(values$df()$x)*abs(input$xlims_disp[2])) +
            ylim(min(values$df()$y)*abs(input$ylims_disp[1]),max(values$df()$y)*abs(input$ylims_disp[2]))
      output$image <- renderPlot({img})

    })
    
    output$downloadImage <- downloadHandler(
      filename = "Modified_image.jpeg",
      contentType = "image/jpeg",
      content = function(file) {
        ## copy the file from the updated image location to the final download location
        file.copy(img, file)
      }
    )  
    #   downloadHandler(
    #   filename = function() {'test.png' },
    #   content = function(file) {
    #     ggsave(file, plot = output$image(), device = "png")
    #   }
    # )
    
}

# Run the application 
shinyApp(ui = ui, server = server)

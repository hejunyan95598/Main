install.packages("plotly")
library(plotly)
#devtools::install_github("ropensci/plotly")
rm(list = ls())
gc()
``

x <- 0.2
y <- 0.72
speed <- 250
nbkdrops <- 100

# Colorset for plot
# See http://colorhunt.co/
cols <- c("#FFC85B", "#379956","#234C63")
ncolors <- length(cols)


# Function to create random points by adding jitter to ----
# a starting set of points
n <- 1000  # Number of points

# Starting template
bkdrop.x <- runif(n, min = 0, max = 1)
bkdrop.y <- runif(n, min = 0, max = 1)

# Function Definition
bkdrop <- function(n = 1000, amount = 0.005){
  
  x <- jitter(bkdrop.x, amount = amount)
  y <- jitter(bkdrop.y, amount = amount)
  
  df <- data.frame(x, y)
  
  return(df)
  
}

# Make backdrops ----
# Each call to the backdrop function is a separate frame
# Number of frames is controlled by nbkdrops

bkdrop.df <- data.frame()
for(i in 1:nbkdrops){
  temp <- bkdrop()
  temp <- data.frame(temp, frame = i, color = sample(1:ncolors, size = nrow(temp), replace = T))
  bkdrop.df <- rbind(bkdrop.df, temp)
  
}

# Make back lights ----
# Coordinates for backlight rectangles
# Will be plotted as line segments
bklight.x <- c(0.28, 0.18, 0.48)
bklight.y <- c(0.42, 0.62, 0.65)
bklight.xend <- c(0.63, 0.50, 0.75)
bklight.yend <- c(0.42, 0.62, 0.65)

# Function to create a dataframe containing coordinates, frame and
# color of each backlight segment
makebklight <- function(id){
  bklight <- data.frame()
  
  for(i in 1:nbkdrops){
    temp <- data.frame(x = bklight.x[id],
                       y = bklight.y[id],
                       xend = bklight.xend[id],
                       yend = bklight.yend[id],
                       frame = i, 
                       color = sample(1:ncolors, size = 1))
    
    bklight <- rbind(bklight, temp)
  }
  
  return(bklight)
}

# Create backlight segments
bklight1 <- makebklight(1)
bklight2 <- makebklight(2)
bklight3 <- makebklight(3)

# Initialize colors for first frame
bklight1$color[1] <- 1
bklight2$color[1] <- 2
bklight3$color[1] <- 3

# Plot !! ----
p <- plot_ly(height = 800, width = 1024, 
             colors = cols, 
             frame = ~frame,
             x = ~x, 
             y = ~y,
             color = ~factor(color)) %>%  
  
  # Backdrop
  add_markers(data = bkdrop.df, 
              opacity = 0.8,
              marker = list(symbol = "star", size = 8),
              hoverinfo = "none") %>%
  
  # Add segments (for back lighting)
  add_segments(data = bklight1, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>%
  
  add_segments(data = bklight2, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>% 
  
  add_segments(data = bklight3, 
               xend = ~xend, yend = ~yend, 
               line = list(width = 150)) %>% 
  
  # Animation options
  # See https://cpsievert.github.io/plotly_book/key-frame-animations.html
  
  animation_opts(speed, easing = "linear", transition = 0) %>%
  animation_button(x = 1, xanchor = "right", y = 1, yanchor = "bottom") %>%
  animation_slider(hide = T) %>%
  
  # Layout, annotations and shapes
  
  layout(
    showlegend = F,
    
    xaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    yaxis = list(title = "", showgrid = F, zeroline = F, showticklabels = F, range = c(0, 1)),
    
    annotations = list(
      
      # For shadow
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.002, y = y + 0.002, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.003, y = y + 0.003, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x + 0.004, y = y + 0.004, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "black")),
      
      # Actual
      list(xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "top",
           x = x, y = y, 
           showarrow = F,
           text = "Happy New<br>Year !",
           font = list(size = 100, family = "Times New Roman",
                       color = "#ff6666"))
    ),
    
    shapes = list(
      
      # Border
      list(xref = "paper", yref = "paper",
           x0 = 0, y0 = 0, 
           x1 = 1, y1 = 1,
           type = "rect",
           line = list(width = 10, color = cols[1])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.01, y0 = 0.01, 
           x1 = 0.99, y1 = 0.99,
           type = "rect",
           line = list(width = 10, color = cols[2])),
      
      list(xref = "paper", yref = "paper",
           x0 = 0.02, y0 = 0.02, 
           x1 = 0.98, y1 = 0.98,
           type = "rect",
           line = list(width = 10, color = cols[3])),
      
      # Black outline
      list(xref = "plot", yref = "plot",
           path = "
           M 0.50 0.53
           L 0.50 0.50
           L 0.18 0.50 
           L 0.18 0.73
           L 0.48, 0.73",
           type = "path",
           line = list(width = 7, color = "black")),
      
      list(xref = "plot", yref = "plot",
           path = "
           M 0.50 0.535
           L 0.48 0.535
           L 0.48 0.77
           L 0.75 0.77
           L 0.75 0.535
           Z",
           type = "path",
           line = list(width = 7, color = "black")),
      
      list(xref = "plot", yref = "plot",
           path = "
           M 0.28 0.5
           L 0.28 0.31
           L 0.63 0.31
           L 0.63 0.535",
           type = "path",
           line = list(width = 7, color = "black"))
      
    )
  )

p
Play

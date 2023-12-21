install.packages("ggplot2")
library(ggplot2)

str(spotify_music)
head(spotify_music)
summary(spotify_music)
sum(is.na(spotify_music))
data<-spotify_music[!duplicated(spotify_music),]
#Univariate Analysis Histogram of bpm (Beats Per Minute)
ggplot(spotify_music, aes(x = bpm)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  ggtitle("Distribution of Beats Per Minute (BPM)") +
  xlab("BPM") +
  ylab("Frequency")

#Boxplot of pop (Popularity)
ggplot(spotify_music, aes(y = pop)) + 
  geom_boxplot (fill = "orange") +
  ggtitle("Boxplot of Song Popularity") +
  ylab("Popularity")


# Bivariate Analysis Scatter Plot between nrgy (Energy) and dnce (Danceability)
ggplot(spotify_music, aes(x = nrgy, y = dnce)) + 
  geom_point(alpha = 0.5, color = "red") +  # Scatter plot points
  geom_smooth(method = "lm", color = "blue") +  # Add linear regression line
  ggtitle("Energy vs Danceability with Trend Line") +
  xlab("Energy") +
  ylab("Danceability")

# Scatter Plot of val (Valence) vs. pop (Popularity)
ggplot(spotify_music, aes(x = val, y = pop)) + 
  geom_point(alpha = 0.5, color = "purple") +
  ggtitle("Valence vs Popularity") +
  xlab("Valence") +
  ylab("Popularity")

# Multivariate Analysis Pair Plot of Selected Variables
install.packages("GGally")
library(GGally)

spotify_subset <- spotify_music[, c("bpm", "nrgy", "dnce", "pop")]
ggpairs(spotify_subset)

# Correlation Matrix of Numerical Variables
install.packages("corrplot")
library(corrplot)

numerical_data <- spotify_music[, c("bpm", "nrgy", "dnce", "dB", "live", "val", "dur", "acous", "spch", "pop")]
cor_matrix <- cor(numerical_data, use = "complete.obs")
corrplot(cor_matrix, method = "circle") 

# Distribution of the BPM (beats per minute) in the songs

ggplot(data, aes(x = bpm)) + 
  geom_histogram(binwidth = 20, fill = "blue", color = "black") +
  labs(title = "Distribution of BPM", x = "BPM", y = "Count")

# Spread of 'danceability' (dnce)
ggplot(data, aes(y = dnce)) + 
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of Danceability", y = "Danceability", x = "")

# Relationship between 'energy' (nrgy) and 'popularity' (pop)
ggplot(data, aes(x = nrgy, y = pop)) + 
  geom_point(color = "red") +
  labs(title = "Energy vs Popularity", x = "Energy", y = "Popularity")

# Number of songs by each artist
ggplot(data, aes(x = factor(artist))) + 
  geom_bar(fill = "green") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of Songs by Artist", x = "Artist", y = "Count")

library(shiny)
ui <- fluidPage(
  titlePanel("Data Visualization App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Select Plot Type:",
                  choices = c("Histogram", "Boxplot", "Scatter Plot", "Bar Chart"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$plotType == "Histogram") {
      ggplot(data, aes(x = bpm)) + 
        geom_histogram(binwidth = 10)
    } else if (input$plotType == "Boxplot") {
      ggplot(data, aes(y = dnce)) + 
        geom_boxplot()
    } else if (input$plotType == "Scatter Plot") {
      ggplot(data, aes(x = nrgy, y = pop)) + 
        geom_point()
    } else if (input$plotType == "Bar Chart") {
      ggplot(data, aes(x = factor(artist))) + 
        geom_bar() + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })
}

shinyApp(ui = ui, server = server)

install.packages('rsconnect')

rsconnect::setAccountInfo(name='mx1s4z-anthony-clemons', 
                          token='95CD1283ACB4D0A9631860574EDA9379', 
                          secret='NUEqmIxhTiwBL6NetJ+5w10mgBsIw4fSr0fSXfhX')

library(rsconnect)
rsconnect::deployApp('path/to/your/app')

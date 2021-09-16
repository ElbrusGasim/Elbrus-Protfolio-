shinyServer(function(input, output) {
    
    data_base_1 <-reactive({
        
        req(input$file_1)
        
        file_data <- read.csv(input$file_1$datapath)
        #file_data <- fread(FileName,stringsAsFactors=strAsFact)
        
        if(is.null(file_data)){
            print("Empty File")
            return(NULL)
        }
        
        file_data
        
    })
    
    data_base_2 <-reactive({
        
        req(input$file_2)
        
        file_data <- read.csv(input$file_2$datapath)
        #file_data <- fread(FileName,stringsAsFactors=strAsFact)
        
        if(is.null(file_data)){
            print("Empty File")
            return(NULL)
        }
        
        file_data
        
    })

    output$select_user_id <- renderUI({
        
        rating_data <- data_base_2()
        selectInput("user_id","Please select User ID for Movie Recommendation",choices = unique(rating_data[,"userId"]))
        
    })
    
    movie_recomendation <- reactive({
        user <- list(id = 1, all_ratings=NULL)
        class(user) <- "User_Summary"
        user
        
        # define constructor, User Rating Summary
        User_Summary <- function(id, usr_ratings) {
            # TOOL#1. defensive programming
            if(id <= 0) stop("Invalid User ID.")
            #id = 1
            #all_ratings <- df_ID_rating[userId == id,rating]
            
            
            tot_movies <- length(usr_ratings)
            tot_movies
            avg_rating <- sum(usr_ratings)/tot_movies
            
            # create a list of constructor values
            value <- list(id= id, all_ratings=usr_ratings, TotalMovies = tot_movies, AvgRating = avg_rating )
            
            # assign attributes of the class using above cons. values.
            attr(value, "class") <- "User_Summary"
            value
            #print("done...")
        }
        
        # define own S3 function,print for the class User_Summary
        print.User_Summary <- function(obj) {
            cat("USER ID is", obj$id, "\n")
            cat("Total Movies viewd by User : ", obj$TotalMovies, "\n")
            cat("Average User Ratings for All Movies", obj$AvgRating)
        }
        
        # Define S3 Generic function. Display Max and Min User Rating
        # for the current user.
        max_min.User_Summary <- function(obj) {
            #all_ratings <- df_ID_rating[userId == id,rating]
            max_rating <- max(obj$all_ratings)
            min_rating <- min(obj$all_ratings)
            
            cat("Maximum Rating Given by the user is", max_rating, "\n")
            cat("Minimjum Rating Given by the user is", min_rating)
        }
        
        
        #===========================================================
        # Define R6 Classes for Object Oriented Programming.
        # R6 includes S4 type classes as well.
        #===========================================================
        MovieRecom <- R6Class("MovieRecom", 
                              
                              private = list(
                                  movie_genre = NULL,
                                  training_data = NULL,
                                  testing_data = NULL,
                                  recom_model = NULL,
                                  
                                  init_recom_model = function(){
                                      movie_genre <- self$movies %>% select(genres) %>% as.data.table()
                                      
                                      movie_genre2 <- as.data.table(tstrsplit(movie_genre[,1], '[|]', 
                                                                              type.convert=TRUE), 
                                                                    stringsAsFactors=FALSE) 
                                      # Create a list of all movie genres, 18 in total.
                                      list_genre = c("Action", "Adventure", "Animation", "Children", 
                                                     "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                                                     "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                                                     "Sci-Fi", "Thriller", "War", "Western")
                                      
                                      
                                      # Compute No of rows and columns.
                                      N_rows = length(movie_genre2)
                                      N_cols = length(list_genre)
                                      
                                      # TOOL#1, dtplyr::data.table()
                                      movie_gen_18_dt = as.data.table((matrix(0, ncol = N_cols, nrow = N_rows)))
                                      colnames(movie_gen_18_dt) = list_genre
                                      
                                      # TOOL#1, TOOL#6-Vectorization.
                                      # Use of %>%, vectorization without using for loops for each row
                                      # here movie genres are set on a vector of data table using vectored code
                                      # as shown below.
                                      for (genre_no in seq(N_cols)) {
                                          movie_gen_18_dt[movie_genre[,1] %like% colnames(movie_gen_18_dt)[genre_no] ,genre_no] <- 1
                                      }
                                      
                                      # create a rating matrix with movie ratings.
                                      rating_data = self$movie_ratings
                                      ratingMatrix = dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
                                      #TOOL#6-Vectorization
                                      ratingMatrix = as.matrix(ratingMatrix[,-1]) #remove userIds
                                      ratingMatrix = as(ratingMatrix, "realRatingMatrix")
                                      
                                      # Select only users who have rated atleast 50 movies.
                                      self$movie_ratings = ratingMatrix[rowCounts(ratingMatrix) > 50,
                                                                        colCounts(ratingMatrix) > 50]
                                      
                                  }
                              ), # End of private list
                              
                              # Create Public attributes                    
                              public = list( 
                                  movie_ratings = NULL,
                                  movies = NULL,
                                  mov_rating = NULL,
                                  predicted_recommendations = NULL,
                                  
                                  # Define Constructor of the Class.
                                  initialize = function( movies, mov_ratings ) {
                                      #stopifnot(is.character(name), length(mov_ratings) == 1)
                                      #stopifnot(is.object(movies), length(movies) < 8)
                                      #stopifnot(is.object(mov_ratings), length(mov_ratings) < 8)
                                      
                                      
                                      self$movie_ratings <- mov_ratings
                                      self$movies <- movies
                                      
                                      # Build the recommendation model by calling
                                      # private function of this class.
                                      private$init_recom_model()
                                  },
                                  
                                  # Define Print function
                                  print = function(...) {
                                      # print predicted ratings
                                      #cat("Person: \n")
                                      #cat("  Name: ", self$name, "\n", sep = "")
                                      #cat("  Age:  ", self$age, "\n", sep = "")
                                      cat("Movie Recommendation Class.")
                                      invisible(self)
                                  },
                                  
                                  predict_recommendation = function(training_data, testing_data) {
                                      
                                      rec_model <- Recommender(data = training_data,
                                                               method = "IBCF",
                                                               parameter = list(k = 30))
                                      
                                      # Predict top 10 recommendations
                                      top_recommendations <- 10 # the number of items to recommend to each user
                                      self$predicted_recommendations <- predict(object = rec_model,
                                                                                newdata = testing_data,
                                                                                n = top_recommendations)
                                      
                                      
                                  },
                                  
                                  get_recommendation_for_user = function(userid) {
                                      
                                      stopifnot(is.numeric(userid), userid > 0, userid <= length(self$predicted_recommendations) )
                                      # Get recommendations for Another user based on predictions of a user
                                      # TOOL#6-Vectorization
                                      user1 <- self$predicted_recommendations@items[[userid]] # recommendation for the first user
                                      
                                      # Top 10 movies of User1, TOOL#6-Vectorization
                                      movies_user1 <- self$predicted_recommendations@itemLabels[user1]
                                      
                                      # predict movies for user2, TOOL#6-Vectorization
                                      movies_user2 <- movies_user1
                                      for (index in 1:10) {
                                          movies_user2[index] <- as.character(subset(movie_data,
                                                                                     movie_data$movieId == movies_user1[index])$title)
                                      }
                                      
                                      return(movies_user2)
                                  }                 
                              ) # end of public lsit
                              
        )
        # End of Class definition, MovieRecom
        
        movie_data <- data_base_1()
        rating_data <- data_base_2()
        #rating_data <- rating_data[rating_data[,1]==as.numeric(input$user_id),]
        
        
        movie_data_lz <- lazy_dt(movie_data)
        rating_data_lz <- lazy_dt(rating_data)
        
        df_ID_rating <- rating_data_lz %>% select(userId,rating) %>% as.data.table()
        
        df_avg_rating_tb <- rating_data_lz %>% as_tibble()
        df_avg_rating <- df_avg_rating_tb %>% group_by(userId) %>% summarize(avg_rat=mean(rating))
        all_users_avg_rating <- sum(df_avg_rating$avg_rat)/length(df_avg_rating$userId)
        
        cat("Average Rating of all users is ", all_users_avg_rating )
        
        usr_id <- input$user_id
        
        usr_ratings <- df_ID_rating$rating[df_ID_rating$userId==usr_id]
        usr_summ <- User_Summary(usr_id, usr_ratings)
        
        usr_summ
        
        max_min.User_Summary(usr_summ)
        
        Movie_Rec_Sys <- MovieRecom$new(movie_data_lz,rating_data)
        
        movie_ratings <- Movie_Rec_Sys$movie_ratings
        set.seed(123)
        sampled_data<- sample(x = c(TRUE, FALSE),
                              size = nrow(movie_ratings),
                              replace = TRUE,
                              prob = c(0.7, 0.3))
        training_data <- movie_ratings[sampled_data, ]
        testing_data <- movie_ratings[!sampled_data, ]
        
        Movie_Rec_Sys$predict_recommendation(training_data,testing_data)
        recommended_movies <- Movie_Rec_Sys$get_recommendation_for_user(as.numeric(input$user_id))
        
        recommended_movies
        
    })
    
    output$print_movie <- renderTable({
        
        movies <- movie_recomendation()
        movies <- as.data.frame(movies)
        names(movies) <- c("Recommended Movies")
        movies[,"#"] <- index(movies)
        movies[,c("#","Recommended Movies")]
        
    })
    

})

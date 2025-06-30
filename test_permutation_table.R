library("tna")
library("JTNA")

# Create sample data for testing permutation table
set.seed(123)
n_students <- 20
n_actions <- 100

# Create sample data with groups
sample_data <- data.frame(
  actor = sample(paste0("Student_", 1:n_students), n_actions, replace = TRUE),
  action = sample(c("Reading", "Writing", "Discussion", "Quiz", "Assignment"), n_actions, replace = TRUE),
  time = as.POSIXct("2023-01-01") + sample(1:1000, n_actions) * 60,
  group = sample(c("A", "B"), n_actions, replace = TRUE)
)

print("Testing JTNA GroupTNA permutation table...")

tryCatch({
  result <- GroupTNA(
    data = sample_data,
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_action = "action", 
    buildModel_variables_long_time = "time",
    buildModel_variables_long_group = "group",
    buildModel_variables_long_order = NULL,
    permutation_show_text = TRUE,
    permutation_iter = 10,  # Keep low for testing
    permutation_paired = FALSE,
    permutation_level = 0.05
  )
  
  print("JTNA GroupTNA executed successfully!")
  
  if(!is.null(result$permutationContent)) {
    print("Permutation table exists")
    
    # Try to access the data
    if(exists("asDF", where = result$permutationContent)) {
      print("Permutation table data:")
      perm_data <- result$permutationContent$asDF
      print(head(perm_data, 10))
      
      # Check if edge names are properly populated
      if(nrow(perm_data) > 0) {
        print("Sample edge names:")
        print(unique(perm_data$edge_name)[1:5])
      }
    } else {
      print("No asDF method available")
    }
  } else {
    print("Permutation table is NULL")
  }
  
}, error = function(e) {
  print(paste("Error in JTNA GroupTNA:", e$message))
}) 
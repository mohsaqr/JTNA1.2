library("tna")

# Create sample data for testing permutation test structure
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

print("Building model for permutation test...")

# Prepare data
dataForTNA <- prepare_data(sample_data, actor="actor", time="time", action="action")
group <- dataForTNA$long_data[!duplicated(dataForTNA$long_data$.session_id),]
model <- group_model(x=dataForTNA, group=group$group, type="relative")

print("Testing permutation test...")
permutationTest <- permutation_test(x=model, iter=10, paired=FALSE, level=0.05)

print("Permutation test structure:")
print(names(permutationTest))

for (comparison_name in names(permutationTest)) {
  print(paste("Comparison:", comparison_name))
  comparison_data <- permutationTest[[comparison_name]]
  print("Comparison data names:")
  print(names(comparison_data))
  
  if (!is.null(comparison_data$edges)) {
    print("Edges structure:")
    print(names(comparison_data$edges))
    
    if (!is.null(comparison_data$edges$stats)) {
      print("Stats structure:")
      stats_df <- comparison_data$edges$stats
      print("Stats column names:")
      print(colnames(stats_df))
      print("Stats data types:")
      print(sapply(stats_df, class))
      print("First few rows:")
      print(head(stats_df))
      
      # Check if there are from/to columns or edge identifiers
      if("from" %in% colnames(stats_df) && "to" %in% colnames(stats_df)) {
        print("Found from/to columns:")
        print(unique(stats_df$from))
        print(unique(stats_df$to))
      } else {
        print("No from/to columns found. Available columns:")
        print(colnames(stats_df))
        print("Row names:")
        print(rownames(stats_df))
      }
    }
  }
} 
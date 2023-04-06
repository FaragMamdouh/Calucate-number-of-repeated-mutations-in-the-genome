library(dplyr)
setwd("C:/Users/HP 1030 G2/Desktop/Dr Mustafa/Task 1/Test_fwd")
path= "C:/Users/HP 1030 G2/Desktop/Dr Mustafa/Task 1/Test_fwd/"
topfolders = c(list.files())
subfolders= list.dirs(topfolders)
data_list <- list()

for (subfolder in subfolders){
  files <- list.files(paste(path, subfolder, sep = "/"), pattern = "^outTable")
  for (file in files){
    # Read the file using the appropriate function (e.g. read.csv() or read.table())
    data <- read.table(paste(path, subfolder, file, sep = "/"), header= TRUE)
    data_list[[file]] <- data
  }
}
Process_dataset= function(dataset){
  counts= as.data.frame(matrix(ncol= 2, nrow= nrow(dataset)))
  counts$V1 <- paste(dataset$AllSubs) # Create V1 column by pasting AllSubs column
  counts$V2 <- paste(dataset$gAllSubs) # Create V2 column by pasting gAllSubs column

  names(counts) <- c("AllSubs", "gAllSubs") # Rename columns to AllSubs and gAllSubs

  Counts_first_column <- filter(counts, gAllSubs == "-") %>% select(AllSubs) # Filter to rows where gAllSubs == "-" and select AllSubs
  Results= table(Counts_first_column) # Count frequencies of AllSubs
  Results=as.data.frame(Results) # Convert to data frame
  # Check if AllSubs or gAllSubs column is empty
  Counts_Both_column <- filter(counts, gAllSubs != "-") # Filter to rows where gAllSubs != "-"
  for (i in 1:nrow(Counts_Both_column)) {
    if (!is.na(Counts_Both_column[i,1]) && !is.na(Counts_Both_column[i,2])) {
      if (Counts_Both_column[i,1] == Counts_Both_column[i,2]) {
        Counts_Both_column <- Counts_Both_column[-i, ]
      }
    }
  }
  Counts_Both_column <- mutate(Counts_Both_column, Combined =paste(AllSubs, gAllSubs, sep="/"))
  results2= as.data.frame(table(Counts_Both_column$Combined))
  colnames(results2) <- c("AllSubs","Freq")
  
  # Add more comments here describing the next few steps
  Results <- rbind(Results, results2) # unit Results and results2 together
  return(Results)
}
# Run the function on each dataset in data_list
final_results <- list()
for (i in names(data_list)) {
  if (nrow(data_list[[i]]) == 0) {
    # If dataset has no rows, create an empty data frame with same column names as final_results[[1]]
    final_results[[i]] <- data.frame(matrix(ncol = ncol(final_results[[1]]), nrow = 0))
    colnames(final_results[[i]]) <- colnames(final_results[[1]])
  } else {
    # If dataset has rows, process the dataset
    final_results[[i]] <- Process_dataset(data_list[[i]])
  }
}
# Loop through each element in final_results list
data_combined <- final_results[[1]]
for (i in 2:length(final_results)) {
  if ("AllSubs" %in% colnames(final_results[[i]])) {
    if (is.logical(final_results[[i]]$AllSubs)) { # Check if the "AllSubs" column is of logical type, and if so, convert it to a factor
      final_results[[i]]$AllSubs <- as.factor(final_results[[i]]$AllSubs)
    }
    
    data_combined <- left_join(data_combined, final_results[[i]], by = "AllSubs", suffix = c("", paste0("_", names(final_results)[i])))
    # Rename any columns in the [combined data]
    colnames(data_combined)[colnames(data_combined) == paste0("related_column_", names(final_results)[i])] <- paste0("related_column_", names(final_results)[i], "_", i - 1)
  } else {
    message(paste0("Warning: AllSubs column not found in data frame ", i))
  }
}
View(data_combined)



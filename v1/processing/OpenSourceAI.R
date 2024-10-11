library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(readxl)
library(ggcorrplot)
library(igraph)
library(ggrepel)
library(corrr)
library(mgcv)
library(patchwork)
library(gt)

# Color palette definition
color_palette <- "Paired"

# Import main data file
data.initial <- read_delim("../data/leaderboard_deduped.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Import data details
data.details  <- read_delim("../data/leaderboard_repo_detailes.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE) |> select(-1)

# Check if we have duplicates in data.details
data.details |> duplicated() |> sum()

# Remove duplicates from data.details
data.details[-(data.details |> duplicated() |> which()),] -> data.details

# Join data items with corresponding details
data.initial |> left_join(data.details, by = c("model_name_for_query" = "name")) -> data

# Import dataset maps
dataset.maps <- read_delim("../data/leaderboard_datasets.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Rename the first column so it matches the column in data
colnames(dataset.maps)[1] <- "model_name_for_query"

# Import base models
base.models <- read_delim("../data/leaderboard_base_models.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Rename the first column so it matches the column in data
colnames(base.models)[1] <- "model_name_for_query"

# Import author activity
author.activity <- read_delim("../data/leaderboard_repo_activity.csv", 
    delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Import commit data
commit.data <- read_delim("../data/leaderboard_commit_data.csv", 
    delim = ",", escape_double = FALSE, trim_ws = TRUE)

# Remove the first column which is row numbers
commit.data[,-1] -> commit.data

# Check if we have missing values in Average accuracy
sum(is.na(data$`Average ⬆️`))

# Check if data we created has same number of rows as initial data
data.initial$model_name_for_query |> unique() |> length() == data$model_name_for_query |> unique() |> length()

# Check if we have duplicates in final data
data[data |> duplicated(),]

# Check number of flagged, merged MoE models
data$Flagged |> sum()
data$Merged |> sum()
data$MoE |> sum()

# FOR SOME REASON MERGED, FLAGGED AND MOE ARE INVERTED TRUE AND FALSE (this was checked manually on the HF website)
data |> mutate(Flagged = ifelse(Flagged == "FALSE", TRUE, FALSE), Merged = ifelse(Merged == "FALSE", TRUE, FALSE), MoE = ifelse(MoE == "FALSE", TRUE, FALSE)) -> data

# Arrange data by Average accuracy and rename columns that have special characters
data |> arrange(desc(`Average ⬆️`)) |> rename(Average = `Average ⬆️`, HubLikes = `Hub ❤️`,Params = "#Params (B)") -> data

# Remove spaces from colnames
colnames(data) <- gsub(" ", "", colnames(data))

# Extract date of creation and last modification while saving exact time in new column
data |> mutate(created_at_time = created_at, last_modified_time = last_modified) -> data
data |> mutate(created_at = as.Date(created_at), last_modified = as.Date(last_modified)) -> data

# Remove emoji and empty space at the beginning of TypeString
data |> mutate(TypeString = str_sub(TypeString, 3 , length(TypeString))) -> data

# Arrange authors by number of repositories
author.activity |> arrange(desc(num_of_repos)) -> author.activity 

# Extract important architectures that have at least 50 instances in the dataset
data$Architecture |> tolower() |> table() |> sort() |> as.data.frame() |> arrange(desc(Freq)) |> dplyr::filter(Freq > 50 & Var1 != "?" & Var1 != "unknown") |> pull(Var1) |> droplevels() |> as.character()-> important_architectures

# Create new column Architecture_new that contains only important architectures all other are grouped under "Other"
data |> mutate(Architecture_new = Architecture |> tolower()) |> mutate(Architecture_new = ifelse(Architecture_new %in% important_architectures, Architecture_new, "Other")) -> data

# Rename some architectures to more readable names
data <- data |> mutate(Architecture_new = case_when(
  Architecture_new == "llamaforcausallm" ~ "LLama", 
  Architecture_new == "mixtralforcausallm" ~ "Mixtral",
  Architecture_new == "qwen2forcausallm" ~ "Qwen2",
  Architecture_new == "mistralforcausallm" ~ "Mistral",
  Architecture_new == "gemmaforcausallm" ~ "Gemma",
  Architecture_new == "phiforcausallm" ~ "Phi",
  Architecture_new == "optforcausallm" ~ "Opt",
  Architecture_new == "gpt2lmheadmodel" ~ "GPT2",
  Architecture_new == "gptneoxforcausallm" ~ "GPT-NeoX",
  TRUE ~ "Other"
))

# Extract date while keeping exact time of first and last commit
commit.data |> mutate(first_commit_time = first_commit, last_commit_time = last_commit, first_commit = as.Date(first_commit), last_commit = as.Date(last_commit)) -> commit.data

# Total days between first and last commit
commit.data |> mutate(total_time_days = as.numeric(difftime(as.Date(last_commit), as.Date(first_commit), units = "days"))) -> commit.data

# Create  numeric value for first commit so it can be used in correlation matrix
commit.data |> select(first_commit) |> mutate(first_commit = as.numeric(first_commit) - min(as.numeric(first_commit))) |> pull() -> commit.data$first_commit_numeric

data |> dplyr::filter(Flagged == F) -> data

data[data$ModelHub == "https://huggingface.co/meta-llama/Meta-Llama-3-8B",]$created_at <- as.Date("2024-04-17")
data[data$ModelHub == "https://huggingface.co/meta-llama/Meta-Llama-3-8B",]$created_at_time <- ymd_hms("2024-04-17 23:16:24")

author.activity |>dplyr::filter(str_detect(author_name, "bot$") == F) -> author.activity

# Filter data for fine-tuned models
data |> dplyr::filter(TypeString == "fine-tuned on domain-specific datasets") -> data.fine.tuned

# Filter base models that are present in fine-tuned data
base.models |> dplyr::filter(model_name_for_query %in% data.fine.tuned$model_name_for_query) -> base.models.fine.tuned

# Calculate column sums for the filtered base models (excluding the first column)
base.models.fine.tuned[,-1] |> colSums() -> colsums

# Identify columns with non-zero sums
colsums > 0 -> idx
idx <- c(TRUE, idx)  # Keep the first column (model names)

# Keep only the columns with non-zero sums
base.models.fine.tuned[,idx] -> base.models.fine.tuned

# Calculate row sums again after filtering columns
base.models.fine.tuned[,-1] |> rowSums() -> rowsums

# Keep only the rows where the row sum is 1 (indicating a unique mapping)
base.models.fine.tuned[which(rowsums == 1),] -> base.models.fine.tuned

# Initialize an empty list to store models and their corresponding base models
models.and.base.models.list <- list()

# Loop through each row to extract the base model names
for (i in 1:nrow(base.models.fine.tuned)){
  base.models.fine.tuned[i, which(base.models.fine.tuned[i, 1:ncol(base.models.fine.tuned)] != 0)] |> colnames() |> (\(x) x[-1])() -> names
  
  models.and.base.models.list[[base.models.fine.tuned[i,]$model_name_for_query]] <- names
}

# Convert the list to a data frame
models.and.base.model.df <- data.frame(model_name_for_query = names(models.and.base.models.list), base_model = I(models.and.base.models.list) |> unlist())

# Reset row names
models.and.base.model.df |> rownames() <- NULL

# Display the frequency of base model occurrences
models.and.base.models.list |> sapply(length) |> table() |> as.data.frame() |> arrange(desc(Freq)) |> select(-1)

# Save the data frame to a CSV file
models.and.base.model.df |> write.csv("../data/models_and_base_models.csv")

# Calculate the maximum average for fine-tuned models and arrange them
data |> dplyr::filter(TypeString == "fine-tuned on domain-specific datasets" & model_name_for_query %in% names(models.and.base.models.list)) |> group_by(model_name_for_query) |> summarise(base_model, Average = max(Average)) |> arrange(Average) |> unique() -> average.fine.tuned.df

# Merge the model and base model data with the average fine-tuned data
models.and.base.model.df |> inner_join(average.fine.tuned.df, by = c("model_name_for_query")) |> select(model_name_for_query, base_model = base_model.x, Average) -> fine.tuned.models.and.base.model.df

# Calculate the maximum average for base models and arrange them
data |> dplyr::filter(model_name_for_query %in% fine.tuned.models.and.base.model.df$base_model) |> group_by(model_name_for_query) |> reframe(base_model = model_name_for_query, Average_base_model = max(Average)) |> arrange(Average_base_model) |> select(-model_name_for_query) -> average.base.models.df

# Display the merged data frame of fine-tuned models and base models
fine.tuned.models.and.base.model.df |> as.tibble()

# Merge the fine-tuned models data with base model averages and arrange by difference in averages
fine.tuned.models.and.base.model.df |> inner_join(average.base.models.df, by = c("base_model")) |> group_by(model_name_for_query, base_model) |> reframe(Average = max(Average), Average_base_model) |> arrange(Average_base_model - Average) |> unique() -> fine.tuned.models.and.base.model.df

# Save the final data frame to a CSV file
write.csv(fine.tuned.models.and.base.model.df, "../data/finetuned_models_and_base_models_with_scores.csv")

# Group the final data frame by model name for further analysis
fine.tuned.models.and.base.model.df |> group_by(model_name_for_query)

# Filter data for chat models
data |> dplyr::filter(TypeString == "chat models (RLHF, DPO, IFT, ...)") -> data.chat.model

# Filter base models that are present in chat model data
base.models |> dplyr::filter(model_name_for_query %in% data.chat.model$model_name_for_query) -> base.models.chat.model

# Calculate column sums for the filtered base models (excluding the first column)
base.models.chat.model[,-1] |> colSums() -> colsums

# Identify columns with non-zero sums
colsums > 0 -> idx

# Keep the first column (model names)
idx <- c(TRUE, idx)

# Keep only the columns with non-zero sums
base.models.chat.model[,idx] -> base.models.chat.model

# Calculate row sums again after filtering columns
base.models.chat.model[,-1] |> rowSums() -> rowsums

# Display the frequency of row sums
table(rowsums)

# Keep only the rows where the row sum is 1 (indicating a unique mapping)
base.models.chat.model[which(rowsums == 1),] -> base.models.chat.model

# Initialize an empty list to store models and their corresponding base models
models.and.base.models.list <- list()

# Loop through each row to extract the base model names
for (i in 1:nrow(base.models.chat.model)){
  base.models.chat.model[i, which(base.models.chat.model[i, 1:ncol(base.models.chat.model)] != 0)] |> colnames() |> (\(x) x[-1])() -> names
  
  models.and.base.models.list[[base.models.chat.model[i,]$model_name_for_query]] <- names
}

# Convert the list to a data frame
chat.models.and.base.model.df <- data.frame(model_name_for_query = names(models.and.base.models.list), base_model = I(models.and.base.models.list) |> unlist())

# Reset row names
chat.models.and.base.model.df |> rownames() <- NULL

# Display the frequency of base model occurrences
models.and.base.models.list |> sapply(length) |> table() |> as.data.frame() |> arrange(desc(Freq))

# Save the data frame to a CSV file
chat.models.and.base.model.df |> write.csv("../data/chat_models_and_base_models.csv")

# Calculate the maximum average for chat models and arrange them
data |> dplyr::filter(TypeString == "chat models (RLHF, DPO, IFT, ...)" & model_name_for_query %in% names(models.and.base.models.list)) |> group_by(model_name_for_query) |> reframe(base_model, Average = max(Average)) |> unique() |> arrange(Average) -> average.chat.model.df

# Merge the model and base model data with the average chat model data
chat.models.and.base.model.df |> inner_join(average.chat.model.df, by = c("model_name_for_query")) |> select(model_name_for_query, base_model = base_model.x, Average) -> chat.models.and.base.model.df

# Calculate the maximum average for base models and arrange them
data |> dplyr::filter(model_name_for_query %in% chat.models.and.base.model.df$base_model) |> group_by(model_name_for_query) |> reframe(base_model = model_name_for_query, Average_base_model = max(Average)) |> arrange(Average_base_model) |> select(-model_name_for_query) -> chat.average.base.models.df

# Merge the chat models data with base model averages and arrange by difference in averages
chat.models.and.base.model.df |> inner_join(chat.average.base.models.df, by = c("base_model")) |> group_by(model_name_for_query, base_model) |> reframe(Average = max(Average), Average_base_model) |> arrange(Average_base_model - Average) |> unique() -> chat.models.and.base.model.df

# Save the final data frame to a CSV file
write.csv(chat.models.and.base.model.df, "../data/chat_models_and_base_models_with_scores.csv")

# Group the final data frame by model name for further analysis
chat.models.and.base.model.df |> group_by(model_name_for_query)

# Filter dataset maps for fine-tuned models
dataset.maps |> dplyr::filter(model_name_for_query %in% fine.tuned.models.and.base.model.df$model_name_for_query) -> dataset.finetuned

# Calculate column sums for the filtered datasets (excluding the first column)
dataset.finetuned[,-1] |> colSums() -> colsums1

# Identify columns with non-zero sums
colsums1 > 0 -> idx1

# Keep the first column (model names)
idx1 <- c(TRUE, idx1) 

# Calculate row sums for the filtered datasets (excluding the first column)
dataset.finetuned[,-1] |> rowSums()

# Keep only the columns with non-zero sums
dataset.finetuned[,idx1] -> dataset.finetuned

# Calculate row sums again after filtering columns
dataset.finetuned[,-1] |> rowSums() -> rowsums1

# Display the filtered dataset
dataset.finetuned

# Initialize an empty list to store models and their corresponding datasets
dataset.finetuned.list <- list()

# Loop through each row to extract the dataset names
for (i in 1:nrow(dataset.finetuned)){
  dataset.finetuned[i, which(dataset.finetuned[i, 1:ncol(dataset.finetuned)] != 0)] |> colnames() |> (\(x) x[-1])() |> paste(collapse = ",") -> names
  
  dataset.finetuned.list[[dataset.finetuned[i,]$model_name_for_query]] <- names
}

# Convert the list to a data frame
fine.tuned.models.and.datasets.df <- data.frame(model_name_for_query = names(dataset.finetuned.list), datasets = I(dataset.finetuned.list) |> unlist())

# Reset row names
fine.tuned.models.and.datasets.df |> rownames() <- NULL

# Save the data frame to a CSV file
fine.tuned.models.and.datasets.df |> write.csv("../data/fine.tuned.models.and.datasets.csv")

# Merge the fine-tuned model and base model data with the dataset data
fine.tuned.models.and.base.model.df |> inner_join(fine.tuned.models.and.datasets.df, by = c("model_name_for_query")) -> complete.df.finetuned

# Calculate the average difference and create a histogram
complete.df.finetuned |> mutate(Average_diff = Average - Average_base_model) |> ggplot(aes(x = Average_diff)) + geom_histogram(binwidth = 1) + labs(title = "Average difference", x = "Average difference", y = "Count") + scale_x_continuous(breaks = seq(-1000, 2222, 1))

# Summarize average differences per dataset and create a bar plot
complete.df.finetuned |> group_by(datasets) |> summarise(n = n(), average_diff = mean(Average - Average_base_model)) |> arrange(desc(average_diff)) |> dplyr::filter(n > 2) |> ggplot(aes(x = datasets, y = average_diff)) + geom_bar(stat = "identity") + labs(title = "Average difference per dataset", x = "Dataset", y = "Average difference") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Add a column for the number of datasets and save the updated data frame to a CSV file
complete.df.finetuned |> mutate(number_of_datasets = str_count(datasets, ",") + 1, Average_diff = Average - Average_base_model) -> complete.df.finetuned
complete.df.finetuned |> write.csv("../data/complete_df_finetuned.csv")

# Summarize the data by the number of datasets
complete.df.finetuned |> group_by(number_of_datasets) |> summarise(n = n())

# Check correlation between change in Average score and number of datasets
complete.df.finetuned |> select(Average_diff, number_of_datasets) |> cor()

# Filter dataset maps for chat models
dataset.maps |> dplyr::filter(model_name_for_query %in% chat.models.and.base.model.df$model_name_for_query) -> dataset.chat

# Calculate column sums for the filtered datasets (excluding the first column)
dataset.chat[,-1] |> colSums() -> colsums2

# Identify columns with non-zero sums
colsums2 > 0 -> idx2

# Keep the first column (model names)
idx2 <- c(TRUE, idx2)

# Calculate row sums for the filtered datasets (excluding the first column)
dataset.chat[,-1] |> rowSums()

# Keep only the columns with non-zero sums
dataset.chat[,idx2] -> dataset.chat

# Calculate row sums again after filtering columns
dataset.chat[,-1] |> rowSums() -> rowsums2

# Display the filtered dataset
dataset.chat

# Initialize an empty list to store models and their corresponding datasets
dataset.chat.list <- list()

# Loop through each row to extract the dataset names
for (i in 1:nrow(dataset.chat)){
  dataset.chat[i, which(dataset.chat[i, 1:ncol(dataset.chat)] != 0)] |> colnames() |> (\(x) x[-1])() |> paste(collapse = ",") -> names
  
  dataset.chat.list[[dataset.chat[i,]$model_name_for_query]] <- names
}

# Convert the list to a data frame
chat.models.and.datasets.df <- data.frame(model_name_for_query = names(dataset.chat.list), datasets = I(dataset.chat.list) |> unlist())

# Reset row names
chat.models.and.datasets.df |> rownames() <- NULL

# Save the data frame to a CSV file
chat.models.and.datasets.df |> write.csv("../data/chat.models.and.datasets.csv")

# Merge the chat model and base model data with the dataset data
chat.models.and.base.model.df |> inner_join(chat.models.and.datasets.df, by = c("model_name_for_query")) -> complete.df.chat

# Calculate the average difference and create a histogram
complete.df.chat |> mutate(Average_diff = Average - Average_base_model) |> ggplot(aes(x = Average_diff)) + geom_histogram(binwidth = 1) + labs(title = "Average difference", x = "Average difference", y = "Count") + scale_x_continuous(breaks = seq(-1000, 2222, 1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Summarize average differences per dataset and create a bar plot
complete.df.chat |> group_by(datasets) |> summarise(n = n(), average_diff = mean(Average - Average_base_model)) |> arrange(desc(average_diff)) |> dplyr::filter(n > 2) |> ggplot(aes(x = datasets, y = average_diff)) + geom_bar(stat = "identity") + labs(title = "Average difference per dataset", x = "Dataset", y = "Average difference") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Add a column for the number of datasets and save the updated data frame to a CSV file
complete.df.chat |> mutate(number_of_datasets = str_count(datasets, ",") + 1, Average_diff = Average - Average_base_model) -> complete.df.chat
complete.df.chat |> write.csv("../data/complete_df_chat.csv")

# Summarize average differences per dataset and create a bar plot (duplicate, appears intentional)
complete.df.chat |> group_by(datasets) |> summarise(n = n(), average_diff = mean(Average - Average_base_model)) |> arrange(desc(average_diff)) |> dplyr::filter(n > 2) |> ggplot(aes(x = datasets, y = average_diff)) + geom_bar(stat = "identity") + labs(title = "Average difference per dataset", x = "Dataset", y = "Average difference") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Identify unique datasets that are present in both chat and fine-tuned models
complete.df.chat$datasets[complete.df.chat$datasets %in% complete.df.finetuned$datasets] |> unique()

# Add a new column 'Type' with value 'chat' to the complete.df.chat data frame and 'finetuned' to the complete.df.finetuned data frame
# Bind rows of both to create final data frame
complete.df.chat |> mutate(Type = "chat") |> bind_rows(complete.df.finetuned |> mutate(Type = "finetuned")) -> complete.chat.finetuned.df

# Save the combined data frame to a CSV file
complete.chat.finetuned.df |> write.csv("../data/complete_chat_finetuned.csv")

author.activity |> select(author_name, num_of_repos) |> head(400) |> 
  gt()

author.activity |> select(author_name, num_of_repos) |> head(10) |> pull(num_of_repos) |> sum()

author.activity |> select(author_name, num_of_repos) |> pull(num_of_repos) |> sum()

hist(author.activity$num_of_repos, breaks = 100, main = "Number of Repositories per Author", xlab = "Number of Repositories", ylab = "Count", col = "lightblue")

author.activity |> ggplot(aes(x = num_of_repos)) + geom_histogram(binwidth = 2) + labs(title = "Number of Repositories per Author", x = "Number of Repositories", y = "Count") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

author.activity |> nrow()


commit.data$num_of_authors |> hist(main = "Number of Authors per Repository", xlab = "Number of Authors", ylab = "Count", col = "lightblue")

data$Architecture_new |> table() |> as.data.frame() |> gt()



# Plot a histogram of the Average variable with a bin width of 0.3
data |> ggplot(aes(x = Average)) + geom_histogram(binwidth = 0.3) + labs(title = "Average Distribution", x = "Average", y = "Count")

# Print summary statistics of the data
summary(data)

# Calculate the correlation matrix for selected variables and plot it using ggcorrplot
data |> select(Average, HubLikes, downloads, Params) |> na.omit() |> cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", outline.color = "black", lab = TRUE) + 
  labs(title = "Correlation Plot for All Data")

# Calculate the correlation matrix for the top 1000 rows (by Average) and plot it using ggcorrplot
data |> arrange(desc(Average)) |> head(1000) |> select(Average, HubLikes, downloads, Params) |> na.omit() |> cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", outline.color = "black", lab = TRUE) + 
  labs(title = "Correlation Plot for Top 1000 Data")

# Calculate the correlation matrix for the top 100 rows (by Average) and plot it using ggcorrplot
data |> arrange(desc(Average)) |> head(100) |> select(Average, HubLikes, downloads, Params) |> na.omit() |> cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", outline.color = "black", lab = TRUE) + 
  labs(title = "Correlation Plot for Top 100 Data")

# Calculate the correlation matrix for the top 50 rows (by Average) and plot it using ggcorrplot
data |> arrange(desc(Average)) |> head(50) |> select(Average, HubLikes, downloads, Params) |> na.omit() |> cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower", outline.color = "black", lab = TRUE) + 
  labs(title = "Correlation Plot for Top 50 Data")

# Select the model_name_for_query and Average columns from the data
data |> select(model_name_for_query, Average) |> 
  # Perform an inner join with dataset.maps on the model_name_for_query column
  inner_join(dataset.maps, by = c("model_name_for_query")) |> 
  # Remove the model_name_for_query column
  select(-model_name_for_query) |> 
  # Calculate the correlation matrix
  cor() -> cor.score.dataset

# Set the margins for the plot
par(mar = c(12, 5, 4, 5))

# Sort the correlations with the Average column (excluding the first row, which is Average itself),
# take the top 10 correlations, and create a bar plot
cor.score.dataset[2:nrow(cor.score.dataset),1] |> 
  sort(decreasing = TRUE) |> 
  head(10) |> 
  barplot(main = "Correlation with Average", col = "blue", las = 2, cex.names = 0.6)


# Set the margins for the plot
par(mar = c(10, 5, 4, 5))

# Calculate the column sums of dataset.maps excluding the first column,
# sort the sums in decreasing order, convert to a data frame, and select the top 100 datasets
dataset.maps[,-1] |> colSums() |> sort(decreasing = TRUE) |> data.frame() |> head(100) -> top100.datasets

# Rename the column to "count"
colnames(top100.datasets) <- "count"

# Create a bar plot of the top 100 datasets
top100.datasets |> ggplot(aes(x = rownames(top100.datasets), y = count)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Top 100 Datasets", x = "Dataset", y = "Count")

# Save the plot as a PDF
ggsave("../out/top100datasets.pdf", width = 20, height = 10)


# Select the model_name_for_query and Average columns from the data
data |> select(model_name_for_query, Average) |> 
  # Perform an inner join with base.models on the model_name_for_query column
  inner_join(base.models, by = c("model_name_for_query"))  |> 
  # Remove the model_name_for_query column
  select(-model_name_for_query) |> 
  # Calculate the correlation matrix
  cor() -> cor.score.base.model

# Set the margins for the plot
par(mar = c(10, 5, 4, 5))

# Sort the correlations with the Average column (excluding the first row, which is Average itself),
# take the top 10 correlations, and create a bar plot
cor.score.base.model[2:nrow(cor.score.base.model),1] |> 
  sort(decreasing = TRUE) |> 
  head(10) |> 
  barplot(main = "Correlation Base model with Average", col = "blue", las = 2, cex.names = 0.6)


# Select the first 100 rows from data, then select model_name_for_query and Average columns
# Perform an inner join with commit.data by matching model_name_for_query to name
# Select Average, num_of_authors, and num_of_commits columns and calculate the correlation matrix
data[1:100,] |> select(model_name_for_query, Average) |> 
  inner_join(commit.data, by = c("model_name_for_query" = "name")) |> 
  select(Average, num_of_authors, num_of_commits) |> 
  cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE,
                         type = "lower",
                         outline.color = "black",
                         lab = TRUE) + 
  labs(title = "Correlation Plot for Top 100 Data")

# Group data by Architecture_new, calculate the mean of Average for each group, and arrange in descending order
data |> group_by(Architecture_new) |> 
  summarise(Average = mean(Average)) |> 
  arrange(desc(Average))

# Group data by TypeString, calculate the mean of Average for each group, and arrange in descending order
data |> group_by(TypeString) |> 
  summarise(Average = mean(Average)) |> 
  arrange(desc(Average))

# Split the 'authors' column by comma and unnest the resulting list
# Remove square brackets and single quotes from the 'authors' column
commit.data |> 
  mutate(authors = strsplit(as.character(authors), ",\\s*")) |> 
  unnest(authors) |> 
  mutate(authors = gsub("\\[|\\]|'", "", authors)) -> commit.data.long

# Calculate the correlation matrix between 'num_of_commits' and 'total_time_days' columns in commit.data.long
# Plot the correlation matrix using ggcorrplot with a title
commit.data.long |> select(num_of_commits, total_time_days) |> cor() |> 
  ggcorrplot::ggcorrplot(hc.order = TRUE,
                         type = "lower",
                         outline.color = "black",
                         lab = TRUE) +
  labs(title = "Correlation Plot: Number of Commits and Total Time in Days") +
  theme(plot.title = element_text(hjust = 0.5))

# Group commit.data.long by authors and calculate summary statistics for each author
# Calculate the total number of models, average number of commits, average total time in days, and average number of authors
# Arrange the results in descending order of total_models
commit.data.long |> group_by(authors) |> 
  summarise(total_models = n(), 
            average_num_of_commits = mean(num_of_commits), 
            average_total_time_days = mean(total_time_days), 
            average_num_of_authors = mean(num_of_authors)) |> 
  arrange(desc(total_models))


# Select the 'authors' and 'first_commit' columns from commit.data and arrange the data by 'first_commit'
commit.data |> select(authors, first_commit) |> arrange(first_commit)

# Group commit.data.long by authors and calculate the number of commits for each author
# Arrange the result in descending order and select the top 100 authors
commit.data.long |> group_by(authors) |> summarise(num_of_commits = n()) |> arrange(desc(num_of_commits)) |> head(100)

# Group commit.data.long by authors and find the earliest commit date for each author
commit.data.long |> group_by(authors) |> summarise(first_commit = min(as.Date(first_commit))) -> first.commit.of.each.author

# Group data by 'created_at' and calculate the mean of 'Average' for each group
# Plot Average vs. Date using ggplot, adding smooth trend lines
data |> group_by(created_at) |> summarise(Average = mean(Average)) |> ggplot(aes(x = created_at, y = Average)) + geom_point() + geom_smooth() + labs(title = "Average vs Date", x = "Date", y = "Average score")

# Group first.commit.of.each.author by 'first_commit' and count the number of new authors over time
first.commit.of.each.author |> group_by(first_commit) |> summarise(new_authors = n()) |> mutate(total_authors = cumsum(new_authors)) -> total.authors.over.time

# Keep only the first occurrence of each author and add columns for month, week, and year of the first commit
commit.data.long |> select(authors, first_commit) |> arrange(first_commit) |> distinct(authors, .keep_all = TRUE) |> mutate(month = floor_date(first_commit,"month"), week = floor_date(first_commit,"week"), year = floor_date(first_commit, "year")) -> author.list

# Plot the total number of authors over time, distinguishing between new authors and total authors
ggplot(author.list |> group_by(month) |> summarise(new_authors = n()) |> mutate(total_authors = cumsum(new_authors)), aes(x = month)) +
  geom_point(aes(y = new_authors, color = "New Authors"), size = 2, alpha = 0.7) +
  geom_point(aes(y = total_authors, color = "Total Authors"), size = 2, alpha = 0.7) +
  geom_smooth(aes(y = new_authors), color = "darkblue") +
  geom_smooth(aes(y = total_authors), color = "#b5162b") +
  scale_color_manual(values = c("New Authors" = "darkblue", "Total Authors" = "#b5162b"),
                     labels = c("New Authors", "Total Authors")) +
  labs(title = "Total and new authors over time", x = "First commit", y = "Authors", color = "Legend") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 3000, 200))

# Save the plot as a PDF
ggsave("../out/total_authors_over_time.pdf", width = 10, height = 6)

# Generate a dataframe with year and month columns from the 'created_at' column
data |> 
  mutate(year = year(created_at), month = month(created_at)) |> 
  group_by(year, month) |> 
  summarise(Average = max(Average, na.rm = TRUE), total = n(), .groups = 'drop') |> 
  mutate(date = make_date(year, month)) |> 
  ungroup() -> average.vs.total.per.month

# Define the y-intercept for placing labels
y.intercept <- 1000

# Create a plot showing the total models per month with notable model release dates
average.vs.total.per.month |> 
  ggplot(aes(x = date + days(15), y = total, fill = Average)) + 
  geom_col() + 
  labs(title = "Total models per month", x = "Month", y = "Total models") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  
  #MODEL RELEASE DATES
  #LAMMA
  geom_vline(xintercept = as.Date("2024-04-18"), linetype = "dashed", color = "red") +
  geom_text(aes(x = as.Date("2024-04-18"), y = y.intercept, label = "LLama3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "red") +
  geom_vline(xintercept = as.Date("2023-07-18"), linetype = "dashed", color = "blue") +
  geom_text(aes(x = as.Date("2023-07-18"), y = y.intercept, label = "LLama2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "blue") +
  geom_vline(xintercept = as.Date("2023-02-24"), linetype = "dashed", color = "darkgreen") +
  geom_text(aes(x = as.Date("2023-02-24"), y = y.intercept, label = "LLama1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "darkgreen") +
  #GEMINI
  geom_vline(xintercept = as.Date("2023-12-6"), linetype = "dashed", color = "black") +
  geom_text(aes(x = as.Date("2023-12-6"), y = y.intercept, label = "Gemini1.0"), angle = 90, hjust = 0.8, vjust = -0.8, color = "black") +
  geom_vline(xintercept = as.Date("2024-02-15"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2024-02-15"), y = y.intercept, label = "Gemini1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  #MISTRAL
  geom_vline(xintercept = as.Date("2023-9-20"), linetype = "dashed", color = "orange") +
  geom_text(aes(x = as.Date("2023-9-20"), y = y.intercept, label = "Mistral7b-v0.1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "orange") +
  geom_vline(xintercept = as.Date("2024-5-22"), linetype = "dashed", color = "green") +
  geom_text(aes(x = as.Date("2024-5-22"), y = y.intercept, label = "Mistral7b-v0.3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "green") +
  geom_vline(xintercept = as.Date("2024-4-10"), linetype = "dashed", color = "pink") +
  geom_text(aes(x = as.Date("2024-4-10"), y = y.intercept, label = "Mixtral 8x22B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "pink") +
  geom_vline(xintercept = as.Date("2023-12-11"), linetype = "dashed", color = "lightgreen") +
  geom_text(aes(x = as.Date("2023-12-11"), y = y.intercept, label = "Mistral7b-v0.2 and Mixtral 8x7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightgreen") + 
  #PHI
  geom_vline(xintercept = as.Date("2023-10-17"), linetype = "dashed", color = "cyan") +
  geom_text(aes(x = as.Date("2023-10-17"), y = y.intercept, label = "Phi1 and Phi1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "cyan") +
  geom_vline(xintercept = as.Date("2024-1-11"), linetype = "dashed", color = "grey") +
  geom_text(aes(x = as.Date("2024-1-11"), y = y.intercept, label = "Phi2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "grey")+
  geom_vline(xintercept = as.Date("2024-4-22"), linetype = "dashed", color = "lightblue") +
  geom_text(aes(x = as.Date("2024-4-22"), y = y.intercept, label = "Phi3-mini"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightblue")+
  #FALCON
  geom_vline(xintercept = as.Date("2023-4-24"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2023-4-24"), y = y.intercept, label = "Falcon-7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  #BLOOM
  geom_vline(xintercept = as.Date("2022-10-21"), linetype = "dashed", color = "darkgrey") +
  geom_text(aes(x = as.Date("2022-10-21"), y = y.intercept, label = "Bloom"), angle = 90, hjust = 0.8, vjust = -0.8, color = "darkgrey")

# Save the plot as a PDF
ggsave("../out/total_models_per_month.pdf", width = 30, height = 8)

# Set the y-intercept for placing labels
y.intercept <- 100

# Create a plot showing the total models per day with notable model release dates  
data |> 
  dplyr::filter(!is.na(created_at) & created_at > "2022-10-1") |> 
  group_by(created_at) |>
  summarise(Average = max(Average, na.rm = TRUE), total = n(), .groups = 'drop') |> 
  ggplot(aes(x = created_at, y = total, fill = Average)) +
  geom_col() + 
  geom_smooth() +
  labs(title = "Total models per date", x = "Date", y = "Total models per date") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #MODEL RELEASE DATES
  #LAMMA
  geom_vline(xintercept = as.Date("2024-04-18"), linetype = "dashed", color = "red") +
  geom_text(aes(x = as.Date("2024-04-18"), y = y.intercept, label = "LLama3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "red") +
  geom_vline(xintercept = as.Date("2023-07-18"), linetype = "dashed", color = "blue") +
  geom_text(aes(x = as.Date("2023-07-18"), y = y.intercept, label = "LLama2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "blue") +
  geom_vline(xintercept = as.Date("2023-02-24"), linetype = "dashed", color = "darkgreen") +
  geom_text(aes(x = as.Date("2023-02-24"), y = y.intercept, label = "LLama1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "darkgreen") +
  #GEMINI
  geom_vline(xintercept = as.Date("2023-12-6"), linetype = "dashed", color = "black") +
  geom_text(aes(x = as.Date("2023-12-6"), y = y.intercept, label = "Gemini1.0"), angle = 90, hjust = 0.8, vjust = -0.8, color = "black") +
  geom_vline(xintercept = as.Date("2024-02-15"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2024-02-15"), y = y.intercept, label = "Gemini1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  #MISTRAL
  geom_vline(xintercept = as.Date("2023-9-20"), linetype = "dashed", color = "orange") +
  geom_text(aes(x = as.Date("2023-9-20"), y = y.intercept, label = "Mistral7b-v0.1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "orange") +
  geom_vline(xintercept = as.Date("2024-5-22"), linetype = "dashed", color = "green") +
  geom_text(aes(x = as.Date("2024-5-22"), y = y.intercept, label = "Mistral7b-v0.3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "green") +
  geom_vline(xintercept = as.Date("2024-4-10"), linetype = "dashed", color = "pink") +
  geom_text(aes(x = as.Date("2024-4-10"), y = y.intercept, label = "Mixtral 8x22B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "pink") +
  geom_vline(xintercept = as.Date("2023-12-11"), linetype = "dashed", color = "lightgreen") +
  geom_text(aes(x = as.Date("2023-12-11"), y = y.intercept, label = "Mistral7b-v0.2 and Mixtral 8x7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightgreen") + 
  #PHI
  geom_vline(xintercept = as.Date("2023-10-17"), linetype = "dashed", color = "cyan") +
  geom_text(aes(x = as.Date("2023-10-17"), y = y.intercept, label = "Phi1 and Phi1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "cyan") +
  geom_vline(xintercept = as.Date("2024-1-11"), linetype = "dashed", color = "grey") +
  geom_text(aes(x = as.Date("2024-1-11"), y = y.intercept, label = "Phi2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "grey")+
  geom_vline(xintercept = as.Date("2024-4-22"), linetype = "dashed", color = "lightblue") +
  geom_text(aes(x = as.Date("2024-4-22"), y = y.intercept, label = "Phi3-mini"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightblue")+
  #FALCON
  geom_vline(xintercept = as.Date("2023-4-24"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2023-4-24"), y = y.intercept, label = "Falcon-7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  #BLOOM
  geom_vline(xintercept = as.Date("2022-10-21"), linetype = "dashed", color = "darkgrey") +
  geom_text(aes(x = as.Date("2022-10-21"), y = y.intercept, label = "Bloom"), angle = 90, hjust = 0.8, vjust = -0.8, color = "darkgrey")+
    scale_fill_binned(type = "viridis")
  
  ggsave("../out/total_models_per_date.pdf", width = 25, height = 8)

# Generate a dataframe with year and month columns from the 'created_at' column
data |>
  dplyr::filter(Params > 0) |>
  mutate(year = year(created_at), month = month(created_at), Average.vs.Params = Average/Params, Params.vs.Average = Params/Average) |>
  group_by(year, month) |>
  summarise(Average.vs.Params = max(Average.vs.Params, na.rm = TRUE), Params.vs.Average = min(Params.vs.Average, na.rm = TRUE), total = n(), .groups = 'drop') |>
  mutate(date = make_date(year, month)) |>
  ungroup() -> average.vs.params.per.month

# Filter months with at least 100 total models and create a plot showing the average/params per month
average.vs.params.per.month |>
  dplyr::filter(total >= 30) |>
  ggplot(aes(x = date, y = Average.vs.Params, fill = total)) + 
  geom_col(position = "dodge", linewidth = 2) + 
  labs(title = "Average/Params by Month", x = "Month", y = "Average/Params") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = expansion(mult = c(0, 0))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# y-intercept for placing labels
y.intercept <- 0.04

# Filter months with at least 30 total models and create a plot showing the params/average per month with notable dates of release of models
average.vs.params.per.month |>
  dplyr::filter(total >= 30) |>
  ggplot(aes(x = date + days(15), y = Params.vs.Average, fill = total)) + 
  geom_col(position = "dodge") + 
  labs(title = "Params/Average by Month", x = "Month", y = "Params/Average") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",expand = expansion(mult = c(0, 0))) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #LAMMA
  geom_vline(xintercept = as.Date("2024-04-18"), linetype = "dashed", color = "red") +
  geom_text(aes(x = as.Date("2024-04-18"), y = y.intercept, label = "LLama3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "red") +
  geom_vline(xintercept = as.Date("2023-07-18"), linetype = "dashed", color = "blue") +
  geom_text(aes(x = as.Date("2023-07-18"), y = y.intercept, label = "LLama2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "blue") +
  #GEMINI
  geom_vline(xintercept = as.Date("2023-12-6"), linetype = "dashed", color = "black") +
  geom_text(aes(x = as.Date("2023-12-6"), y = y.intercept, label = "Gemini1.0"), angle = 90, hjust = 0.8, vjust = -0.8, color = "black") +
  geom_vline(xintercept = as.Date("2024-02-15"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2024-02-15"), y = y.intercept, label = "Gemini1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  #MISTRAL
  geom_vline(xintercept = as.Date("2023-9-27"), linetype = "dashed", color = "orange") +
  geom_text(aes(x = as.Date("2023-9-27"), y = y.intercept, label = "Mistral7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "orange") +
  geom_vline(xintercept = as.Date("2024-4-10"), linetype = "dashed", color = "pink") +
  geom_text(aes(x = as.Date("2024-4-10"), y = y.intercept, label = "Mixtral 8x22B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "pink") +
  geom_vline(xintercept = as.Date("2023-12-11"), linetype = "dashed", color = "lightgreen") +
  geom_text(aes(x = as.Date("2023-12-11"), y = y.intercept, label = "Mixtral 8x7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightgreen") + 
  #PHI
  geom_vline(xintercept = as.Date("2023-10-17"), linetype = "dashed", color = "cyan") +
  geom_text(aes(x = as.Date("2023-10-17"), y = y.intercept, label = "Phi1 and Phi1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "cyan") +
  geom_vline(xintercept = as.Date("2024-1-11"), linetype = "dashed", color = "grey") +
  geom_text(aes(x = as.Date("2024-1-11"), y = y.intercept, label = "Phi2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "grey")+
  geom_vline(xintercept = as.Date("2024-4-22"), linetype = "dashed", color = "lightblue") +
  geom_text(aes(x = as.Date("2024-4-22"), y = y.intercept, label = "Phi3-mini"), angle = 90, hjust = 0.8, vjust = -0.8, color = "lightblue")+
  #FALCON
  geom_vline(xintercept = as.Date("2023-4-24"), linetype = "dashed", color = "purple") +
  geom_text(aes(x = as.Date("2023-4-24"), y = y.intercept, label = "Falcon-7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "purple") +
  scale_fill_binned(type = "gradient")       

# Save plot as PDF
ggsave("../out/params.vs.average.per.month.pdf", width = 30, height = 12)

# y-intercept for placing labels
y.intercept <- 250

# Create a plot showing the total models per week with notable model release dates
data |> 
  select(Average, Params, created_at, Architecture_new) |> 
  na.omit() |> 
  arrange(created_at) |> 
  dplyr::filter(Params > 0 & created_at >= "2023-01-01") |> 
  mutate(week_start = floor_date(created_at, "week")) |> 
  group_by(week_start,Architecture_new) |> 
  summarise(Average = max(Average, na.rm = TRUE), total = n(), .groups = 'drop') |> 
  ggplot(aes(x = week_start, y = total, fill = Architecture_new)) +
  geom_col() + 
  labs(title = "Total models per date", x = "Date", y = "Total models per date") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  #MODEL RELEASE DATES
  #LAMMA
  geom_vline(xintercept = as.Date("2024-04-18"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-04-18"), y = y.intercept, label = "LLama3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2023-07-18"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-07-18"), y = y.intercept, label = "LLama2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2023-02-24"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-02-24"), y = y.intercept, label = "LLama1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  #GEMINI
  geom_vline(xintercept = as.Date("2023-12-6"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-12-6"), y = y.intercept, label = "Gemini1.0"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2024-02-15"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-02-15"), y = y.intercept, label = "Gemini1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  #MISTRAL
  geom_vline(xintercept = as.Date("2023-9-20"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-9-20"), y = y.intercept, label = "Mistral7b-v0.1"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2024-5-22"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-5-22"), y = y.intercept, label = "Mistral7b-v0.3"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2024-4-10"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-4-10"), y = y.intercept, label = "Mixtral 8x22B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2023-12-11"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-12-11"), y = y.intercept, label = "Mistral7b-v0.2 and Mixtral 8x7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) + 
  #PHI
  geom_vline(xintercept = as.Date("2023-10-17"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-10-17"), y = y.intercept, label = "Phi1 and Phi1.5"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  geom_vline(xintercept = as.Date("2024-1-11"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-1-11"), y = y.intercept, label = "Phi2"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3)+
  geom_vline(xintercept = as.Date("2024-4-22"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2024-4-22"), y = y.intercept, label = "Phi3-mini"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3)+
  #FALCON
  geom_vline(xintercept = as.Date("2023-4-24"), linetype = "dashed", color = "#5d6066") +
  geom_text(aes(x = as.Date("2023-4-24"), y = y.intercept, label = "Falcon-7B"), angle = 90, hjust = 0.8, vjust = -0.8, color = "#5d6066", size = 3) +
  scale_fill_brewer(palette = color_palette)

# Save the plot as a PDF
ggsave("../out/total_models_per_week.jpg", width = 30, height = 10)

release_dates <- data.frame(
        date = as.Date(c("2024-04-18", "2023-07-18", "2023-02-24", "2023-12-06",
                         "2024-02-15", "2023-09-20", "2024-05-22", "2024-04-10",
                         "2023-12-11", "2023-10-17", "2024-01-11", "2024-04-22",
                         "2023-04-24")),
        label = c("LLama3", "LLama2", "LLama1", "Gemini1.0", 
                  "Gemini1.5","Mistral7b-v0.1", "Mistral7b-v0.3", "Mixtral 8x22B",
                  "Mistral7b-v0.2 and Mixtral 8x7B", "Phi1 and Phi1.5","Phi2", "Phi3-mini",                     "Falcon-7B")
      )

# Create a dataframe with the total number of new models and total models per day
data |> 
  mutate(week_start = floor_date(created_at, "week")) |>
  mutate(TypeString = ifelse(is.na(TypeString), "unknown", TypeString)) |>
  group_by(TypeString, week_start) |> 
  summarise(new_models = n()) |> 
  mutate(total_models = cumsum(new_models)) |> 
  group_by(week_start) |> 
  summarise(TypeString, new_models, total_models, total_new_models_that_week = sum(new_models), .groups = "drop") -> total.models.vs.date.df
# Plot new models vs. date, grouped by TypeString, with a title, limited y-axis, and customized theme
# Scale x-axis to display dates from January 1, 2023, to June 1, 2024, with monthly breaks
# Manually set fill colors for different TypeString categories
(total.models.vs.date.df |> 
  dplyr::filter(!is.na(TypeString)) |> 
  ggplot(aes(x = week_start, y = new_models, fill = TypeString)) + 
  geom_col(color = "black", linewidth = 0.15) + 
  labs(title = "New models per week", x = "", y = "Week") + 
  theme(legend.position = "none", axis.text.x = element_blank(), panel.background = element_blank(), panel.grid = element_blank()) + 
  scale_x_date(date_labels = "%Y-%m-%d", limits = c(as.Date("2023-01-01"), as.Date("2024-06-01")), breaks = seq(as.Date("2023-01-01"), as.Date("2024-06-01"), by = "1 month")) +  
  scale_fill_brewer(palette = "Set2") -> p1) 


# Plot percentage of new models vs. date, grouped by TypeString, with a customized theme
# Scale x-axis to display dates from January 1, 2023, to June 1, 2024, with monthly breaks
# Manually set fill colors for different TypeString categories
(total.models.vs.date.df |>
  dplyr::filter(!is.na(TypeString)) |> 
  group_by(week_start) |> 
  mutate(percentage = new_models/total_new_models_that_week) |> 
  ggplot(aes(x = week_start, y = percentage, fill = TypeString)) + 
  geom_col(color = "black", linewidth = 0.15) + 
  labs(title = "", x = "Date", y = "Percentage") + 
  theme(legend.position = "bottom", panel.background = element_blank()) + 
  scale_x_date(date_labels = "%Y-%m-%d", limits = c(as.Date("2023-01-01"), as.Date("2024-06-01")), breaks = seq(as.Date("2023-01-01"), as.Date("2024-06-01"), by = "1 month")) + 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.text = element_text(size = 8), axis.title.x = element_text(margin = margin(t = 20, 
                             r = 50,  # Right margin
                             b = 40,  # Bottom margin
                             l = 10))) -> p2)

# Arrange plots p1 and p2 vertically, save the combined plot as a PDF
gridExtra::grid.arrange(p1, p2, ncol = 1) -> p3

p1 + p2 + plot_layout(ncol = 1)

ggsave("../out/new_models_vs_date_combined.pdf", p3, width = 20, height = 16)

# Plot author activity over time, showing the evolution of author activity by TypeString
# Filter author.activity, arrange by num_of_repos, select top 10, join with data, and select relevant columns
# Group by author_name, TypeString, and created_at, then plot using ggplot
(author.activity[-1,] |> arrange(desc(num_of_repos)) |> head(10) |> 
  inner_join(data, by = c("author_name" = "author")) |> 
  select(author_name, TypeString, Average, created_at) |> 
  group_by(author_name, TypeString, created_at) |> 
  ggplot(aes(x = created_at, y = author_name, fill = TypeString, color = author_name)) + 
  geom_line(linetype = "solid", linewidth = 1.3) +  
  geom_tile(color = "black") + 
  labs(title = "Author activity", x = "", y = "Total") + 
  theme_minimal() +
  scale_color_brewer(palette = color_palette) +   # Use a color palette for line colors
  scale_fill_brewer(palette = color_palette) +   # Use a color palette for fill colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "top", axis.text.y = element_blank()) -> p11)

# Save the author activity plot as a PDF
ggsave("../out/author_activity.pdf", width = 20, height = 20)

# Plot average author activity over time, showing the average score of authors' activities
# Filter author.activity, select top 10, join with data, select relevant columns, group, and reframe data
# Plot using ggplot
(author.activity[-1,] |> head(10) |> 
  inner_join(data, by = c("author_name" = "author")) |> 
  select(author_name, TypeString, Average, created_at) |> 
  group_by(author_name, TypeString, created_at) |> 
  reframe(Average = mean(Average)) |> 
  ggplot(aes(x = created_at, y = Average, color = author_name)) + 
  geom_line(linewidth = 1.3) + 
  labs(title = "Author activity", x = "Date", y = "Average score") + 
  theme_minimal() +
  scale_color_brewer(palette = color_palette) +   # Use a color palette for line colors
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") -> p12)

# Arrange the author activity plots vertically and save as a combined PDF
gridExtra::grid.arrange(p11, p12, ncol = 1) -> p13
ggsave("../out/author_activity_combined.pdf", p13, width = 20, height = 12)

# Select unique dates and arrange them from smallest to largest
unique_dates <- sort(unique(data$created_at))

# Select every 5th date
selected_dates <- unique_dates[seq(1, length(unique_dates), by = 5)]

# Prepare filtered data for plotting
data |> 
  dplyr::filter(!is.na(TypeString)) |> 
  group_by(created_at, TypeString) |>
  summarise(total = n(), .groups = 'drop') |>
  group_by(TypeString) |>
  arrange(created_at, .by_group = TRUE) |>
  mutate(total_cumsum = cumsum(total)) |>
  ungroup() |>
  # Complete the data frame with missing dates
  complete(TypeString, created_at = seq(min(selected_dates), max(selected_dates), by = "day")) |>
  # Fill missing cumulative sums
  group_by(TypeString) |>
  fill(total_cumsum, .direction = "down") |>
  dplyr::filter(created_at %in% selected_dates) |>
  ungroup() -> filtered_data

# Plot Type vs. Average over time
(data |> dplyr::filter(!is.na(TypeString)) |> 
    ggplot(aes(x = created_at, y = Average, color = TypeString)) + 
    geom_point(alpha = 0.65, position = position_dodge2()) + 
    geom_smooth(se = FALSE, linewidth = 1.5) + 
    labs(title = "Type vs Average", x = "", y = "Average") +  
    scale_color_brewer(palette = color_palette) +
    theme(legend.position = "none", panel.background = element_blank(), panel.grid = element_blank(), axis.text.x = element_blank()) -> p_type_vs_average)

# Plot Type vs. Total combined
(filtered_data |> 
  ggplot(aes(x = created_at, y = total_cumsum, color = TypeString)) +
  geom_line(aes(group = TypeString), position = "dodge", linewidth = 1.5, alpha = 0.8) +
  labs(title = "", x = "Date", y = "Total") +
  scale_color_brewer(palette = color_palette) +
  theme(legend.position = "bottom", panel.background = element_blank(), panel.grid = element_blank()) +
  scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2023-01-01"), as.Date("2024-06-01")), breaks = seq(as.Date("2023-01-01"), as.Date("2024-06-01"), by = "1 month")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) -> p_type_vs_total)

# Arrange the plots vertically and save as a combined PDF
gridExtra::grid.arrange(p_type_vs_average, p_type_vs_total, ncol = 1) -> p_type_combined
ggsave("../out/type_combined.pdf", p_type_combined, width = 18, height = 8)

# Find common datasets between complete.df.chat and complete.df.finetuned
complete.df.chat$datasets[complete.df.chat$datasets %in% complete.df.finetuned$datasets] |> unique() -> datasets_common

# Filter and process data from complete.chat.finetuned.df
complete.chat.finetuned.df |> 
  group_by(datasets, Type) |> 
  reframe(n = n(), mean_diff = mean(Average_diff)) |> 
  arrange(desc(n)) |> 
  dplyr::filter(datasets %in% datasets_common) |> 
  dplyr::filter(n > 3) |> 
  group_by(datasets) |> 
  reframe(ntotal = sum(n >= 4)) |> 
  dplyr::filter(ntotal == 2) |>  
  pull(datasets) -> datasets_common2

# Filter and process data from complete.chat.finetuned.df
complete.chat.finetuned.df |> 
  group_by(datasets, Type) |> 
  reframe(n = n(), mean_diff = mean(Average_diff)) |> 
  arrange(desc(n)) |> 
  dplyr::filter(datasets %in% datasets_common2) |> 
  dplyr::filter(n > 3)  |> 
  ggplot(aes(x = datasets, y = mean_diff, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge2()) + 
  labs(title = "Average difference per dataset", x = "Dataset", y = "Average difference") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot as a PDF
ggsave("../out/average_diff_per_dataset.pdf", width = 5, height = 5)


# Plot for Params between 65 and 75
data |> 
  dplyr::filter(Params > 65 & Params < 75) |> 
  group_by(created_at) |>  
  reframe(Average = max(Average)) |>  
  ggplot(aes(created_at, Average)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Average vs Date for Params between 65 and 75")

# Plot for Params between 30 and 40
data |> 
  dplyr::filter(Params > 30 & Params < 40) |> 
  group_by(created_at) |>  
  reframe(Average = max(Average)) |>  
  ggplot(aes(created_at, Average)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Average vs Date for Params between 30 and 40")

# Plot for Params between 1 and 15
data |> 
  dplyr::filter(Params > 1 & Params < 15) |> 
  group_by(created_at) |>  
  reframe(Average = max(Average)) |>  
  ggplot(aes(created_at, Average)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Average vs Date for Params between 1 and 15")


# Plot Params vs Average with scatter points and smoothed line
data |> 
  group_by(Params) |>  # Group data by Params value
  summarise(n = n(), average = max(Average)) |>  # Summarise data by count and maximum average
  ggplot(aes(x = Params, y = average)) +  # Define aesthetics for the plot
  geom_point() +  # Add scatter points
  geom_smooth() +  # Add smoothed line
  labs(title = "Params vs Average", x = "Params", y = "Average")  # Set plot titles and axis labels

# Plot Params density
library(DescTools)
data |> 
  group_by(Params) |>  # Group data by Params value
  summarise(n = n(), average = max(Average)) |>  # Summarise data by count and maximum average
  ggplot(aes(Params)) +  # Define aesthetics for the plot
  geom_density(fill = "lightgreen") +  # Add density plot with green fill
  labs(title = "Params density", x = "Params", y = "Density") +  # Set plot titles and axis labels
  geom_vline(xintercept = median(data$Params), color = "red", linetype = "dashed") +  # Add vertical line for median
  geom_vline(xintercept = mean(data$Params), color = "blue", linetype = "dashed") +  # Add vertical line for mean
  geom_label(aes(x = median(data$Params), y = 0.0125, label = paste("Median =", median(data$Params))), color = "red", fill = "white") +  # Add label for median
  geom_label(aes(x = mean(data$Params), y = 0.014, label = paste("Mean =", round(mean(data$Params), 2))), color = "blue", fill = "white") +  # Add label for mean
  theme_minimal()  # Apply minimal theme

# Save Params density plot
ggsave("../out/params_density.pdf", width = 10, height = 6)


# Create histogram of Params with fill by Architecture_new
(plot_params_vs_architecture <- data |> 
  dplyr::filter(Params > 0) |>  # Filter data where Params > 0
  ggplot(aes(Params, fill=Architecture_new)) +  # Define aesthetics for the plot
  geom_histogram(binwidth = 1, color = "black", size = .1) +  # Add histogram with specified bin width and border color
  labs(title = "Params histogram", x = "Params", y = "Count") +  # Set plot titles and axis labels
  theme_minimal() +  # Apply minimal theme
  scale_x_continuous(breaks = seq(0, 300, 5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 4000, 200)) +  # Set y-axis breaks
  scale_fill_brewer(palette = color_palette) +  # Set color palette for fill
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1)))  # Remove legend and adjust x-axis text

# Save Params histogram plot
ggsave("../out/params_histogram.pdf", width = 10, height = 6)

# Calculate total specific architecture per size of Params
temp <- data |> 
  dplyr::filter(Params > 0) |>  # Filter data where Params > 0
  group_by(Params, Architecture_new) |>  # Group data by Params and Architecture_new
  summarise(TotalSpesificPerSize = n())  # Summarise data to get total specific architecture per size of Params

# Create plot showing percentage of specific architecture per number of parameters
plot_params_vs_architecture_percentage <- data |> 
  dplyr::filter(Params > 0) |>  # Filter data where Params > 0
  group_by(Params) |>  # Group data by Params
  summarise(TotalModelsPerSize = n()) |>  # Summarise data to get total models per size of Params
  inner_join(temp, by = "Params") |>  # Inner join with temp to get total specific architecture per size of Params
  mutate(Percentage = TotalSpesificPerSize / TotalModelsPerSize) |>  # Calculate percentage of specific architecture per size of Params
  ggplot(aes(x = Params, y = Percentage, fill = Architecture_new)) +  # Define aesthetics for the plot
  geom_col(position = "stack", color = "black", size = 0.1) +  # Add stacked column plot with black border
  labs(title = "Percentage of specific architecture per number of parameters", x = "Params", y = "Percentage") +  # Set plot titles and axis labels
  theme_minimal() +  # Apply minimal theme
  scale_fill_brewer(palette = color_palette) +  # Set color palette for fill
  scale_x_continuous(breaks = seq(0, 300, 5)) +  # Set x-axis breaks
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))  # Adjust legend position and x-axis text


# Create plot showing Params vs Average with color gradient by Average
plot_params_vs_average <- data |>  
  dplyr::filter(Params > 0) |>  # Filter data where Params > 0
  group_by(Params) |>  # Group data by Params
  summarise(Average = max(Average), TotalModelsPerSize = n()) |>  # Summarise data to get maximum Average and total models per size of Params
  ggplot(aes(x = Params, color = Average)) +  # Define aesthetics for the plot
  geom_segment(aes(xend = Params,  y = 0, yend = 100), size = 2) +  # Add line segments to represent the maximum Average
  labs(title = "Params vs Average", x = "Params", y = "Average") +  # Set plot titles and axis labels
  scale_x_continuous(breaks = seq(0, 300, 5)) +  # Set x-axis breaks
  scale_y_continuous(breaks = seq(0, 100, 5)) +  # Set y-axis breaks
  theme_minimal() +  # Apply minimal theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") +  # Adjust x-axis text and legend position
  scale_color_continuous(type = "viridis")  # Set color gradient using viridis palette

# Create plot showing Params vs Date vs Average
(plot_params_vs_date_vs_average <- data |>  
  dplyr::filter(Params > 0) |>  # Filter data where Params > 0
  group_by(Params, created_at) |>  # Group data by Params and created_at
  summarise(Average = max(Average), .groups = "drop") |>  # Summarise data to get maximum Average per Params per created_at
  group_by(Params) |>  # Group data by Params
  dplyr::filter(Average == max(Average) & !is.na(created_at)) |>  # Filter rows where Average is maximum and created_at is not NA
  distinct(Params, .keep_all = TRUE)  |>  # Keep only distinct rows
  ungroup() |>  # Ungroup data
  ggplot(aes(x = Params, y = Average, fill = created_at)) +  # Define aesthetics for the plot
  geom_col(color = "black", size = 0.1) +  # Add column plot with black border
  labs(title = "Params vs Max Average", y = "Average", x = "Params") +  # Set plot titles and axis labels
  theme_minimal() +  # Apply minimal theme
  scale_x_continuous(breaks = seq(0, 300, 5)) +  # Set x-axis breaks
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.y = element_text(margin =unit(c(0,0.3,0,0), "cm")), legend.position = "bottom") +  # Adjust x-axis text, y-axis title, and legend position
  scale_fill_date(date_labels = "%Y-%m-%d", low = "purple", high = "lightgreen", guide = "legend"))  # Set fill color gradient using date labels

# Combine plots into a single grid arrangement
gridExtra::grid.arrange(plot_params_vs_architecture, plot_params_vs_architecture_percentage, plot_params_vs_date_vs_average, ncol = 1) -> plot_params_combined

# Save combined plot as a PDF
ggsave("../out/params_combined.pdf", plot_params_combined, width = 20, height = 24)


# Read the CSV file and create a directed graph
graph <- read.csv("chat_models_and_base_models.csv") |> 
  select(-X) |> 
  graph_from_data_frame(directed = TRUE, vertices = NULL)

# Set the size of vertices
vertex_size <- 1

# Create a PDF file for graph output
pdf("graph_output.pdf", width = 50, height = 30)

# Plot the graph with specified parameters
plot(graph, 
     vertex.size = vertex_size, 
     vertex.label.cex = 0.7, 
     vertex.label.dist = 1, 
     edge.arrow.size = 0.1,
     asp = 0)  # asp=0 allows for automatic aspect ratio adjustment

dev.off()  # Close the PDF device

# Pivot the data to long format and filter rows where value equals 1
models_and_basemodels_all <- base.models |> 
  pivot_longer(cols = -model_name_for_query, names_to = "basemodel", values_to = "value") |> 
  subset(value == 1, select = -value)

# Write the resulting data to a CSV file
write.csv(models_and_basemodels_all, "../data/models_and_basemodels.csv")

# Create a directed graph from the data frame
graph <- models_and_basemodels_all |> 
  graph_from_data_frame(directed = TRUE)

# Set the size of vertices
vertex_size <- 0.5

# Create a PDF file for graph output
pdf("graph_output.pdf", width = 50, height = 20)

# Plot the graph with specified parameters
plot(graph, 
     vertex.size = vertex_size, 
     vertex.label.cex = 0.4, 
     vertex.label.dist = 1, 
     edge.arrow.size = 0.1,
     asp = 0)  # asp=0 allows for automatic aspect ratio adjustment

dev.off()  # Close the PDF device

# Select the first few rows of the 'basemodel' column
selected_models <- models_and_basemodels_all$basemodel |> head()

explore_graph_recursively <- function(graph, vertex, edges_collected = NULL) {
  # Get the neighbors of the current vertex (outgoing edges)
  neighbors <- neighbors(graph, vertex, mode = "out")

  for (neighbor in neighbors) {
    # Collect the edge
    edges_collected <- rbind(edges_collected, data.frame(from = V(graph)[vertex]$name, to = V(graph)[neighbor]$name))
    
    # Recursively explore the neighbor
    edges_collected <- explore_graph_recursively(graph, neighbor, edges_collected)
  }

  return(edges_collected)
}

# Initialize an empty data frame to collect all edges
all_edges_collected <- data.frame(from = character(0), to = character(0))

# Define your starting vertices
data |> dplyr::filter(TypeString == "fine-tuned on domain-specific datasets") |> dplyr::filter(model_name_for_query %in% models_and_basemodels_all$model_name_for_query) |> pull(model_name_for_query) |> unique() |> head(10) -> start_vertices

# Explore the graph starting from each defined vertex
for (v in start_vertices) {
  vertex_index <- which(V(graph)$name == v)
  edges_collected <- explore_graph_recursively(graph, vertex_index)
  all_edges_collected <- rbind(all_edges_collected, edges_collected)
}

# Remove duplicate edges
all_edges_collected <- unique(all_edges_collected)
all_edges_collected$from |> c(all_edges_collected$to) |> unique() -> all_vertices
data |> dplyr::filter(model_name_for_query %in% all_vertices) |> select(model_name_for_query,Average, created_at) |> unique() -> vertex_scores

# Create a subgraph containing only the collected edges# Create a subgraph containing only the collected edges# Create a subgraph containing only the collected edges
sub_g <- graph_from_data_frame(all_edges_collected, directed = TRUE)
vertex_scores[match(V(sub_g)$name, vertex_scores$model_name_for_query), c("Average","created_at")]-> vertex_scores 

# Use a layout algorithm to improve the visualization
#vertex_scores$Average[is.na(vertex_scores$Average)] <- mean(vertex_scores$Average, na.rm = TRUE)
layout <- layout_with_fr(sub_g)

vertex_sizes <- (vertex_scores$Average - min(vertex_scores$Average)) / (max(vertex_scores$Average) - min(vertex_scores$Average))
vertex_dates <- vertex_scores$created_at

# Define vertex colors, setting starting vertices to green
vertex_colors <- ifelse(V(sub_g)$name %in% start_vertices, "green", "orange")

vertex_data <- data.frame(name = V(sub_g)$name, score = vertex_scores$Average, date = vertex_dates)
edge_data <- all_edges_collected
edge_data$from_date <- vertex_data$date[match(edge_data$from, vertex_data$name)]
edge_data$to_date <- vertex_data$date[match(edge_data$to, vertex_data$name)]  

# Plot the graph over time with segments representing edges and points representing vertices
ggplot() +
  geom_segment(data = edge_data, aes(x = to_date, xend = from_date, y = to, yend = from), 
               color = "grey", arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  geom_point(data = vertex_data, aes(x = date, y = name, size = score, color = name %in% start_vertices)) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "green")) +
  theme_minimal() +
  labs(title = "Graph Plot Over Time", x = "Date", y = "Vertices", size = "Score") +
  theme(legend.position = "bottom")

# Plot the graph over time with segments representing edges, points representing vertices, and labels for vertices
ggplot() +
  geom_segment(data = edge_data, aes(x = to_date, xend = from_date, y = vertex_data$score[match(to, vertex_data$name)], yend = vertex_data$score[match(from, vertex_data$name)]), 
               color = "grey", arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
  geom_point(data = vertex_data, aes(x = date, y = score, size = score, color = name %in% start_vertices)) +
  geom_text(data = vertex_data, aes(x = date, y = score, label = name), hjust = 1, vjust = 1, size = 3) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "green")) +
  theme_minimal() +
  labs(title = "Graph Plot Over Time", x = "Date", y = "Score", size = "Score") +
  theme(legend.position = "bottom")

# Save the plots as a PDF file
ggsave("../out/graph_plot_over_time.pdf", width = 20, height = 40)

# Use a layout algorithm to improve the visualization
#vertex_scores$Average[is.na(vertex_scores$Average)] <- mean(vertex_scores$Average, na.rm = TRUE)
layout <- layout_with_fr(sub_g)

vertex_sizes <- (vertex_scores$Average - min(vertex_scores$Average, na.rm = T)) / (max(vertex_scores$Average, na.rm = T) - min(vertex_scores$Average, na.rm = T))
vertex_sizes[is.na(vertex_sizes)] <- 0
pdf("subgraph_output.pdf", 29, 20)  # Specify the file name and dimensions
# Plot the subgraph with the collected edges using the chosen layout
plot(sub_g, layout = layout, vertex.color = vertex_colors, vertex.size = vertex_sizes * 5 + 1, vertex.label.cex = 0.6,
     vertex.label.dist = 0, vertex.label.degree = -pi/2, # Positions labels directly above the vertices
     edge.color = "red", main = "Subgraph with Collected Edges",
     vertex.label = V(sub_g)$name, edge.arrow.size = 1, asp = 0, axes = T)
dev.off()  # Close the PDF device


sub_g |> reverse_edges() -> sub_g_reversed

pdf("subgraph_output_reversed.pdf", 29, 20)  # Specify the file name and dimensions
# Plot the subgraph with the collected edges using the chosen layout
plot(sub_g_reversed, layout = layout, vertex.color = vertex_colors, vertex.size = vertex_sizes * 4 + 1, vertex.label.cex = 0.6,
     vertex.label.dist = 0, vertex.label.degree = -pi/2, # Positions labels directly above the vertices
     edge.color = "red", main = "Subgraph with Collected Edges",
     vertex.label = V(sub_g_reversed)$name, edge.arrow.size = 0.6)
dev.off()  # Close the PDF device

# Get unique architecture names and sort them
unique_architectures <- data$Architecture_new |> unique() |> sort()

# Calculate change in popularity of models over time compared to the previous week
data  |> 
  select(Params, created_at, Architecture_new) |>
  na.omit() |>
  arrange(created_at) |>
  mutate(week_start = floor_date(created_at, "week")) |>
  group_by(week_start, Architecture_new) |>
  summarise(total_that_week = n(), .groups = "drop") |>
  complete(week_start = seq(min(week_start), max(week_start), by = "week"), Architecture_new, fill = list(total_that_week = 0)) |> 
  group_by(Architecture_new) |>
  mutate(change = total_that_week - dplyr::lag(total_that_week, default = 0)) -> change_in_popularity

# Plot change in popularity of models over time compared to the previous week
change_in_popularity |> 
  ggplot(aes(x = week_start, y = change, fill = Architecture_new)) +
  stat_smooth(method = "loess", se = FALSE, geom = "area", alpha = 0.6) +
  labs(title = "Change in Popularity of Models Over Time", x = "Date", y = "Change in Popularity") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = color_palette) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("../out/change_in_popularity_compared_to_previous_week.pdf", width = 20, height = 10)


# Calculate change in popularity of models over time compared to the average model popularity
change_in_popularity |> 
  group_by(week_start) |> 
  summarise(average_that_week = mean(total_that_week), total_that_week, Architecture_new) |> 
  mutate(change = total_that_week - average_that_week) |> 
  ggplot(aes(x = week_start, y = change, fill = Architecture_new)) +
  stat_smooth(method = "loess", se = FALSE, geom = "area", alpha = 0.6) +
  labs(title = "Change in Popularity of Models Over Time Compared to Average Model Popularity", x = "Date", y = "Change in Popularity") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = color_palette) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(aes(week_start,average_that_week), method = "loess", se = FALSE, color = "#464d49", size = 0.3)

ggsave("../out/change_in_popularity_compared_to_average.pdf", width = 20, height = 10)


# Define a function to find increasing maximums in a dataframe
find_increasing_maximums <- function(df) {
  max <- 0  # Initialize maximum value
  reference <- df[1,]$Average  # Store the reference value
  plotdf <- data.frame()  # Initialize an empty dataframe to store plot data
  for(row in 1:nrow(df)) {
    if(df[row,]$Average > max) {  # Check if current value is greater than current maximum
      max <- df[row,]$Average  # Update maximum value
      plotdf <- rbind(plotdf, df[row,] |>  mutate(Average = Average - reference))  # Append data to plot dataframe
    }
  }
  return(plotdf)  # Return dataframe with increasing maximums
}

# Define a function to process data and find increasing maximums
process_data <- function(data, pattern, params = NULL, param_range = 0) {
  # Filter data based on parameters and conditions
  if(!is.null(params)) {
    data |> 
      dplyr::filter(Params >= params & Params <= params + param_range & TypeString != "base merges and moerges" & str_detect(tags,"merge") %in% c(NA,FALSE) & Merged == FALSE) -> processed_data
  } else {
    data |> 
      dplyr::filter(TypeString != "base merges and moerges" & str_detect(tags,"merge") %in% c(NA,FALSE) & Merged == FALSE) -> processed_data
  }
  
  # Process data
  processed_data <- processed_data |> 
    select(Architecture, model_name_for_query, Params, Average, created_at, created_at_time) |>  # Select relevant columns
    mutate(original_name = model_name_for_query) |>  # Create a copy of model_name_for_query
    select(Architecture, model_name_for_query, Params, Average, created_at, created_at_time) |>  # Select relevant columns again
    mutate(original_name = model_name_for_query) |>  # Create another copy of model_name_for_query
    mutate(model_name_for_query = tolower(str_remove_all(model_name_for_query, "-"))) |>  # Convert model_name_for_query to lowercase and remove hyphens
    mutate(model_name_for_query = (str_remove_all(model_name_for_query, "_"))) |>  # Remove underscores
    mutate(model_name_for_query = (str_remove_all(model_name_for_query, "\\."))) |>  # Remove dots
    mutate(model_name_for_query = tolower(gsub(c("^.*?/"), "", model_name_for_query))) |>  # Remove substrings before "/"
    dplyr::filter(str_detect(model_name_for_query, pattern) & !is.na(created_at)) |>  # Filter based on pattern and non-missing created_at values
    arrange(created_at_time) |>  # Arrange data by created_at_time
    mutate(total_models = n(), total_days_model = as.numeric(max(created_at) - min(created_at)), cumulative_models = cumsum(!is.na(model_name_for_query)))  # Calculate total models, total days model, and cumulative models
 
  # Check if processed_data contains rows
  if (nrow(processed_data) > 0) {
    release_date <- min(processed_data$created_at)  # Find the minimum release date
    processed_data <- processed_data |>  # Pipe processed_data
      mutate(days_since_release = as.integer(difftime(created_at, release_date, units = "days")), Average_original = Average) |>  # Calculate days since release and store original average
      find_increasing_maximums() |>  # Find increasing maximums using helper function
      mutate(Model = str_c(pattern,params))  # Add a column for the model
  }
  
  return(processed_data)  # Return processed data
}

# Filter data excluding "base merges and moerges" in TypeString and rows with "merge" in tags
data |> 
  dplyr::filter(TypeString != "base merges and moerges") |> 
  dplyr::filter(!str_detect(tags,"merge"))

# Display unique values in TypeString column
data$TypeString |> unique()

# Filter data excluding "base merges and moerges" in TypeString, rows with "merge" in tags, and Merged is FALSE
# Convert model_name_for_query to lowercase, remove hyphens and underscores, filter based on "mistral" in model_name_for_query, arrange by created_at_time,
# calculate total_models, total_days_model, and cumulative_models, and then filter Params equal to 7
data |> 
  dplyr::filter(TypeString != "base merges and moerges") |> 
  dplyr::filter(!str_detect(tags,"merge") %in% c(NA,FALSE) & Merged == F) |> 
  mutate(model_name_for_query = tolower(gsub(c("-","_"), "", model_name_for_query))) |>
  mutate(model_name_for_query = tolower(gsub(c("^.*?/"), "", model_name_for_query))) |>
  dplyr::filter(str_detect(model_name_for_query, "llama3") & !is.na(created_at)) |> 
  arrange(created_at_time) |> 
  mutate(total_models = n(), total_days_model = as.numeric(max(created_at) - min(created_at)), cumulative_models = cumsum(!is.na(model_name_for_query))) |> 
  dplyr::filter(Params == 8)
  

# Process data for "llama3" model with 8 parameters
process_data(data, "llama3", 8)

data |> dplyr::filter(str_detect(model_name_for_query, "meta"))
# Filter data based on conditions related to merges and model_name_for_query
# Store model_name_for_query values in list_of_merges
data |> 
  dplyr::filter(TypeString == "base merges and moerges" | Merged == TRUE | str_detect(tags, "merge") == T | str_detect(tolower(model_name_for_query), "merge") == T) |> 
  pull(model_name_for_query) -> list_of_merges

# Store initial length of list_of_merges
(prev_len <- length(list_of_merges))

# Create a copy of data as data.no.mereges
data.no.mereges <- data

# Loop until length of list_of_merges becomes 0
while (prev_len > 0) {
  # Filter data.no.mereges excluding rows with model_name_for_query containing values in list_of_merges
  data.no.mereges |> 
    dplyr::filter(!str_detect(model_name_for_query, paste(list_of_merges, collapse = "|"))) -> data.no.mereges
  
  # Filter data.no.mereges based on base_model containing values in list_of_merges
  # Store model_name_for_query values in list_of_merges
  data.no.mereges |> 
    dplyr::filter(str_detect(base_model, paste(list_of_merges, collapse = "|")) %in% c(T)) |> pull(model_name_for_query)-> list_of_merges
  
  # Update prev_len with the new length of list_of_merges
  prev_len <- length(list_of_merges)
  print(prev_len)
}



# Process each model
llamma38b_data <- process_data(data.no.mereges, "llama3", 8)
llamma38b_data |> mutate(total_improved_models = n()) -> llamma38b_data
mistral7b_data <- process_data(data.no.mereges, "mistral7b", 7)
mistral7b_data |> mutate(total_improved_models = n()) -> mistral7b_data
mixtral8x7b_data <- process_data(data.no.mereges, "mixtral8x7b", 46)
mixtral8x7b_data |> mutate(total_improved_models = n()) -> mixtral8x7b_data
llamma27b_data <- process_data(data.no.mereges, "llama27b", 7)
llamma27b_data |> mutate(total_improved_models = n()) -> llamma27b_data
phi3mini_data <- process_data(data.no.mereges, "phi3mini")
phi3mini_data |> mutate(total_improved_models = n()) -> phi3mini_data
phi2_data <- process_data(data.no.mereges, "phi2")
phi2_data |> mutate(total_improved_models = n()) -> phi2_data
process_data(data.no.mereges, "llama3", 70) -> llama370b_data
llama370b_data |> mutate(total_improved_models = n()) -> llama370b_data
process_data(data.no.mereges, "llama", 30) -> llama30b_data
llama30b_data |> mutate(total_improved_models = n()) -> llama30b_data
process_data(data.no.mereges, "llama7b",7) -> llama7b_data
llama7b_data |> mutate(total_improved_models = n()) -> llama7b_data
llama270b_data <- process_data(data.no.mereges, "llama2", 70)
llama270b_data |> mutate(total_improved_models = n()) -> llama270b_data
process_data(data.no.mereges, "phi3medium") -> phi3medium_data
phi3medium_data |> mutate(total_improved_models = n()) -> phi3medium_data
# Combine all data
combined_data <- bind_rows(llamma38b_data, mistral7b_data, mixtral8x7b_data, llamma27b_data, phi3mini_data, phi2_data, llama370b_data, llama30b_data, llama7b_data, llama270b_data, phi3medium_data)

# Calculate normalized average popularity and store the result in combined_data
combined_data |> mutate(Average_norm_popular = round(Average / total_models,2)) -> combined_data


data.no.mereges |> dplyr::filter(str_detect(model_name_for_query, "meta"))


# Plot the combined data: Model Comparison Over Time

ggplot(combined_data, aes(x = days_since_release, y = Average, color = Model)) + 
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = original_name), max.overlaps = 50, size = 2) +
  labs(title = "Model Comparison Over Time", x = "Days since release", y = "Average") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_color_discrete()# Adjust color scale if necessary

ggsave("../out/model_comparison_over_time.pdf", width = 30, height = 10)

# Plot the combined data: Model Comparison Over Time Normalized by Total Number of Models
ggplot(combined_data, aes(x = days_since_release, y = Average_norm_popular, color = Model)) + 
  geom_line() +
  geom_point() +
  #geom_label_repel(aes(label = original_name), max.overlaps = 31, size = 2) +
  labs(title = "Model Comparison Over Time Normalized by Total Number of Models", x = "Days since release", y = "Average increase normalized") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Adjust color scale if necessary

ggsave("../out/model_comparison_over_time_normalized.pdf", width = 15, height = 10)

# Plot the combined data: Model Popularity Index vs. Average
ggplot(combined_data |>  mutate(Model_popularity_index = cumulative_models / (days_since_release + 1)), aes(x = Model_popularity_index, y = Average, color = Model)) + 
  geom_point() + 
  #geom_label_repel(aes(label = original_name), max.overlaps = 20, size = 3) +
  labs(title = "Model Popularity Index vs. Average", x = "Model Popularity Index", y = "Average") +
  theme_minimal() +
  scale_color_brewer(palette = "Set3")  # Adjust color scale if necessary



# Compute the popularity index and average by popularity index for each model
combined_data  |> 
  mutate(popularity_index = total_models / total_days_model, Average_by_popularity_index = Average / popularity_index) -> combined_data

# Plot the average by popularity index over time for models with minimum and maximum average
combined_data |> 
  group_by(Model) |> 
  dplyr::filter(Average == min(Average) | Average == max(Average))  |> 
  ggplot(aes(y = Average_by_popularity_index , x = days_since_release, color = Model)) +
  geom_line()

# Plot the average by popularity index over time for models with minimum and maximum average
combined_data |> 
  group_by(Model) |> 
  dplyr::filter(Average == min(Average) | Average == max(Average)) |> 
  mutate(Model_popularity_index = cumulative_models / (days_since_release), Average2 = Average / Model_popularity_index) |> 
  ggplot(aes(y = Average2, x= days_since_release, color = Model)) +
  geom_line()

# Plot the comparison of average over time for each model
ggplot(combined_data, aes(x = days_since_release, y = Average, color = Model)) + 
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = original_name), max.overlaps = 20, size = 2) +
  labs(title = "Model Comparison Over Time Normalized by Total Number of Models", x = "Days", y = "Average") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_fill_binned(type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5))

# Plot the comparison of average over time normalized by popularity index for each model
combined_data |> 
  mutate(Model_popularity_index = cumulative_models / (days_since_release+1), Average2 = Average / (Model_popularity_index+1)) |> 
  ggplot(aes(x = days_since_release, y = Average2, color = Model)) + 
  geom_line() +
  geom_point() +
  geom_label_repel(aes(label = model_name_for_query), max.overlaps = 30, size = 2) +
  labs(title = "Model Comparison Over Time Normalized by Total Number of Models", x = "Days since release of first model", y = "Average normalized per model") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  scale_fill_binned(type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_continuous(breaks = seq(0, max(combined_data$days_since_release), by = 5))

# Save the plot as a PDF
ggsave("../out/model_comparison_over_time_normalized_by_popularity_index.pdf", width = 15, height = 10)


commit.data.long |> select(authors, first_commit) |> arrange(first_commit) |> distinct(authors, .keep_all = TRUE) |> group_by(first_commit) |> summarise(new_authors = n()) |> mutate(total_authors = cumsum(new_authors)) |> dplyr::filter(first_commit >= "2023-01-01") -> prediction.data

# Split data to train and test randomly

train.data <- prediction.data[1:round(nrow(prediction.data) * 0.8),]
test.data <- prediction.data[(round(nrow(prediction.data) * 0.8) + 1):nrow(prediction.data),]

# Fit a linear model to the data
model <- lm(total_authors ~ first_commit + new_authors, data = train.data)

predict(model, newdata = test.data) |> round(0) -> predictions

# Plot the predicted number of users
ggplot(prediction.data, aes(x = first_commit, y = total_authors)) +
  geom_line() +
  geom_point() +
  geom_point(data = test.data, aes(x = first_commit, y = predictions), color = "red") +
  labs(title = "Predicted Number of Users Over Time", x = "Month", y = "Total Users") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



lm(total_authors ~ first_commit, data=train.data) -> model

predict(model, newdata = test.data) |> round(0) -> predictions

# Plot the predicted number of users
ggplot(prediction.data, aes(x = first_commit, y = total_authors)) +
  geom_line() +
  geom_point() +
  geom_point(data = test.data, aes(x = first_commit, y = predictions), color = "red") +
  labs(title = "Predicted Number of Users Over Time", x = "Month", y = "Total Users") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


predict(model, newdata = data.frame(first_commit = as.Date("2025-01-01"))) |> round(0)

# Create sequence of dates from 2024-06-01 to 2025-06-01
dates <- seq(as.Date("2024-06-01"), as.Date("2029-06-01"), by = "month")

mgcv::gam(total_authors ~ s(first_commit |> as.numeric()), data = train.data) -> model_gam

# Predict the number of users for each date
predictions <- data.frame(first_commit = dates) |> mutate(prediction = predict(model_gam, newdata = data.frame(first_commit = dates))) |> mutate_if(is.numeric, round)

# Plot the predicted number of users

predictions |> ggplot(aes(x = first_commit, y = prediction)) + geom_point() + labs(title = "Predicted Number of Users Over Time", x = "Month", y = "Total Users") + theme_minimal() + theme(legend.position = "bottom") + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_y_continuous(breaks = seq(0, 10000, by = 200))




prediction.data |> ggplot(aes(first_commit, total_authors)) + geom_line() + geom_point() + labs(title = "Total Users Over Time", x = "Month", y = "Total Users") + theme_minimal() + theme(legend.position = "bottom") + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_smooth(method = "gam", color = "red")



predict(model_gam, newdata = test.data) |> round(0) -> predictions_gam

# Plot the predicted number of users

ggplot(prediction.data, aes(x = first_commit, y = total_authors)) +
  geom_line() +
  geom_point() +
  geom_point(data = test.data, aes(x = first_commit, y = predictions_gam), color = "red") +
  labs(title = "Predicted Number of Users Over Time", x = "Month", y = "Total Users") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

data |> select(model_name_for_query, Average, ARC, created_at, Architecture_new, Params, TypeString) |> 
  arrange(desc(ARC)) |> head(20)

data |> select(model_name_for_query, Average, HellaSwag, created_at, Architecture_new, Params, TypeString) |>
  arrange(desc(HellaSwag)) |> head(20)

data |> select(model_name_for_query, TruthfulQA, Average, created_at, Architecture_new, Params, TypeString) |>
  arrange(desc(TruthfulQA)) |> head(20)

data |> select(model_name_for_query, Average,Winogrande, created_at, Architecture_new, Params, TypeString) |>
  arrange(desc(Winogrande)) |> head(20)

data |> write.csv("../data/data.csv", row.names = FALSE)
author.activity |> write.csv("../data/author_activity.csv", row.names = FALSE)
combined_data |> write.csv("../data/data_for_model_evolution.csv", row.names = FALSE)

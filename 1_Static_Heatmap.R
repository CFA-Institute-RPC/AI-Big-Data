library(readxl)
library(ComplexHeatmap)
library(tidyverse)
library(circlize)

#Q6: Which of the following platforms/technologies, if any, do you use for the following activities you typically perform in your job?
job_titles <- read_excel("data/Q6_Data with detailed job titles.xlsx")

summary(as.factor(job_titles$`Demographic Type`))

#First thing to check is how many responses we have
n_distinct(job_titles$`Partner Id`)

#427 responses to this question. Not every question in the survey needed to be answered 
#in order to submit the survey - which is why we have 569 responses to Q1.

#Convert relevant columns to factors
job_titles <- job_titles %>%
  mutate_at(vars(`Age Range`, `All Job Titles`,
                 `Charterholder`,`Customer Market`,
                 `Customer Region`,`Customer State Province`,
                 `Customer Sub Region`,`Demographic Type`,
                 `Employment Status`,`Gender`,`Is On Professional Leave`,
                 `Question Description`,`Question ID Grouped`,
                 `Society Name`,`Years with the Charter Grouped`), as.factor)

#Find out for each job category, how many unique jobs titles there are...
job_title_counts <- job_titles %>%
  distinct(`Partner Id`, .keep_all = TRUE) %>%
  group_by(`All Job Titles`) %>%
  summarise(count = n())

#How to deal with small numbers?

#Start off with heatmap for each job category... later expand to individual job titles
job_category_counts <- job_titles %>%
  distinct(`Partner Id`, .keep_all = TRUE) %>%
  group_by(`Demographic Type`) %>%
  summarise(count = n())

#Remove 'Unspecified' and 'Other' category as noninformative
job_titles <- job_titles %>%
  filter(`Demographic Type` != 'Unspecified' & `Demographic Type`!= 'Other')

#Drop empty factor levels 
job_titles$`Demographic Type` <- droplevels(job_titles$`Demographic Type`)

#Split into smaller dataframes for each job category
job_category_list <- split(job_titles, job_titles$`Demographic Type`)

#Function to count number of answers to each question-platform grouping 
data_transform <- function(df) {
  #Count how many answered the workflow sub-question
  answer_counts <- df %>%
    group_by(`Question Description`) %>%
    summarise(total_answered_this_question = n_distinct(`Partner Id`))
  
  #Count how many tech use answers there are for each sub question
  usage_counts <- df %>%
    group_by(`Question Description`, `Question ID Grouped`) %>%
    summarise(count_using_platform = n_distinct(`Partner Id`), .groups = "drop")
  
  #Join the two dfs together to calculate %
  usage_counts %>%
    left_join(answer_counts, by = "Question Description") %>%
    mutate(percentage = count_using_platform / total_answered_this_question * 100)
}

#Initialize list to store transformed dataframes
heatmap_data <- list()

#Apply function to each job category dataframe
for (i in 1:length(job_category_list)) {
  df <- job_category_list[[i]]
  heatmap_data[[i]] <- data_transform(df)
}

# Retain n= for each question within each job category
n_counts <- list()
for (data in 1:length(heatmap_data)) {
  temp <- heatmap_data[[data]]
  temp <- temp %>%  
    select(c('Question Description','total_answered_this_question')) %>%
    filter(`Question Description`!= "None of the above") %>% #drop 'none of the above' answer
    distinct()
  n_counts[[data]] <- temp
}

#function to pivot wide data (columns = platform, rows = questions)
pivot_data <- function(df) {
df %>%
    filter(`Question Description` != "None of the above") %>% #remove 'none of the above'
  select(-c(`count_using_platform`,`total_answered_this_question`)) %>%  
  pivot_wider(
    names_from = `Question ID Grouped`,
    values_from = percentage,values_fill = 0) %>%
  group_by(`Question Description`) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
}

#Apply the transformation to each dataframe in the list
heatmap_data <- lapply(heatmap_data,pivot_data)

#To plot heatmaps next to each other as a list, each heatmap needs to have the same number of rows & columns
lapply(heatmap_data,dim)
#heatmap 1 has 20 questions, heatmap 5 has 19, heatmap 6 has 18... they need to be uniform

#questions <- unique(as.character(heatmap_data[[1]]$`Question Description`))

#for (i in 1:length(heatmap_data)) { #Iterate through each heatmap in list 
 # for (question in 1:length(questions)) { #Iterate through each question
  #  if (questions[question] %in% as.character(heatmap_data[[i]]$`Question Description`)) { 
   #   next } #If question is in QD column - skip to next question, otherwise add new row to dataframe with that question
  #  else {
   #   heatmap_data[[i]] <- add_row(heatmap_data[[i]],`Question Description`= questions[question],`Excel platform`=0,
    #          `Gen AI`=0,`Market databases`=0,`Other`=0,`Other programming languages`=0,
     #         `Other visualization technologies`=0,`Python platform`=0,`SQL`=0) }
  #}
  #heatmap_data[[i]] <- heatmap_data[[i]] %>%
   # arrange(`Question Description`)  #arrange rows alphabetically for consistent ordering
    
  #heatmap_data[[i]]$`Question Description` <- as.factor(heatmap_data[[i]]$`Question Description`) # convert back to factor column
#}

# Verify code worked 
#lapply(heatmap_data,dim)

# Apply same code to n_counts
#for (i in 1:length(n_counts)) { 
 # for (question in 1:length(questions)) { #Iterate through each question
  #  if (questions[question] %in% as.character(n_counts[[i]]$`Question Description`)) { 
   #   next } #If question is in QD column - skip to next question, otherwise add new row to dataframe with that question
   # else {
    #  n_counts[[i]] <- add_row(n_counts[[i]],`Question Description`= questions[question],
     #                          `total_answered_this_question`=0) }
 # }
#  n_counts[[i]] <- n_counts[[i]] %>%
  #  arrange(`Question Description`)  #arrange rows alphabetically for consistent ordering
  #n_counts[[i]]$`Question Description` <- as.factor(n_counts[[i]]$`Question Description`) # convert back to factor column
#}
#Verify code worked
#lapply(n_counts,dim)

#We also need to make sure the rownames of each dataframe are in the same order - sort alphabetically.

#Assign row names and remove redundant columns
heatmap_data <- lapply(heatmap_data, function(df) {
  rownames <- df$`Question Description`  #Extract row names
  df <- as.matrix(df[, -which(names(df) == "Question Description")])  #Convert to matrix, excluding the column
  rownames(df) <- rownames  #Set row names for the matrix
  return(df)
})

#Label dataframes
names(heatmap_data) <- names(job_category_list)
names(n_counts) <- names(job_category_list)

#Create gradient of heatmap colours starting from antiquewhite (0%) to darkorchid1 (100%)
heatmap_colours <- colorRamp2(c(0,100),c("lightblue","#002F6C"))

counts_list <- list()
for (i in 1:length(n_counts)) {
  temp <- n_counts[[i]]
  counts_vec <- paste0("n=",as.character(temp$total_answered_this_question))
  counts_list[[i]] <- counts_vec
}

names(counts_list) <- names(n_counts)

n_annotations <- list()

for (i in 1:length(counts_list)) {
  temp <- rowAnnotation(
    `Total Answered` = anno_text(counts_list[[i]],
                                 gp = gpar(fontsize = 10, col = "black"))
  )
  n_annotations[[i]] <- temp
}

names(n_annotations) <- names(n_counts)

#Create example heatmap
advisory_heatmap <- Heatmap(heatmap_data[['Advisory']],name="Percentage",col=heatmap_colours,
        column_title='Advisory',
        column_title_gp=gpar(fontsize=20,fontface="bold"), #create column title
        border_gp = gpar(col = "black", lty = 1), #set heatmap border
        rect_gp = gpar(col = "black", lwd = 1), #set cell borders
        show_row_dend=FALSE, #remove col/row dendograms
        show_column_dend=FALSE,
        row_names_side = "left",column_names_side="top",
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.0f", heatmap_data[['Advisory']][i, j]), x, y, gp = gpar(fontsize = 10,
                                                                                       col="white")) #Add % values to cells
        },row_names_gp=gpar(fontsize=10),column_names_gp=gpar(fontsize=10),
        right_annotation = n_annotations[[1]]) #add sample size 

advisory_heatmap = draw(advisory_heatmap) #Speeds up heatmap creation


#Initialise list to store heatmaps for each job category
heatmap_list <- list()

create_heatmap <- function(df_name,heatmap_colours,annotation){
  
  data <- heatmap_data[[df_name]]
  data_annotation <- n_annotations[[annotation]]
  
  counts <- n_counts[[df_name]]
  
  #Create Heatmaps
  Heatmap(data,name="Percentage",col=heatmap_colours,
          column_title=df_name,
          column_title_gp=gpar(fontsize=20,fontface="bold"), #create column title
          border_gp = gpar(col = "black", lty = 1), #set heatmap border
          rect_gp = gpar(col = "black", lwd = 1), #set cell borders
          show_row_dend=FALSE, 
          show_column_dend=FALSE,
          row_names_side = "left",column_names_side="top",
          column_names_rot = 45, #rotate column names
          row_names_max_width = max_text_width(
            rownames(data), 
            gp = gpar(fontsize = 12)
          ),
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid.text(sprintf("%.0f", data[i, j]), x, y, gp = gpar(fontsize = 12,col='white')) #add % values to cells
          },row_names_gp=gpar(fontsize=12),column_names_gp=gpar(fontsize=12),
          right_annotation = data_annotation) 
}

df_names <- names(heatmap_data)

#Loop through each dataframe name, create heatmap, and store in the list
for (df_name in df_names) {
  heatmap_list[[df_name]] <- create_heatmap(df_name, heatmap_colours,annotation=df_name)
}

#Save heatmaps
saveRDS(heatmap_list, file = "app/heatmap_list.rds")




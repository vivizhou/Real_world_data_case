convert_data <- function(main_directory, xlsx_directory){
  # List all patient directories
  patient_directories <- list.dirs(main_directory, recursive = FALSE, full.names = TRUE)
  
  # Define data groups
  data_groups <- c("AdmitMedsTac", "AdmitDate", "Admissions", "BasicPatient", "BasicTransplant", "CIMSmisc", "CIMSother",
                   "Dialysis", "Glucose", "Labsavg", "Labsraw", "Meds", "mGFRMRI", "MMF", "serology", "Tac", "vitals")
  
  # Create an empty list to store data frames for each data group
  data_list <- lapply(1:length(data_groups), function(i) {
    data.frame()
  })
  names(data_list) <- data_groups
  
  # Define keywords for each data group
  list_keywords <- list(c("admi.*", ".*med.*", ".*tac.*"),
                        c("admit", "date"),
                        c("admission.*"),
                        c("basic.*patient"),
                        c("basic.*transplant"),
                        c("cims.*misc"),
                        c("cims.*other"),
                        c("dialysis"),
                        c("glucose"),
                        c("labs.*avg"),
                        c("labs.*raw"),
                        c("meds"),
                        c(".*mgfr.*", ".*mri.*"),
                        c(".*mmf.*"),
                        c(".*serology.*"),
                        c(".*tac.*"),
                        c(".*vital.*")
  )
  
  # Function to check if the filename contains all keywords
  contains_all_keywords <- function(file_name, keywords) {
    file_name_lower <- tolower(file_name)
    return(all(sapply(keywords, function(keyword) length(grep(keyword, perl = TRUE, file_name_lower)) > 0)))
  }
  
  # Process each patient directory
  for (patient_directory in patient_directories) {
    
    # Exclude malfunctioned files in the "Data/11-20" directory
    xls_files <- if (patient_directory == "Data/11-20") {
      list.files(patient_directory, pattern = "[^1]\\.xls$", full.names = TRUE)
    } else {
      list.files(patient_directory, pattern = "\\.xls$", full.names = TRUE)
    }
    
    # Process each xls file in the directory
    for (xls_file in xls_files) {
      file_name <- tools::file_path_sans_ext(basename(xls_file))
      index <- 0
      
      # Check which data group the file belongs to based on keywords
      for (i in 1:length(list_keywords)){
        
        keywords <-list_keywords[[i]]
        if (contains_all_keywords(file_name, keywords)) {
          index <- i
          cat(paste("Processing",file_name, "match to index ", index), "\n")
          break
        }
      }
      
      # Read and process the data based on the determined data group
    
      if (index > 0) {
        if (index == 1) {
          # for now, for this data type, sheet 2 instead of sheet 1 will be taken.
          # for the data type with MED and TAC, some data have 10 columns, while some have 9,
          # I harmonize them by adding an extra column to the ones with 9.
          data <- readxl::read_excel(xls_file, sheet = 2)
          if (ncol(data) < 10) {
            data <- readxl::read_excel(xls_file, sheet = 2, col_types = c('numeric', 'date', 'text', 'guess', 'text', 
                                                                          "text", "numeric", "text", "date"))
            names(data[10]) <- "remark"
          } else if (ncol(data) == 10) {
            data <- readxl::read_excel(xls_file, sheet = 2, col_types = c('numeric', 'date', 'text', 'guess', 'text', 
                                                                          "text", "numeric", "text", "date", "text"))
            names(data)[10] <- "remark"
          }
          
        } else if (index == 13) { 
          # For mGFR data, some are not well separated into columns.
          data_raw <- readxl::read_excel(xls_file, col_types = "text")
          
          if (ncol(data_raw)< 2){
            data <- data.frame()
            for (j in 1:nrow(data_raw)){
              delim = "\\s{2,}"
              split_columns <- strsplit(as.character(data_raw[j,1]), delim, perl = TRUE)[[1]]
              data <- rbind(data,split_columns )
            }
            colnames(data) <-  strsplit(as.character(colnames(data_raw)), delim, perl = TRUE)[[1]]
            
          }
        } else{
          
          # For other data groups, read data directly, all columns considered text type for convenience.
          data <- readxl::read_excel(xls_file, col_types = "text")
          
        }
        names(data) <- tolower(names(data))
        
        if (nrow(data_list[[index]])>0) {
          data_list[[index]] <- merge(data_list[[index]],data, by = intersect(names(data_list[[index]]),names(data)), all = TRUE)
          
        }else{
          data_list[[index]] <- data
        }
        cat(paste("\n", "number of rows", nrow(data_list[[index]]), "\n" ))
      }else{
        # Internal check to find inconsistent data
        cat(paste("Error with file", file_name))
      }
      
    }
  }
  
  # Save each group of data into one xlsx file
  for (key in names(data_list)) {
    # Combine data frames along the rows
    combined_data <- data_list[[key]]
    xlsx_file <- paste0(key, ".xlsx")
    xlsx_path <- file.path(xlsx_directory, xlsx_file)
    # Write data to xlsx file
    writexl::write_xlsx(combined_data, xlsx_path)
  }
  
}


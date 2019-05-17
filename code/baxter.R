
library(tidyverse)
library(readxl)

get_metadata <- function() {
    read_excel(
        path = "raw_data/baxter.metadata.xlsx",
        col_types = c(
            sample = "text",
            fit_result = "numeric",
            Site = "text",
            Dx_Bin = "text",
            dx = "text",
            Hx_Prev = "logical",
            Hx_of_Polyps = "logical",
            Age = "numeric",
            Gender = "text",
            Smoke = "logical",
            Diabetic = "logical",
            Hx_Fam_CRC = "logical",
            Height = "numeric",
            Weight = "numeric",
            NSAID = "logical",
            Diabetes_Med = "logical",
            stage = "text"
        )
    )
    metadata[["Height"]] <- na_if(metadata[["Height"]], 0)
    metadata[["Weight"]] <- na_if(metadata[["Weight"]], 0)
    metadata[["Site"]] <-
        recode(.x = metadata[["Site"]], "U of Michigan" = "U Michigan")
    metadata[["Dx_Bin"]] <-
        recode(.x = metadata[["Dx_Bin"]], "Cancer." = "Cancer")
    metadata[["Gender"]] <- recode(.x = metadata[["Gender"]], "m" = "male")
    metadata[["Gender"]] <-
        recode(.x = metadata[["Gender"]], "f" = "female")
    
    metadata <- rename_all(.tbl = metadata, .funs = tolower)
    metadata <- rename(
        .data = metadata,
        previous_history = hx_prev,
        history_of_polyps = hx_of_polyps,
        family_history_of_crc = hx_fam_crc,
        diagnosis_bin = dx_bin,
        diagnosis = dx,
        sex = gender
    )
    
    metadata <-
        mutate(metadata, diagnosis = factor(diagnosis, levels = c("normal", "adenoma", "cancer")))
    return(metadata)
}

get_bmi <- function(weight_kg, height_cm){
    return(weight_kg / (height_cm/100) ^ 2)
}

get_bmi_category <- function(weight_kg, height_cm){
    bmi <- get_bmi(weight_kg, height_cm)
    
    bmi_cat <- case_when(bmi >= 30 ~ "obese",
                         bmi >= 25 ~ "overweight",
                         bmi >= 18.5 ~ "normal",
                         TRUE ~ "underweight")
    
    return(bmi_cat)
}

is_obese <- function(weight_kg, height_cm){
    bmi_category <- get_bmi_category(weight_kg, height_cm)
    return(bmi_category == "obese")
}

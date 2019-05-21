library(dplyr)
library(readxl)

#' Reads and cleans raw metadata
#'
#' @return The cleaned metadata as a dataframe
#' @examples
#' metadata <- get_metadata()
get_metadata <- function() {
    baxter_metadata <- read_excel(
        path = "inst/extdata/baxter.metadata.xlsx",
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
    baxter_metadata[["Height"]] <- na_if(baxter_metadata[["Height"]], 0)
    baxter_metadata[["Weight"]] <- na_if(baxter_metadata[["Weight"]], 0)
    baxter_metadata[["Site"]] <-
        recode(.x = baxter_metadata[["Site"]], "U of Michigan" = "U Michigan")
    baxter_metadata[["Dx_Bin"]] <-
        recode(.x = baxter_metadata[["Dx_Bin"]], "Cancer." = "Cancer")
    baxter_metadata[["Gender"]] <- recode(.x = baxter_metadata[["Gender"]], "m" = "male")
    baxter_metadata[["Gender"]] <-
        recode(.x = baxter_metadata[["Gender"]], "f" = "female")
    baxter_metadata <- rename_all(.tbl = baxter_metadata, .funs = tolower)
    baxter_metadata <- rename(
        .data = baxter_metadata,
        previous_history = hx_prev,
        history_of_polyps = hx_of_polyps,
        family_history_of_crc = hx_fam_crc,
        diagnosis_bin = dx_bin,
        diagnosis = dx,
        sex = gender
    )
    baxter_metadata <-
        mutate(baxter_metadata, diagnosis = factor(diagnosis, levels = c("normal", "adenoma", "cancer")))
    return(baxter_metadata)
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

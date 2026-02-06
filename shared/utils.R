# Functions shared by Sangeeta. Need modifying

library(dplyr)
library(paletteer)

ggsave_manuscript <- function(
    filename,
    plot,
    width = 6,
    height = 4,
    units = "in",
    dpi = 300
) {
  
  ggsave(
    filename = paste0(filename, ".svg"), plot = plot, 
    width = width, height = height, units = units
  )
  ggsave(
    filename = paste0(filename, ".png"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )
  
  ggsave(
    filename = paste0(filename, ".pdf"), plot = plot, width = width,
    height = height, units = units, dpi = dpi
  )
  
}  

## text size in theme element is in pts (not mms, ggplot2 docs are confusing)
## size in geom_text is in mms and needs converting to pts.
## Another confusing thing in the docs is that the constant shipped with 
## ggplot2 .pt is 2.845276, not 0.35mm as stated in the docs.
## that is why to go from pt to mm, we divide by 2.845276
## 1pt = 0.3527778mm
## Remember that pt is a phyical absolute unit.
## So when resizing the plot, you need to change font sizes.
theme_manuscript <- function() {
  theme_classic() +
    theme(
      ##text = element_text(family = "Arial", ),
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.title.x = element_blank(),      
      axis.text = element_text(size = 10),
      
      legend.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_blank(),
      
      strip.text = element_text(size = 14),
    )
}

datasource_palette <- function() {
  # colours from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  moh_colour <- "#E69F00FF" #"#D55E00" # "#D68A59"
  iata1_colour <- "#56B4E9FF" #"#56B4E9" #"#01796F"
  iata2_colour <- "#009E73FF" #"#F0E442" #"#CC79A7" #"#009E73" #"#007BA7"
  # npfsswabcolor <- "#CC5500"
  # npfssurveycolor <- "#808080"
  out <- c(moh_colour, iata1_colour, iata2_colour)
  # names(out) <- c("moh", "iata_method1", "iata_method2")
  names(out) <- c("MOH", "IATA w/ baseline adjustment", "IATA w/ no baseline adjustment")
  out
}

testing_palette <- function() {
  # colours from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
  notest_colour <- "#F0E442" 
  test80_colour <- "#0072B2" 
  test99_colour <- "#D55E00" 
  testsympt_colour <- "#CC79A7" 
  out <- c(notest_colour, test80_colour, test99_colour, testsympt_colour)
  # names(out) <- c("moh", "iata_method1", "iata_method2")
  names(out) <- c("0.00", "0.80", "0.99", "Symptomatic")
  out
}


age_palette <- function(category = c("qs", "school")) {
  match.arg(category)
  out <- paletteer_d("basetheme::clean")
  out <- c("#000000FF", out)
  if (category == "qs") {
    out <- out[c(1, 2, 3, 4, 5, 6)]
    age_groups <- age_group_levels(category = "qs")
  } else {
    ## Only the first age-group is common between the two
    out <- out[c(1, 7, 8, 9, 10, 11)]
    age_groups <- age_group_levels(category = "school")
  }
  out <- out[seq_along(age_groups)]
  names(out) <- age_groups
  out
}


weekday_palette <- function() {
  out <- paletteer_d("ggthemes::Red_Blue_Brown")
  names(out) <- wdays <- weekdays(
    seq(as.Date("2/10/2023"), length.out = 7, by = "1 day")
  )
  out
}

facet_by_sha <- function(...) {
  facet_wrap(~sha, labeller = labeller(sha = sha_labeller()), ...)
}

facet_by_age_group <- function(category = c("qs", "school"), ...) {
  facet_wrap(~age_group, ...)
}

scale_y_sha <- function(...) {
  scale_y_discrete(labels = sha_labeller(), breaks = sha_levels(), ...)
}

sha_levels <- function() {
  c(
    ENG = "England", NW = "North West", 
    NE = "North East", `Y&H` = "Yorkshire & Humber",
    WM = "West Midlands", 
    EM = "East Midlands", EE = "East of England", 
    SW = "South West", 
    SC = "South Central", LON = "London", 
    SE = "South East Coast"
  )
}

sha_labeller <- function() {
  vals <- names(sha_levels()) ## These should be the values
  labels <- unname(sha_levels()) ## These should be the labels
  setNames(object = vals, nm = labels)
}

sha_palette <- function() {
  out <- paletteer_d("RColorBrewer::Paired")
  out <- c("#000000FF", out[1:10])
  names(out) <- sha_levels()
  out
}
## Take an age and assign it to a bin
## Return a pretty label.
## upper is a numeric vector indicating the
## upper limit of the age-bin, 
## lower and upper are included in the bin.
## e.g. age = 5
##
## age_to_age_group(age = 5, upper = c(4, 8, 12))
## returns
## [5, 8]
## age_to_age_group(age = 14, upper = c(4, 8, 12))
## [12, Inf)
age_to_age_group_alt <- function(age, upper) {
  f <- function(age, upper) {
    ## First index in upper such that age is smaller
    first <- which(upper >= age)
    if (length(first) == 0) {
      label <- paste0("[", upper[length(upper)], ", Inf)")
    } else if (first[1] == 1) {
      label <- paste0("[0, ", upper[1], "]")
    } else {
      first <- first[1]
      label <- paste0("[", upper[first - 1] + 1,", ", upper[first], "]")
    }
    label
  }
  out <- vector(mode = "list", length = length(age))
  for (idx in seq_along(age)) {
    out[idx] <- f(age[idx], upper)
  }
  unlist(out)
}

age_group_levels <- function(category = c("qs", "school", "sampling_effort", 
                                          "response", "other")) {
  
  match.arg(category)
  if (category == "qs")
    out <- c("1-4", "5-14", "15-24", "25-44", "45-64", "65+")
  else if (category == "school")
    out <- c("1-4", "5-10", "11-16", "17-24", "25+")
  else if (category == "sampling_effort") {
    out <- c("5-10", "11-15", "16-29", "30-64", "65+")
  } else if (category == "response") {
    out <- c("5-17", "18-20", "30-44", "45+")
  } else {
    out <- c("<1", "1-4", "5-10", "11-14", "15", "16-17", "18-20",
             "21-24", "25-29", "30-34", "35-44", "45-54", "55-64", "65-74", "75-84", 
             "85+")
  }
  out
}

age_to_age_group <- function(age, category = c("qs", "school", "sampling_effort", 
                                               "response", "other")) {
  match.arg(category)
  if (category == "qs") {
    age_group <- case_when(
      age < 1 ~ "<1",
      age >= 1 & age <= 4 ~ "1-4",
      age >= 5 & age <= 14 ~ "5-14",
      age >= 15 & age <= 24 ~ "15-24",
      age >= 25 & age <= 44 ~ "25-44",
      age >= 45 & age <= 64 ~ "45-64",
      TRUE ~ "65+"
    )
  } else if (category == "school") {
    age_group <- case_when(
      age < 1 ~ "<1",
      age >= 1 & age <= 4 ~ "1-4",
      age >= 5 & age <= 10 ~ "5-10",
      age >= 11 & age <= 16 ~ "11-16",
      age >= 17 & age <= 24 ~ "17-24",
      TRUE ~ "25+"
    )} else if (category == "sampling_effort") {
      age_group <- case_when(
        age < 1 ~ "<1",
        age >= 1 & age <= 4 ~ "1-4",
        age >= 5 & age <= 10 ~ "5-10",
        age >= 11 & age <= 15 ~ "11-15",
        age >= 16 & age <= 29 ~ "16-29",
        age >= 30 & age <= 64 ~ "30-64",
        age >= 65  ~ "65+"
      )
    } else if (category == "response") {
      age_group <- case_when(
        age < 1 ~ "<1",
        age >= 1 & age <= 4 ~ "1-4",
        age >= 5 & age <= 17 ~ "5-17",
        age >= 18 & age <= 20 ~ "18-20",
        age >= 30 & age <= 44 ~ "30-44",
        age >= 45  ~ "45+"
      )
    } else {
      age_group <- case_when(
        age < 1 ~ "<1",
        age >= 1 & age <= 4 ~ "1-4",
        age >= 5 & age <= 10 ~ "5-10",
        age >= 11 & age <= 14 ~ "11-14",
        age == 15 ~ "15",
        age >= 16 & age <= 17 ~ "16-17",
        age >= 18 & age <= 20 ~ "18-20",
        age >= 21 & age <= 24 ~ "21-24",
        age >= 25 & age <= 29 ~ "25-29",
        age >= 30 & age <= 34 ~ "30-34",
        age >= 35 & age <= 44 ~ "35-44",
        age >= 45 & age <= 54 ~ "45-54",
        age >= 55 & age <= 64 ~ "55-64",
        age >= 65 & age <= 74 ~ "65-74",
        age >= 75 & age <= 84 ~ "75-84",
        TRUE ~ "85+"
      )
    }
  age_group
}




## Return a character vector that can be used as levels of age_group.
## makes plotting easier.
## left_zero indicates whether the first bin should be
## [0, <first element of upper>)
age_group_levels_alt <- function(upper, left_zero = TRUE) {
  
  if (left_zero) {
    ls <- vector(mode = "list", length = length(upper) + 1)
    ls[1] <- paste0("[0, ", upper[1], "]")
    
  } else {
    ls <- vector(mode = "list", length = length(upper))
    ls[1] <- paste0("[", upper[1], ", ", upper[2], "]")
  }
  index <- 1:(length(ls) - 1)
  for (idx in index) {
    ls[idx + 1] <- paste0(
      "[", upper[idx] + 1, ", ", upper[idx + 1], "]"
    )
  }
  
  ls[length(ls)] <- paste0(
    "[", upper[length(upper)], ", Inf)"
  )
  
  ls
}


## Convert to lower, strip punctuation marks
clean_pct_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  ## also remove "and"
  x <- gsub("and", "", x)
  x
}


## Eli Billauer, 3.4.05 (Explicitly not copyrighted)
## This function is released to the public domain; Any use is allowed.
## First adapted by H Mills,
## then by S Bhatia (using chatgpt)
## v is a numeric vector, in our case incidence time series
## x indexes v, could be a sequence of integers, or dates for example
peakdet <- function(v, delta, x = NULL) {
  # Initialize empty data frames to store maxima and minima
  maxtab <- data.frame(index = numeric(0), value = numeric(0))
  mintab <- data.frame(index = numeric(0), value = numeric(0))
  
  # Ensure that the input vector 'v' is numeric
  v <- as.numeric(v)
  
  # If x is not provided, create an index vector
  if (is.null(x)) {
    x <- seq_along(v)
  } else {
    # Convert x to numeric and check for length consistency
    x <- as.numeric(x)
    if (length(v) != length(x)) {
      stop("Input vectors v and x must have the same length")
    }
  }
  
  # Check if delta is a scalar and positive
  if (length(delta) > 1) {
    stop("Input argument DELTA must be a scalar")
  }
  if (delta <= 0) {
    stop("Input argument DELTA must be positive")
  }
  
  # Initialize variables to track minima and maxima
  mn <- Inf
  mx <- -Inf
  mnpos <- NA
  mxpos <- NA
  lookformax <- TRUE
  
  # Loop through the vector v
  for (i in seq_along(v)) {
    this <- v[i]
    
    # Update maxima and minima
    if (this > mx) {
      mx <- this
      mxpos <- x[i]
    }
    
    if (this < mn) {
      mn <- this
      mnpos <- x[i]
    }
    
    # Check for peaks based on the lookformax variable
    if (lookformax) {
      if (this < mx - delta) {
        # Record maximum and update variables
        maxtab <- rbind(maxtab, cbind(index = mxpos, value = mx))
        mn <- this
        mnpos <- x[i]
        lookformax <- FALSE
      }
    } else {
      if (this > mn + delta) {
        # Record minimum and update variables
        mintab <- rbind(mintab, cbind(index = mnpos, value = mn))
        mx <- this
        mxpos <- x[i]
        lookformax <- TRUE
      }
    }
  }
  
  # Return a list containing the maxima and minima tables
  list(maxtab = maxtab, mintab = mintab)
}

scale_x_date_manuscript <- function(...) {
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = c(0, 0), ...
  )
}
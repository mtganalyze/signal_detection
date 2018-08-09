library(jsonlite)
library(dplyr)
library(stringr)


### load set data
### fetched from https://mtgjson.com/

load_setdata <- function(setname, local = TRUE){
  if(local){
    filepath <- paste0("setdata/", setname, ".json")
  } else {
    filepath <- paste0("https://mtgjson.com/json/", setname, ".json")
  }
  fromJSON(filepath)
}

### load card rating data
### fetched from http://www.draftaholicsanonymous.com/

load_cardratings <- function(setname){
  filepath <- paste0("draftaholicsdata/", setname, ".txt")
  rawdata <- read.table(filepath, sep = "\t", quote = "")[[1]] %>% as.character()
  data_index <- seq(which(rawdata == "Which Card Would You Choose?") + 2, 
                    which(rawdata == "Hover over a row to preview cards") - 1)
  dat <- rawdata[data_index]
  df <- as.data.frame(t(matrix(dat, nrow = 3)), stringsAsFactors = FALSE)
  names(df) <- c("rank", "name", "rating")
  df <- df %>% mutate(rank = as.numeric(rank),
                      rating = as.numeric(rating))
  list("timestamp" = file.mtime(filepath),
       "carddata" = df)
}

### join card ratings to card data

attach_ratings <- function(setdata, ratingsdata){
  mod_setdata <- setdata
  mod_setdata$cards <- left_join(
    mod_setdata$cards %>% 
      mutate(cleanname = lapply(str_extract_all(tolower(name), "[a-z]+"), 
                                FUN = function(x)paste0(x, collapse = "")) %>% unlist), 
    ratingsdata$carddata %>% 
      mutate(cleanname = lapply(str_extract_all(tolower(name), "[a-z]+"), 
                                FUN = function(x)paste0(x, collapse = "")) %>% unlist) %>% 
      select(-name),
    by = "cleanname") %>% 
    select(-cleanname)
  mod_setdata$ratingsDate <- str_sub(ratingsdata$timestamp, 1, 10)
  mod_setdata
}

### sample a given number of random boosters from setdata
### allows seeding a fixed card through the argument fixcard

sample_booster <- function(setdata, n_booster, fixcard = NULL){
  content <- lapply(setdata$booster, function(x) first(x)) %>% unlist
  
  ## get set of rares, uncommons and commons
  n_rares <- sum(content == "rare")
  rares <- setdata$cards  %>%  
    filter(!is.na(rating), rarity %in% c("Rare", "Mythic Rare")) %>%
    mutate(prob = ifelse(rarity == "Mythic Rare", 0.5, 1))
  n_uncommons <- sum(content == "uncommon")
  uncommons <- setdata$cards  %>%  
    filter(!is.na(rating), rarity %in% c("Uncommon")) 
  n_commons <- sum(content == "common")
  commons <- setdata$cards  %>%  
    filter(!is.na(rating), rarity %in% c("Common")) 
  
  ### check whether a fixed card has to be includes
  if(!is.null(fixcard)){
    if(fixcard %in% c(rares$number, uncommons$number, commons$number)){
      if(fixcard %in% rares$number){
        n_rares <- n_rares - 1
        rares <- rares %>% filter(number != fixcard)
      }
      if(fixcard %in% uncommons$number){
        n_uncommons <- n_uncommons - 1
        uncommons <- uncommons %>% filter(number != fixcard)
      }
      if(fixcard %in% commons$number){
        n_commons <- n_commons - 1
        commons <- commons %>% filter(number != fixcard)
      }
      fix_sample <- rep(fixcard, n_booster)
    } else {
      warning("Fixed card not contained in set")
      fix_sample <- character(0)
    }
  } else {
    fix_sample <- character(0)
  }
  
  ### sample rares, uncommons, commons for each booster
  r_sample <- sapply(seq(n_booster), function(i){
    sample(rares$number, size = n_rares, replace = FALSE, prob = rares$prob)
  }) %>% unlist
  u_sample <- sapply(seq(n_booster), function(i){
    sample(uncommons$number, size = n_uncommons, replace = FALSE)
  }) %>% unlist
  c_sample <- sapply(seq(n_booster), function(i){
    sample(commons$number, size = n_commons, replace = FALSE)
  }) %>% unlist
  
  ### combine all samples into booster matrix
  matrix(rbind(fix_sample, r_sample, u_sample, c_sample), 
         nrow = length(fixcard) + n_rares + n_uncommons + n_commons)
}

### translate card ratings into pick probabilities
### model is based on generalized ELO-model for multiple options, i.e.
### the rating scale corresponds to the log-Odds scale
### (ratings differences among mulitple cards are consistend with corresponding Odds-Ratios)
### The remaining optimization problem is to find a PROPER probability vector which
### satisfies the predetermined OddsRatios
### Elo-coeff (assumed to be 400 / log(10)) to translate ratings into logOdds 
### was inferred from draftaholics.com (by trial and error) 

ratings2probs <- function(ratings, coeff = 173.7){
  if(length(ratings) == 0){return(1)}
  ### compute rating difference with respect to maximum rating
  rating_diffs <- ratings - max(ratings)
  ### function to compute probs according to max probability based on log-odds relation
  probs_from_max <- function(maxprob){
    alpha <- maxprob / (1 - maxprob) * exp(2 * rating_diffs / coeff)
    probs <- alpha / (1 + alpha)
    probs
  }
  ### compute max probability subject to probabilities sum of one (proper distribution)
  max_prob <- uniroot(f = function(p){sum(probs_from_max(p)) - 1}, interval = c(0.0001,0.9999))
  ### return prob vector subject to proper max probability
  probs <- probs_from_max(max_prob$root)
  probs / sum(probs)
}

### the following function does the same,
###  but is based on the assumption that the rating difference 
### determines the prob ratio instead of the odds ratio
ratings2probs_RR <- function(ratings, coeff = 173.7){
  if(length(ratings) == 0){return(1)}
  ### compute rating difference with respect to maximum rating
  rating_diffs <- ratings - max(ratings)
  ### probs are proportional to the exponential of the rating diffs
  probs <- exp(rating_diffs / coeff)
  probs / sum(probs)
}

### function to compute first pick distirbution given setdata with ratings attached

compute_firstpicks <- function(rated_setdata, samplesize = 100000){
  ### cut down card rating vector
  elig_cards <- rated_setdata$cards %>%  
    filter(!is.na(rating)) %>% 
    .[["number"]]
  card_ratings <- rated_setdata$cards %>%  
    filter(!is.na(rating)) %>% 
    .[["rating"]]       
  names(card_ratings) <- elig_cards
  empty_probs <- setNames(rep(0, length(elig_cards)), elig_cards)
  
  ### generate boosters and make picks
  boosters <- sample_booster(rated_setdata, n = samplesize)
  card_pickprobs <- sapply(seq(dim(boosters)[2]), FUN = function(b_nr){
    booster_cards <- boosters[, b_nr]
    booster_probs <- empty_probs
    booster_probs[booster_cards] <- ratings2probs(card_ratings[booster_cards])
    booster_probs
  })
  
  ### summarize the results
  pickprobs_df <- data.frame(number = rownames(card_pickprobs),
                             p1p1 = rowSums(card_pickprobs) / samplesize,
                             stringsAsFactors = F,
                             row.names = NULL)
  
  mod_data <- rated_setdata
  mod_data$cards <- left_join(mod_data$cards, pickprobs_df, by = "number")
  mod_data
}

### function to simulate a sample booster and compute pick probabilities 
### for given setdata with ratings attached

crackapack <- function(rated_setdata){
  sample_booster <- compute_firstpicks(rated_setdata, samplesize = 1)$cards
  sample_booster %>% 
    filter(p1p1 > 0) %>% 
    select(name, rarity, rating, p1p1) %>% 
    arrange(rarity) %>% 
    mutate(p1p1 = sprintf("%.1f %%", 100 * p1p1)) %>% 
    rename(Name = name, Rarity = rarity, Rating = rating, P1P1 = p1p1)
}

### function to compute pick point distirbution of specific card based on
### given setdata with ratings attached

compute_pp_dist <- function(rated_setdata, fixcard, samplesize = 1000){
  ### cut down card rating vector
  elig_cards <- rated_setdata$cards %>%  
    filter(!is.na(rating)) %>% 
    .[["number"]]
  card_ratings <- rated_setdata$cards %>%  
    filter(!is.na(rating)) %>% 
    .[["rating"]]       
  names(card_ratings) <- elig_cards

  ### generate boosters
  boosters <- sample_booster(rated_setdata, n = samplesize, fixcard = fixcard)
  
  ### compute pick point distribution per booster
  ppd_bybooster <- sapply(seq(dim(boosters)[2]), FUN = function(b_nr){
    ppd <- numeric(0)
    booster_cards <- boosters[, b_nr] 
    repeat{
      ## compute card pick probabilities
      cp_probs <- ratings2probs(card_ratings[booster_cards])
      ## compute probability of fixcard being still in pack times being picked next
      ppd <- c(ppd, (1 - sum(ppd)) * cp_probs[fixcard])
      ## randomly pcik non-fixcard subject to pick_probs
      res_cards <- setdiff(names(cp_probs), fixcard)
      pick <- sample(res_cards, size = 1, replace = FALSE, prob = cp_probs[res_cards])
      booster_cards <- setdiff(booster_cards, pick)
      if(length(booster_cards) == 1) break
    }
    ## return complete pick point distribution
    setNames(c(ppd, 1 - sum(ppd)), NULL)
  })
    
  ### summarize the results
  rowSums(ppd_bybooster) / samplesize
}
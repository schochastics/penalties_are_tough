get_outcome_missed <- function(link) {
  doc <- read_html(link)
  goals <- get_simple_goals(doc)
  tbl <- tibble(
    taker       = doc %>% html_nodes("#sb-verschossene .sb-aktion-wechsel-ein .wichtig") %>% html_text() %>% str_squish(),
    taker_team  = doc %>% html_nodes("#sb-verschossene .sb-aktion-wappen a") %>% html_attr("title"),
    # outcome     = doc %>% html_nodes("#sb-verschossene .hide-for-small") %>% html_text() %>% str_squish() %>% str_to_lower(),
    time        = doc %>% html_nodes("#sb-verschossene .sb-sprite-uhr-klein") %>% html_attr("style") %>% str_remove("background-position: ")
    # goalie      = doc %>% html_nodes("#sb-verschossene .sb-aktion-wechsel-aus .wichtig")%>% html_text() %>% str_squish(),
    # goalie_team = doc %>% html_node(".sb-aktion-spielstand img") %>% html_attr("alt")
  )
  outcome <- doc %>%
    html_nodes("#sb-verschossene .hide-for-small") %>%
    html_text() %>%
    str_squish() %>%
    str_to_lower()
  goalie <- doc %>%
    html_nodes("#sb-verschossene .sb-aktion-wechsel-aus .wichtig") %>%
    html_text() %>%
    str_squish()
  goalie_team <- doc %>%
    html_node(".sb-aktion-spielstand img") %>%
    html_attr("alt")

  tbl <- tbl %>%
    mutate(
      outcome = ifelse(length(outcome) != nrow(tbl), "not scored", outcome),
      goalie = ifelse(length(goalie) != nrow(tbl), NA_character_, goalie),
      goalie_team = ifelse(length(goalie_team) != nrow(tbl), NA_character_, goalie_team)
    ) %>%
    select(taker, taker_team, outcome, time, goalie, goalie_team)

  if (nrow(tbl) == 0) {
    return(tbl)
  }

  tbl <- tbl %>%
    rowwise() %>%
    mutate(time = get_time(time))

  tbl$at_score <- "0:0"
  for (i in 1:nrow(tbl)) {
    idx <- which(parse_number(tbl$time[i]) <= parse_number(goals$time))
    tbl$at_score[i] <- goals$res[idx[1] - 1]
  }
  tbl
}

get_simple_goals <- function(doc) {
  time <- doc %>%
    html_nodes("#sb-tore .sb-sprite-uhr-klein") %>%
    html_attr("style") %>%
    str_remove("background-position: ")
  time <- c("0'", sapply(time, get_time, USE.NAMES = FALSE), "91'") %>% unlist()
  res <- doc %>%
    html_nodes("#sb-tore b") %>%
    html_text()
  res <- c("0:0", res, ifelse(length(res) == 0, "0:0", res[length(res)]))
  scorer <- doc %>%
    html_nodes("#sb-tore .sb-aktion-aktion") %>%
    html_text() %>%
    str_squish() %>%
    word(1, 1, sep = ",")
  if (all(is.na(scorer))) {
    scorer <- c(NA, NA)
  } else {
    scorer <- c(NA, scorer, NA)
  }
  # team <- doc %>% html_nodes("#sb-tore .sb-aktion-wappen img") %>% html_attr("alt")
  team <- doc %>%
    html_nodes("#sb-tore .sb-aktion-wappen a") %>%
    html_attr("title")
  team <- c(NA, team, NA)
  penalty <- doc %>%
    html_nodes("#sb-tore .sb-aktion-aktion") %>%
    html_text() %>%
    str_squish() %>%
    word(2, 2, sep = ",") %>%
    str_detect("enalty")
  if (all(is.na(penalty))) {
    penalty <- c(FALSE, FALSE)
  } else {
    penalty <- c(FALSE, penalty, FALSE)
  }
  home <- doc %>%
    html_node(".sb-heim .sb-vereinslink") %>%
    html_attr("title")
  away <- doc %>%
    html_node(".sb-gast .sb-vereinslink") %>%
    html_attr("title")
  game <- c(home, away)
  goalie_team <- game[3 - match(team, game)]
  goalie <- doc %>%
    html_nodes(".aufstellung-rueckennummer-name a") %>%
    html_attr("href") %>%
    word(2, sep = "/") %>%
    str_replace_all("-", " ") %>%
    str_to_title() %>%
    .[c(1, 12)]

  if (all(is.na(goalie))) {
    tabs <- tryCatch(doc %>% html_table() %>% .[2:3], error = function(e) vector("list", 3))
    if (length(tabs) == 2 & !(all(sapply(tabs, is.null)))) {
      goalie <- unlist(lapply(tabs, function(x) x$X2[x$X1 == "Goalkeeper"]))
    }
  }

  goalie <- goalie[match(goalie_team, game)]
  tibble(time, res, scorer, team, penalty, goalie, goalie_team)
}

get_shootout <- function(doc) {
  taker <- doc %>%
    html_nodes("#sb-elfmeterscheissen .sb-aktion-aktion") %>%
    html_text() %>%
    str_squish() %>%
    word(1, 1, sep = ",")
  team <- doc %>%
    html_nodes("#sb-elfmeterscheissen .sb-aktion-wappen a") %>%
    html_attr("title")
  outcome <- doc %>%
    html_nodes("#sb-elfmeterscheissen .sb-aktion-aktion") %>%
    html_text() %>%
    str_squish() %>%
    word(2, sep = ",") %>%
    str_trim() %>%
    str_to_lower()
  home <- doc %>%
    html_node(".sb-heim .sb-vereinslink") %>%
    html_attr("title")
  away <- doc %>%
    html_node(".sb-gast .sb-vereinslink") %>%
    html_attr("title")
  game <- c(home, away)
  goalie_team <- game[3 - match(team, game)]
  goalie <- doc %>%
    html_nodes(".aufstellung-rueckennummer-name a") %>%
    html_attr("href") %>%
    word(2, sep = "/") %>%
    str_replace_all("-", " ") %>%
    str_to_title() %>%
    .[c(1, 12)]

  if (all(is.na(goalie))) {
    tabs <- tryCatch(doc %>% html_table() %>% .[2:3], error = function(e) vector("list", 3))
    if (length(tabs) == 2) {
      goalie <- unlist(lapply(tabs, function(x) x$X2[x$X1 == "Goalkeeper"]))
    }
  }

  goalie <- goalie[match(goalie_team, game)]
  tibble(taker, taker_team = team, outcome, goalie, goalie_team, time = "Pens", at_score = NA_character_)
}

tm_get_matches <- function(league, year) {
  # url <- paste0("https://www.transfermarkt.com/fa-cup/startseite/pokalwettbewerb/",league,"?saison_id=",year)
  url <- paste0("https://www.transfermarkt.com/fa-cup/gesamtspielplan/pokalwettbewerb/", league, "/saison_id/", year)
  doc <- read_html(url)

  # sel <- html_table(doc)[[1]]$X2
  # sel <- str_extract_all(sel,"[0-9]{2}/[0-9]{2}")[[1]]
  # if(!paste0(str_sub(year,3,4),"/",str_sub(year+1,3,4))%in%sel |
  #    !as.character(year)%in%sel){
  #   return(tibble())
  # }
  # home <- doc %>% html_nodes(".verein-heim a") %>% html_attr("title") %>% .[!str_detect(.,"name_mittel")]
  # away <- doc %>% html_nodes(".verein-gast a") %>% html_attr("title") %>% .[!str_detect(.,"name_mittel")]
  home <- doc %>%
    html_nodes("table .text-right.no-border-rechts.hauptlink a") %>%
    html_attr("title") %>%
    str_trim()
  away <- doc %>%
    html_nodes(".large-8 .box table .no-border-links.hauptlink a") %>%
    html_attr("title") %>%
    str_trim()
  if (length(home) != length(away)) {
    away <- doc %>%
      html_nodes(".zentriert.hauptlink~ .hauptlink a") %>%
      html_attr("title") %>%
      str_trim()
  }
  result <- doc %>%
    html_nodes(".large-8 .box table .zentriert.hauptlink") %>%
    html_text() %>%
    str_squish()
  if (any(result == "postponed")) {
    id <- which(result == "postponed")
    result <- result[-id]
    home <- home[-id]
    away <- away[-id]
  }
  link <- doc %>%
    html_nodes(".large-8 .box table .zentriert.hauptlink a") %>%
    html_attr("href") %>%
    paste0("https://www.transfermarkt.com", .)

  tibble(home, away, result, link)
}

tm_pen_match <- function(url) {
  doc <- read_html(url)
  round <- doc %>%
    html_node(".sb-datum") %>%
    html_text() %>%
    str_squish() %>%
    word(1, sep = "\\|") %>%
    str_trim()
  # scored
  goals <- get_simple_goals(doc)
  scored <- goals %>%
    dplyr::filter(penalty) %>%
    mutate(taker = as.character(scorer), taker_team = team, outcome = "scored") %>%
    select(-penalty, -scorer, -team, taker, taker_team, outcome, goalie, goalie_team, time, at_score = res)
  # missed
  missed <- get_outcome_missed(url)
  # shootout
  shootout <- get_shootout(doc)
  pens <- bind_rows(scored, missed, shootout)
  pens$matchday <- round
  pens
}

get_time <- function(txt) {
  num <- str_extract_all(txt, "[-]*[0-9]+")[[1]]
  xoffset <- num[2]
  yoffset <- num[1]
  if (xoffset == "-0") {
    xoffset <- "0"
  } else if (yoffset == "-0") {
    yoffset <- "0"
  }
  x <- -seq(0, 9 * 36, 36)
  y <- -seq(0, 11 * 36, 36)
  grid <- expand_grid(y, x)
  grid <- cbind(grid, 1:120)
  time <- which(grid[, 1] == xoffset & grid[, 2] == yoffset)
  if (length(time) == 0) {
    return("-")
  } else {
    return(paste0(time, "'"))
  }
}


pen_tm_detail <- function(league, year) {
  found_missed <- TRUE
  found_scored <- TRUE
  found <- found_missed | found_scored
  k <- 0
  missed_tbl <- tibble(
    matchday = integer(0), game = character(0), result = character(0),
    taker = character(0), taker_team = character(0), outcome = character(0),
    goalie = character(0), goalie_team = character(0)
  )
  scored_tbl <- tibble(
    matchday = integer(0), game = character(0), result = character(0),
    taker = character(0), taker_team = character(0), outcome = character(0),
    goalie = character(0), goalie_team = character(0)
  )

  while (found) {
    k <- k + 1
    cat(k, "\r")
    url <- paste0("https://www.transfermarkt.com/premier-league/elfmeterstatistik/wettbewerb/", league, "/saison_id/", year, "/plus/1/page/", k)
    doc <- read_html(url)
    if (k == 1) {
      mpages <- doc %>%
        html_nodes(".tm-pagination__link") %>%
        html_attr("title") %>%
        str_extract("[0-9]+") %>%
        as.numeric()
      page_max <- mpages[which(is.na(mpages)) - 1]
      if (length(page_max) == 0) {
        page_max <- 1
      }
      if (length(page_max) == 1) {
        page_max <- c(1, page_max)
      }
      if (page_max[2] == 10) {
        page_max[2] <- max(c(mpages[which(is.na(mpages))[2] + 1], 10), na.rm = TRUE)
      }
    }
    tables <- doc %>% html_table()
    idx <- which(map_lgl(tables, function(x) any(str_detect(names(x), "Matchday"))))
    if (length(idx) == 1) {
      found_missed <- FALSE
    }
    if (k > page_max[1]) {
      found_missed <- FALSE
    }
    if (k > page_max[2]) {
      found_scored <- FALSE
    }
    if (found_missed) {
      missed <- tables %>%
        .[[idx[1]]] %>%
        janitor::clean_names()
      id1 <- seq(1, nrow(missed), 3)
      id2 <- seq(2, nrow(missed), 3)
      id3 <- seq(3, nrow(missed), 3)
      hteam <- doc %>%
        html_nodes("#yw1 .no-border-rechts:nth-child(8)") %>%
        html_nodes("a") %>%
        html_attr("title")
      ateam <- doc %>%
        html_nodes("#yw1 .no-border-links+ .no-border-links") %>%
        html_nodes("a") %>%
        html_attr("title")
      team <- doc %>%
        html_nodes("#yw1 .no-border-rechts:nth-child(2)") %>%
        html_nodes("a") %>%
        html_attr("title")
      links <- doc %>%
        html_nodes("#yw1 .ergebnis-link") %>%
        html_attr("href") %>%
        paste0("https://www.transfermarkt.com", .) %>%
        unique()
      outcome_tbl <- map_dfr(links, get_outcome_missed)
      missed_clean <- tibble(
        matchday = missed$matchday[id1], game = paste(hteam, ateam, sep = " - "), result = missed$result_2[id1],
        taker = missed$wappen[id2], taker_team = team, time = missed$wappen_3[id1], at_score = missed$x[id1],
        goalie = missed$at_a_score[id1]
      ) %>%
        mutate(goalie_team = str_remove(game, paste0(team)) %>% str_remove(" - "))

      missed_clean <- left_join(missed_clean, outcome_tbl,
        by = c("taker", "taker_team", "time", "goalie", "goalie_team")
      ) %>%
        mutate(at_score = at_score.x) %>%
        select(-at_score.y, -at_score.x)

      missed_tbl <- bind_rows(missed_tbl, missed_clean)
    }
    if (found_scored) {
      scored <- doc %>%
        html_table() %>%
        .[[idx[length(idx)]]] %>%
        janitor::clean_names()
      id1 <- seq(1, nrow(scored), 3)
      id2 <- seq(2, nrow(scored), 3)
      id3 <- seq(3, nrow(scored), 3)
      hteam <- doc %>%
        html_nodes("#yw2 .no-border-rechts:nth-child(8)") %>%
        html_nodes("a") %>%
        html_attr("title")
      ateam <- doc %>%
        html_nodes("#yw2 .no-border-links+ .no-border-links") %>%
        html_nodes("a") %>%
        html_attr("title")
      team <- doc %>%
        html_nodes("#yw2 .no-border-rechts:nth-child(2)") %>%
        html_nodes("a") %>%
        html_attr("title")

      scored_clean <- tibble(
        matchday = scored$matchday[id1], game = paste(hteam, ateam, sep = " - "), result = scored$result_2[id1],
        taker = scored$wappen[id2], taker_team = team, time = scored$wappen_3[id1], at_score = scored$x[id1],
        outcome = "scored", goalie = scored$at_a_score[id1]
      ) %>%
        mutate(goalie_team = str_remove(game, paste0(team)) %>% str_remove(" - "))

      scored_tbl <- bind_rows(scored_tbl, scored_clean)
    }
    found <- found_missed | found_scored
  }
  bind_rows(scored_tbl, missed_tbl) %>% arrange(matchday)
}


collate_files <- function(dir) {
  fl <- list.files(dir, full.names = TRUE)
  tmp <- map_dfr(fl, read_csv, show_col_types = FALSE)
  write_csv(tmp, paste0(dir, ".csv"))
  invisible(tmp)
}

scrape_penalties <- function(year = 2021) {
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(rvest))
  leagues <- read_csv("raw_data/penalties/leagues.csv", show_col_types = FALSE)

  for (i in seq_len(nrow(leagues))) {
    dir <- paste0(str_to_lower(leagues$Code[i]), "-", leagues$League[i])
    cat(dir, " ", year, "\n")
    season_string <- paste0(year, "-", str_sub(year + 1, 3, 4))
    outfile <- paste0("raw_data/penalties/", dir, "/", season_string, ".csv")
    # if(file.exists(outfile)){
    #   next()
    # }
    pen_tbl <- tryCatch(pen_tm_detail(leagues$Code[i], year) %>%
      mutate(season = season_string), error = function(e) tibble())
    write_csv(pen_tbl, outfile)
  }
}

scrape_penalties_cup <- function(cup_file, year = 2021) {
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(rvest))
  leagues <- read_csv(cup_file, show_col_types = FALSE, progress = FALSE)

  for (i in seq_len(nrow(leagues))) {
    dir <- paste0(str_to_lower(leagues$Code[i]), "-", leagues$League[i])
    if (!dir.exists(paste0("raw_data/penalties/", dir))) {
      dir.create(paste0("raw_data/penalties/", dir))
    }
    season_string <- paste0(year, "-", str_sub(year + 1, 3, 4))
    outfile <- paste0("raw_data/penalties/", dir, "/", season_string, ".csv")
    if (file.exists(outfile)) {
      system(paste("rm", outfile))
    }
    games <- tm_get_matches(leagues$Code[i], year)
    if (nrow(games) == 0) {
      next()
    }
    for (j in 1:nrow(games)) {
      cat(dir, " ", year, ":", format(j / nrow(games), digits = 2, nsmall = 2), "\r")
      pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
      k <- 1
      while ("error" %in% names(pens) & k < 5) {
        pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
        k <- k + 1
        # Sys.sleep(runif(1,54,65))
      }
      if ("error" %in% names(pens)) {
        next()
      }
      if (nrow(pens) == 0) {
        next()
      }
      pens$result <- games$result[j]
      pens$game <- paste0(games$home[j], " - ", games$away[j])
      pens <- pens %>%
        select(matchday, game, result, taker, taker_team, outcome, goalie, goalie_team, time, at_score) %>%
        mutate(season = season_string)

      write_csv(pens, outfile, append = file.exists(outfile), progress = FALSE)
    }
    cat("\n---\n")
  }
}
# national teams scrapers ----
tm_get_int_matches <- function(url) {
  doc <- read_html(url)
  home <- doc %>%
    html_nodes("table .text-right.no-border-rechts.hauptlink a") %>%
    html_attr("title") %>%
    str_trim()
  away <- doc %>%
    html_nodes(".large-8 .box table .no-border-links.hauptlink a") %>%
    html_attr("title") %>%
    str_trim()
  if (length(home) != length(away)) {
    away <- doc %>%
      html_nodes(".zentriert.hauptlink~ .hauptlink a") %>%
      html_attr("title") %>%
      str_trim()
  }
  result <- doc %>%
    html_nodes(".large-8 .box table .zentriert.hauptlink") %>%
    html_text() %>%
    str_squish()
  if (any(result == "postponed")) {
    id <- which(result == "postponed")
    result <- result[-id]
    home <- home[-id]
    away <- away[-id]
  }
  link <- doc %>%
    html_nodes(".large-8 .box table .zentriert.hauptlink a") %>%
    html_attr("href") %>%
    paste0("https://www.transfermarkt.com", .)

  tibble(home, away, result, link)
}

scrape_wm <- function() {
  years <- c(1930, 1934, 1938, seq(1950, 2022, 4))
  years_short <- str_sub(years, start = 3, end = 4)
  urls <- paste0("https://www.transfermarkt.com/world-cup-", years, "/gesamtspielplan/pokalwettbewerb/WM", years_short)
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat(url, "\n")
    games <- tm_get_int_matches(url)
    outfile <- paste0("raw_data/penalties/int-wm/", years[i], ".csv")
    for (j in 1:nrow(games)) {
      cat(years[i], ":", format(j / nrow(games), digits = 2, nsmall = 2), "\r")
      pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
      k <- 1
      while ("error" %in% names(pens) & k < 5) {
        pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
        k <- k + 1
        # Sys.sleep(runif(1,54,65))
      }
      if ("error" %in% names(pens)) {
        stop(paste0("error in ", games$link[j]))
      }
      if (nrow(pens) == 0) {
        next()
      }
      pens$result <- games$result[j]
      pens$game <- paste0(games$home[j], " - ", games$away[j])
      pens <- pens %>%
        select(matchday, game, result, taker, taker_team, outcome, goalie, goalie_team, time, at_score) %>%
        mutate(season = years[i])

      write_csv(pens, outfile, append = file.exists(outfile), progress = FALSE)
    }
  }
}

scrape_em <- function() {
  years <- seq(1960, 2020, 4)
  years_short <- str_sub(years, start = 3, end = 4)
  urls <- paste0("https://www.transfermarkt.com/euro-", years, "/gesamtspielplan/pokalwettbewerb/EM", years_short)
  for (i in seq_along(urls)) {
    url <- urls[i]
    cat(url, "\n")
    games <- tm_get_int_matches(url)
    outfile <- paste0("raw_data/penalties/int-em/", years[i], ".csv")
    for (j in 1:nrow(games)) {
      cat(years[i], ":", format(j / nrow(games), digits = 2, nsmall = 2), "\r")
      pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
      k <- 1
      while ("error" %in% names(pens) & k < 5) {
        pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
        k <- k + 1
        # Sys.sleep(runif(1,54,65))
      }
      if ("error" %in% names(pens)) {
        stop(paste0("error in ", games$link[j]))
      }
      if (nrow(pens) == 0) {
        next()
      }
      pens$result <- games$result[j]
      pens$game <- paste0(games$home[j], " - ", games$away[j])
      pens <- pens %>%
        select(matchday, game, result, taker, taker_team, outcome, goalie, goalie_team, time, at_score) %>%
        mutate(season = years[i])

      write_csv(pens, outfile, append = file.exists(outfile), progress = FALSE)
    }
  }
}

scrape_nation_league <- function(year = 2018) {
  suppressPackageStartupMessages(require(tidyverse))
  suppressPackageStartupMessages(require(rvest))
  leagues <- data.frame(
    Code = c("UNLA", "UNLB", "UNLC", "UNLD", "UNFI"),
    League = c(
      "uefa-nations-league-a", "uefa-nations-league-b",
      "uefa-nations-league-c", "uefa-nations-league-d",
      "uefa-nations-league-finals"
    )
  )

  for (i in seq_len(nrow(leagues))) {
    dir <- paste0("int-nations-league/", str_to_lower(leagues$Code[i]), "-", leagues$League[i])
    if (!dir.exists(paste0("raw_data/penalties/", dir))) {
      dir.create(paste0("raw_data/penalties/", dir))
    }
    season_string <- paste0(year, "-", str_sub(year + 1, 3, 4))
    outfile <- paste0("raw_data/penalties/", dir, "/", season_string, ".csv")
    if (file.exists(outfile)) {
      system(paste("rm", outfile))
    }
    games <- tm_get_matches(leagues$Code[i], year)
    for (j in 1:nrow(games)) {
      cat(dir, " ", year, ":", format(j / nrow(games), digits = 2, nsmall = 2), "\r")
      pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
      k <- 1
      while ("error" %in% names(pens) & k < 5) {
        pens <- tryCatch(tm_pen_match(games$link[j]), error = function(e) tibble(error = "error"))
        k <- k + 1
        # Sys.sleep(runif(1,54,65))
      }
      if ("error" %in% names(pens)) {
        stop(paste0("error in ", games$link[j]))
      }
      if (nrow(pens) == 0) {
        next()
      }
      pens$result <- games$result[j]
      pens$game <- paste0(games$home[j], " - ", games$away[j])
      pens <- pens %>%
        select(matchday, game, result, taker, taker_team, outcome, goalie, goalie_team, time, at_score) %>%
        mutate(season = season_string)

      write_csv(pens, outfile, append = file.exists(outfile), progress = FALSE)
    }
    cat("\n---\n")
  }
}

# library(tidyverse)
# library(rvest)
# dat <- read_csv("raw_data/penalties/domestic.csv")
# cups <- dat |> dplyr::filter(type=="cup")
# res <- tibble()
# for(i in 1:nrow(cups)){
#   cdir <- paste0("raw_data/penalties/",str_to_lower(paste0(cups$Code[i],"-",cups$Name[i])))
#   seasons <- list.files(cdir)
#   years <- str_extract(seasons,"[0-9]{4}")
#   for(y in years){
#     cat(i,"-",y,"\n")
#     games <- tm_get_matches(cups$Code[i],y)
#     tmp <- tibble(Code=cups$Code[i],Name=cups$Name[i],Country=cups$Country[i],Type=cups$type[i],Season=y,games=nrow(games))
#     res <- bind_rows(res,tmp)
#   }
# }
# write_csv(res,"../tmp/all_cup_games.csv")
#
# leagues <- dat |> dplyr::filter(type=="league")
#
# get_teams <- function(code,y){
#   tbl <- paste0("https://www.transfermarkt.com/premier-league/tabelle/wettbewerb/",code,"/saison_id/",y) |>
#     read_html() |>
#     html_nodes(".responsive-table") |>
#     html_table()
#   nrow(tbl[[1]])
# }
#
# res1 <- tibble()
# for(i in 1:nrow(leagues)){
#   cdir <- paste0("raw_data/penalties/",str_to_lower(paste0(leagues$Code[i],"-",leagues$Name[i])))
#   seasons <- list.files(cdir)
#   years <- str_extract(seasons,"[0-9]{4}")
#   for(y in years){
#     if(as.numeric(y)<2008){
#       next()
#     }
#     cat(i,"-",y,"\n")
#     teams <- get_teams(leagues$Code[i],y)
#     tmp <- tibble(Code=leagues$Code[i],Name=leagues$Name[i],Country=leagues$Country[i],
#                   Type=leagues$type[i],Season=y,games=choose(teams,2)*2)
#     res1 <- bind_rows(res1,tmp)
#   }
# }
#
# years <- c(1930,1934,1938,seq(1950,2018,4))
# years_short <- str_sub(years,start = 3,end = 4)
# urls <- paste0("https://www.transfermarkt.com/world-cup-",years,"/gesamtspielplan/pokalwettbewerb/WM",years_short)
# games <- sapply(urls,function(x) nrow(tm_get_int_matches(x)))
# reswm <- tibble(Code="WM",Name="World Cup",Country="World",type="International Cup",Season=years,games=unname(games))
#
# years <- seq(1960,2020,4)
# years_short <- str_sub(years,start = 3,end = 4)
# urls <- paste0("https://www.transfermarkt.com/euro-",years,"/gesamtspielplan/pokalwettbewerb/EM",years_short)
# games <- sapply(urls,function(x) nrow(tm_get_int_matches(x)))
# resem <- tibble(Code="EM",Name="European Championship",Country="Europe",type="International Cup",Season=years,games=unname(games))
# write_csv(bind_rows(reswm,resem),"../tmp/all_intcup_games.csv")
#
# nation <- data.frame(Code=c("UNLA","UNLB","UNLC","UNLD","UNFI"),
#                      Name = c("uefa-nations-league-a","uefa-nations-league-b",
#                               "uefa-nations-league-c","uefa-nations-league-d",
#                               "uefa-nations-league-finals"),
#                      Country="Europe",type="International Cup",season=rep(c(2018,2020),each=5)) |>
#   rowwise() |>
#   mutate(games=nrow(tm_get_matches(Code,season)))
# write_csv(nation,"../tmp/all_nation_games.csv")
#
# euro <- dat |> dplyr::filter(type=="euro cup")
# res2 <- tibble()
# for(i in 1:nrow(euro)){
#   cdir <- paste0("raw_data/penalties/",str_to_lower(paste0(euro$Code[i],"-",euro$Name[i])))
#   seasons <- list.files(cdir)
#   years <- str_extract(seasons,"[0-9]{4}")
#   for(y in years){
#     cat(i,"-",y,"\n")
#     games <- tm_get_matches(euro$Code[i],y)
#     tmp <- tibble(Code=euro$Code[i],Name=euro$Name[i],Country=euro$Country[i],Type=euro$type[i],Season=y,games=nrow(games))
#     res2 <- bind_rows(res2,tmp)
#   }
# }
#
# names(reswm)[4] <- names(resem)[4] <- names(nation)[4] <- "Type"
# names(nation)[5] <- "Season"
# write_csv(rbind(res,res1,res2,nation,resem,reswm),"raw_data/penalties/game_count.csv")
#
# df <- read_csv("raw_data/penalties/game_count.csv")
# df$new <- NA
# get_no_games <- function(code,name,season,type){
#   if(type=="league"){
#     url <- paste0("https://www.transfermarkt.ch/super-league/gesamtspielplan/wettbewerb/",code,"?saison_id=",season)
#   } else{
#     url <- paste0("https://www.transfermarkt.ch/copa-del-rey/gesamtspielplan/pokalwettbewerb/",code,"?saison_id=",season)
#   }
#   doc <- read_html(url)
#   doc |> html_nodes(".ergebnis-link") |> length()
# }
#
# for(i in 1:nrow(df)){
#   cat(i,"\n")
#   if(is.na(df$new[i])){
#     df$new[i] <- get_no_games(code = df$Code[i],name = df$Name[i],season = df$Season[i],df$Type[i])
#   }
# }
#
# df <- read_csv("~/Downloads/wrong_rates.csv") |>
#   dplyr::filter(scored>=95) |>
#   rename(league=1) |>
#   separate(league,sep = "/",into = c("a","code","year")) |>
#   mutate(code=word(code,1,sep="-") |> str_to_upper()) |>
#   mutate(year=word(year,1,sep="-"))
#
# leagues <- read_csv("raw_data/penalties/leagues.csv",show_col_types = FALSE)
# for(i in 1:nrow(df)){
#   j <- which(leagues$Code==df$code[i])
#   year <- as.numeric(df$year[i])
#   dir <- paste0(str_to_lower(leagues$Code[j]),"-",leagues$League[j])
#   cat(dir," ",year,"\n")
#   season_string <- paste0(year,"-",str_sub(year+1,3,4))
#   outfile <- paste0("raw_data/penalties/",dir,"/",season_string,".csv")
#   # if(file.exists(outfile)){
#   #   next()
#   # }
#   pen_tbl <- suppressWarnings(tryCatch(pen_tm_detail(leagues$Code[j],year) %>%
#                                          mutate(season=season_string),error=function(e) tibble()))
#   write_csv(pen_tbl,outfile)
# }

#
# get_no_extras <- function(code,name,season,type){
#   if(type=="league"){
#     url <- paste0("https://www.transfermarkt.ch/super-league/gesamtspielplan/wettbewerb/",code,"?saison_id=",season)
#   } else if(type=="International Cup"){
#     url <- paste0("https://www.transfermarkt.ch/copa-del-rey/gesamtspielplan/pokalwettbewerb/",code,str_sub(season,3,4))
#   } else{
#     url <- paste0("https://www.transfermarkt.ch/copa-del-rey/gesamtspielplan/pokalwettbewerb/",code,"?saison_id=",season)
#   }
#   doc <- read_html(url)
#   extra <- doc |> html_nodes(".ergebnis_zusatz") |> html_text() |> str_trim()
#   cat("-",unique(extra),"\n")
#   c(sum(extra=="n.V."),sum(extra=="n.E."))
# }
# df <- read_csv("raw_data/penalties/game_count1.csv")
# df <- df |> select(-games) |>
#   rename(No_games=new_games) |>
#   mutate(No_extra=NA,No_penalty=NA)
#
# for(i in 1:nrow(df)){
#   cat(i,"\n")
#   res <- tryCatch(get_no_extras(code = df$Code[i],name = df$Name[i],season = df$Season[i],type = df$Type[i]),error = function(e) c(NA,NA))
#   df$No_extra[i] <- res[1]
#   df$No_penalty[i] <- res[2]
# }

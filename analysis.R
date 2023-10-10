####################################### Part 5 plots and figures paper ##############################################################

####################################### Penalty Shootouts are Different ##############################################################

###################################### load environment ##############################################################

library("ggplot2")
library("RColorBrewer")
library("ggpubr")
library("gridExtra")
library("zoo")
library("gridtext")
library("stringr")
library("xtable")

load(file = "data/SaD_D2.1_3.RData") # no tests
data_final_D2.1 <- data_final
data_regular_D2.1 <- data_regular
data_regular_90_D2.1 <- data_regular_D2.1[data_regular_D2.1$TypePhaseName %in% c("Second half", "First half"), ]
data_regular_120_D2.1 <- data_regular_D2.1[data_regular_D2.1$TypePhaseName %in% c("2nd Extra Time", "1st Extra Time"), ]
data_shootout_D2.1 <- data_shootout
rm(data_final, data_regular, data_shootout)

load(file = "data/SaD_D2.2_3.RData") # binom
data_final_D2.2 <- data_final
data_regular_D2.2 <- data_regular
data_regular_90_D2.2 <- data_regular_D2.2[data_regular_D2.2$TypePhaseName %in% c("Second half", "First half"), ]
data_regular_120_D2.2 <- data_regular_D2.2[data_regular_D2.2$TypePhaseName %in% c("2nd Extra Time", "1st Extra Time"), ]
data_shootout_D2.2 <- data_shootout
rm(data_final, data_regular, data_shootout)

load(file = "data/SaD_D2.3_3.RData") # multinom
data_final_D2.3 <- data_final
data_regular_D2.3 <- data_regular
data_regular_90_D2.3 <- data_regular_D2.3[data_regular_D2.3$TypePhaseName %in% c("Second half", "First half"), ]
data_regular_120_D2.3 <- data_regular_D2.3[data_regular_D2.3$TypePhaseName %in% c("2nd Extra Time", "1st Extra Time"), ]
data_shootout_D2.3 <- data_shootout
rm(data_final, data_regular, data_shootout)

load(file = "data/SaD_D1.1_D1.2.RData") # only poisson
for (i in seq_along(foldernames_TM)) {
    id_tourn <- startsWith(TM_data_1.2$file, substr(foldernames_TM[i], 58, nchar(foldernames_TM[i])))
    TM_data_1.2[id_tourn, "TournamentName"] <- substr(foldernames_TM[i], 58, nchar(foldernames_TM[i]))
}
data_final_D2.4 <- TM_data_1.2
data_regular_D2.4 <- TM_data_1.2[TM_data_1.2$TypePhaseName != "Penalty Shoot-Out", ]
data_regular_90_D2.4 <- data_regular_D2.4[data_regular_D2.4$TypePhaseName %in% c("Second half", "First half"), ]
data_regular_120_D2.4 <- data_regular_D2.4[data_regular_D2.4$TypePhaseName %in% c("2nd Extra Time", "1st Extra Time"), ]
data_shootout_D2.4 <- TM_data_1.2[TM_data_1.2$TypePhaseName == "Penalty Shoot-Out", ]
rm(TM_data_1.2, id_tourn)

outcome_function <- function(data, digits = 2) {
    if (!"outcome_binom" %in% colnames(data)) { # D2.1 or D2.3
        return(round(table(data$EventTypeName) / sum(table(data$EventTypeName)) * 100, digits = digits))
    } else { # D2.2
        return(round(table(data$EventTypeName) / sum(table(data$EventTypeName)) * 100, digits = digits)[1])
    }
} # calculate conversion rates

##########  Table 1 - Competitions ############

# output table tournaments
length(unique(data_final_D2.1$TournamentName)) # 115 tournaments

data_tournaments <- data.frame(matrix(NA, ncol = 13, nrow = length(unique(data_final_D2.1$TournamentName))))
colnames(data_tournaments) <- c(
    "country",
    "penalties_D1", "regulation_D1", "extra_time_D1", "shootout_D1",
    "penalties_D2", "regulation_D2", "extra_time_D2", "shootout_D2",
    "penalties_D3", "regulation_D3", "extra_time_D3", "shootout_D3"
)
rownames(data_tournaments) <- unique(data_final_D2.1$TournamentName)

for (i in seq_along(unique(data_final_D2.1$TournamentName))) {
    tournament <- rownames(data_tournaments)[i]

    data_tournaments[i, "country"] <- unique(data_final_D2.1[data_final_D2.1$TournamentName == tournament, "country"])
    data_tournaments[i, "penalties_D1"] <- nrow(data_final_D2.4[data_final_D2.4$TournamentName == tournament, ])
    data_tournaments[i, "regulation_D1"] <- nrow(data_regular_90_D2.4[data_regular_90_D2.4$TournamentName == tournament, ])
    data_tournaments[i, "extra_time_D1"] <- nrow(data_regular_120_D2.4[data_regular_120_D2.4$TournamentName == tournament, ])
    data_tournaments[i, "shootout_D1"] <- nrow(data_shootout_D2.4[data_shootout_D2.4$TournamentName == tournament, ])

    data_tournaments[i, "penalties_D2"] <- nrow(data_final_D2.2[data_final_D2.2$TournamentName == tournament, ])
    data_tournaments[i, "regulation_D2"] <- nrow(data_regular_90_D2.2[data_regular_90_D2.2$TournamentName == tournament, ])
    data_tournaments[i, "extra_time_D2"] <- nrow(data_regular_120_D2.2[data_regular_120_D2.2$TournamentName == tournament, ])
    data_tournaments[i, "shootout_D2"] <- nrow(data_shootout_D2.2[data_shootout_D2.2$TournamentName == tournament, ])

    data_tournaments[i, "penalties_D3"] <- nrow(data_final_D2.3[data_final_D2.3$TournamentName == tournament, ])
    data_tournaments[i, "regulation_D3"] <- nrow(data_regular_90_D2.3[data_regular_90_D2.3$TournamentName == tournament, ])
    data_tournaments[i, "extra_time_D3"] <- nrow(data_regular_120_D2.3[data_regular_120_D2.3$TournamentName == tournament, ])
    data_tournaments[i, "shootout_D3"] <- nrow(data_shootout_D2.3[data_shootout_D2.3$TournamentName == tournament, ])
}

order_tournaments <- order(data_tournaments$penalties_D1, decreasing = TRUE)

data_tournaments <- data_tournaments[order_tournaments, ]
nrow(data_tournaments)
data_tournaments <- cbind(
    "tournament" = apply(data.frame(str_split(str_split(rownames(data_tournaments),
        pattern = "/", simplify = T
    )[, 2], pattern = "-", simplify = T)[, -1]), 1, paste, collapse = " "),
    data_tournaments
)


data_tournaments["sum", ] <- c("sum", NA, colSums(data_tournaments[, !colnames(data_tournaments) %in% c("tournament", "country")]))

data_tournaments$tournament <- c(
    "Russian Cup", "Coupe de France", "EFL Cup", "Serie A", "La Liga",
    "Turkish Cup", "Süper Lig", "Liga Portugal", "Norwegian Football Cup", "Ligue 1",
    "DFB-Pokal", "Poland Ekstraklasa", "Bundesliga", "Belgian Pro League", "Coppa Italia",
    "Polish Cup", "Premier League", "Liga 1", "Russian Premier League", "Copa del Rey",
    "Serbian Super League", "Taça de Portugal", "Eredivisie", "Austrian Cup", "Nemzeti Bajnokság",
    "FA Cup", "Super League Greece 1", "Ukrainian Cup", "Europa League", "National Division",
    "Croatian football league", "Scottish Premiership", "KNVB Beker", "Ukrainian Premier League", "Swiss Super League",
    "Scottish Cup", "Danish Cup", "Maltese Premier League", "Czech Cup", "NIFL Irish Premiership",
    "Meistriliiga", "Slovak Cup", "Greek Football Cup", "Finnish Cup", "Bosnia and Herzegovina Football Cup",
    "Belarusian Cup", "Czech First League", "Israeli Premier League", "Champions League", "Austrian Football Bundesliga",
    "Serbian Cup", "Estonian Cup", "Belgian Cup", "Croatia Cup", "League of Ireland Premier Division",
    "Kazakh Premier League", "Kategoria Superiore", "Veikkausliiga", "Allsvenskan", "Macedonian First League",
    "Cypriot First Division", "Swiss Cup", "Bulgarian First League", "Hungarian Cup", "Liga 12",
    "Romanian Cup", "Belarusian Premier League", "Latvian Higher League", "Bulgarian Cup", "Eliteserien",
    "Faroe Islands Premier League", "Men's Best Division", "Slovenian PrvaLiga", "Israel State Cup", "Danish Superliga",
    "Svenska Cupen", "Armenian Premier League", "1st League", "Azerbaijan Premier League", "Luxembourg Cup",
    "Erovnuli Liga", "A Lyga", "Football Superleague of Kosovo", "FAW Welsh Cup", "First League of Montenegro",
    "Georgian Cup", "Cymru Premier", "Icelandic Men's Football Cup", "Faroe Islands Cup", "FIFA World Cup Final",
    "FAI Cup", "Kazakhstan Cup", "Sammarinese Football Championship", "FA Trophy", "Nations League",
    "Kosovar Cup", "Super Liga", "Slovenian Football Cup", "Albanian Cup", " Primera Divisió",
    "European Championship", "Armenian Cup", "Rock Cup", "Gibraltar Football League", "Cypriot Cup",
    "Conference League", "Moldovan Cup", "Latvian Football Cup", "Irish League Cup", "Coppa Titano",
    "Liechtenstein Football Cup", "Azerbaijan Cup", "Lithuanian Football Federation Cup", "Super Cup", "Copa Constitució", "sum"
)

print(xtable::xtable(data_tournaments[, 1:6], type = "latex", ), include.rownames = FALSE)
print(xtable::xtable(data_tournaments[, c(1, 2, 7:10)], type = "latex", ), include.rownames = FALSE)
print(xtable::xtable(data_tournaments[, c(1, 2, 11:14)], type = "latex", ), include.rownames = FALSE)

write.csv(data_tournaments, "processed_data/tab.9-11.csv")


#### players #####

length(unique(data_final_D2.2$player_name))
length(unique(data_final_D2.2$goalie_name))
sum(data_final_D2.2$goalie_name == "dummy_goalie")

length(unique(data_final_D2.3$player_name))
length(unique(data_final_D2.3$goalie_name))
sum(data_final_D2.3$goalie_name == "dummy_goalie")

##########  Figure 1  ############

fig_1 <- matrix(NA, ncol = 12, nrow = 1)
colnames(fig_1) <- c(
    "goal_D2_regulation", "goal_D2_extra", "goal_D2_shootout",
    "goal_D3_regulation", "goal_D3_extra", "goal_D3_shootout",
    "save_D3_regulation", "save_D3_extra", "save_D3_shootout",
    "miss_D3_regulation", "miss_D3_extra", "miss_D3_shootout"
)
rownames(fig_1) <- "rate"

fig_1[1, "goal_D2_regulation"] <- outcome_function(data_regular_90_D2.2)
fig_1[1, "goal_D2_extra"] <- outcome_function(data_regular_120_D2.2)
fig_1[1, "goal_D2_shootout"] <- outcome_function(data_shootout_D2.2)
fig_1[1, "goal_D3_regulation"] <- outcome_function(data_regular_90_D2.3)["Goal"]
fig_1[1, "goal_D3_extra"] <- outcome_function(data_regular_120_D2.3)["Goal"]
fig_1[1, "goal_D3_shootout"] <- outcome_function(data_shootout_D2.3)["Goal"]
fig_1[1, "save_D3_regulation"] <- outcome_function(data_regular_90_D2.3)["Save"]
fig_1[1, "save_D3_extra"] <- outcome_function(data_regular_120_D2.3)["Save"]
fig_1[1, "save_D3_shootout"] <- outcome_function(data_shootout_D2.3)["Save"]
fig_1[1, "miss_D3_regulation"] <- outcome_function(data_regular_90_D2.3)["Missed"]
fig_1[1, "miss_D3_extra"] <- outcome_function(data_regular_120_D2.3)["Missed"]
fig_1[1, "miss_D3_shootout"] <- outcome_function(data_shootout_D2.3)["Missed"]

write.csv(fig_1, "processed_data/fig.1.csv")



##########  tab 2  ############


#### leagues ####
top_leagues <- c("leagues/es1-laliga", "leagues/fr1-ligue-1", "leagues/gb1-premier-league", "leagues/l1-bundesliga", "leagues/it1-serie-a")

data_leagues_reg_D2.3 <- data_regular_90_D2.3[data_regular_90_D2.3$CompetitionShortName == "leag", ]

data_leagues_reg_top5_D2.3 <- data_leagues_reg_D2.3[data_leagues_reg_D2.3$TournamentName %in% top_leagues, ]
data_leagues_reg_other_D2.3 <- data_leagues_reg_D2.3[!data_leagues_reg_D2.3$TournamentName %in% top_leagues, ]

data_leagues_reg_D2.2 <- data_regular_90_D2.2[data_regular_90_D2.2$CompetitionShortName == "leag", ]

data_leagues_reg_top5_D2.2 <- data_leagues_reg_D2.2[data_leagues_reg_D2.2$TournamentName %in% top_leagues, ]
data_leagues_reg_other_D2.2 <- data_leagues_reg_D2.2[!data_leagues_reg_D2.2$TournamentName %in% top_leagues, ]

conv_mat_leagues <- matrix(c(
    length(unique(data_leagues_reg_D2.3$TournamentName)), length(unique(data_leagues_reg_top5_D2.3$TournamentName)), length(unique(data_leagues_reg_other_D2.3$TournamentName)),
    nrow(data_leagues_reg_D2.3), nrow(data_leagues_reg_top5_D2.3), nrow(data_leagues_reg_other_D2.3),
    length(unique(data_leagues_reg_D2.2$TournamentName)), length(unique(data_leagues_reg_top5_D2.2$TournamentName)), length(unique(data_leagues_reg_other_D2.2$TournamentName)),
    nrow(data_leagues_reg_D2.2), nrow(data_leagues_reg_top5_D2.2), nrow(data_leagues_reg_other_D2.2)
), byrow = F, nrow = 3)
colnames(conv_mat_leagues) <- c("#_D2.3", "n_D2.3", "#_D2.2", "n_D2.2")
rownames(conv_mat_leagues) <- c("national leagues", "top five leagues", "other leagues")

tab2 <- cbind(
    conv_mat_leagues,
    c(outcome_function(data_leagues_reg_D2.2), outcome_function(data_leagues_reg_top5_D2.2), outcome_function(data_leagues_reg_other_D2.2)),
    matrix(c(outcome_function(data_leagues_reg_D2.3), outcome_function(data_leagues_reg_top5_D2.3), outcome_function(data_leagues_reg_other_D2.3)), nrow = 3, byrow = T)
)
colnames(tab2) <- c(
    "#_D2.3", "n_D2.3", "#_D2.2", "n_D2.2",
    "goal_D2", "goal_D3", "miss_D3", "save_D3"
)

tab2 <- tab2[c("top five leagues", "other leagues", "national leagues"), c(
    "#_D2.2", "n_D2.2", "goal_D2",
    "#_D2.3", "n_D2.3", "goal_D3", "save_D3", "miss_D3"
)]

colnames(tab2) <- c(
    "leagues_D2", "penalties_D2", "goal_D2", "leagues_D3", "penalties_D3",
    "goal_D3", "save_D3", "miss_D3"
)
rownames(tab2) <- c("Big-5", "other leagues", "combined")

write.csv(tab2, "processed_data/tab.2.csv")


#### cups ####
top_cups <- c("cups/cdr-copa-del-rey", "cups/frc-coupe-de-france", "cups/fac-fa-cup", "cups/cgb-efl-cup", "cups/dfb-dfb-pokal", "cups/cit-coppa-italia")

# D2.3
data_cups_reg_D2.3 <- data_regular_90_D2.3[data_regular_90_D2.3$CompetitionShortName == "cups", ]
data_cups_extra_D2.3 <- data_regular_120_D2.3[data_regular_120_D2.3$CompetitionShortName == "cups", ]
data_cups_shootout_D2.3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "cups", ]

data_cups_reg_top5_D2.3 <- data_cups_reg_D2.3[data_cups_reg_D2.3$TournamentName %in% top_cups, ]
data_cups_extra_top5_D2.3 <- data_cups_extra_D2.3[data_cups_extra_D2.3$TournamentName %in% top_cups, ]
data_cups_shootout_top5_D2.3 <- data_cups_shootout_D2.3[data_cups_shootout_D2.3$TournamentName %in% top_cups, ]

data_cups_reg_other_D2.3 <- data_cups_reg_D2.3[!data_cups_reg_D2.3$TournamentName %in% top_cups, ]
data_cups_extra_other_D2.3 <- data_cups_extra_D2.3[!data_cups_extra_D2.3$TournamentName %in% top_cups, ]
data_cups_shootout_other_D2.3 <- data_cups_shootout_D2.3[!data_cups_shootout_D2.3$TournamentName %in% top_cups, ]

data_int_cups_reg_D2.3 <- data_regular_90_D2.3[data_regular_90_D2.3$CompetitionShortName == "int_cups", ]
data_int_cups_extra_D2.3 <- data_regular_120_D2.3[data_regular_120_D2.3$CompetitionShortName == "int_cups", ]
data_int_cups_shootout_D2.3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "int_cups", ]

data_int_sides_reg_D2.3 <- data_regular_90_D2.3[data_regular_90_D2.3$CompetitionShortName == "nat_teams", ]
data_int_sides_extra_D2.3 <- data_regular_120_D2.3[data_regular_120_D2.3$CompetitionShortName == "nat_teams", ]
data_int_sides_shootout_D2.3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "nat_teams", ]

# D2.2
data_cups_reg_D2.2 <- data_regular_90_D2.2[data_regular_90_D2.2$CompetitionShortName == "cups", ]
data_cups_extra_D2.2 <- data_regular_120_D2.2[data_regular_120_D2.2$CompetitionShortName == "cups", ]
data_cups_shootout_D2.2 <- data_shootout_D2.2[data_shootout_D2.2$CompetitionShortName == "cups", ]

data_cups_reg_top5_D2.2 <- data_cups_reg_D2.2[data_cups_reg_D2.2$TournamentName %in% top_cups, ]
data_cups_extra_top5_D2.2 <- data_cups_extra_D2.2[data_cups_extra_D2.2$TournamentName %in% top_cups, ]
data_cups_shootout_top5_D2.2 <- data_cups_shootout_D2.2[data_cups_shootout_D2.2$TournamentName %in% top_cups, ]

data_cups_reg_other_D2.2 <- data_cups_reg_D2.2[!data_cups_reg_D2.2$TournamentName %in% top_cups, ]
data_cups_extra_other_D2.2 <- data_cups_extra_D2.2[!data_cups_extra_D2.2$TournamentName %in% top_cups, ]
data_cups_shootout_other_D2.2 <- data_cups_shootout_D2.2[!data_cups_shootout_D2.2$TournamentName %in% top_cups, ]

data_int_cups_reg_D2.2 <- data_regular_90_D2.2[data_regular_90_D2.2$CompetitionShortName == "int_cups", ]
data_int_cups_extra_D2.2 <- data_regular_120_D2.2[data_regular_120_D2.2$CompetitionShortName == "int_cups", ]
data_int_cups_shootout_D2.2 <- data_shootout_D2.2[data_shootout_D2.2$CompetitionShortName == "int_cups", ]

data_int_sides_reg_D2.2 <- data_regular_90_D2.2[data_regular_90_D2.2$CompetitionShortName == "nat_teams", ]
data_int_sides_extra_D2.2 <- data_regular_120_D2.2[data_regular_120_D2.2$CompetitionShortName == "nat_teams", ]
data_int_sides_shootout_D2.2 <- data_shootout_D2.2[data_shootout_D2.2$CompetitionShortName == "nat_teams", ]

# table
conv_mat_cups <- matrix(c(
    length(unique(data_cups_reg_D2.3$TournamentName)), length(unique(data_cups_reg_top5_D2.3$TournamentName)), length(unique(data_cups_reg_other_D2.3$TournamentName)), length(unique(data_int_cups_reg_D2.3$TournamentName)), length(unique(data_int_sides_reg_D2.3$TournamentName)),
    nrow(data_cups_reg_D2.3), nrow(data_cups_reg_top5_D2.3), nrow(data_cups_reg_other_D2.3), nrow(data_int_cups_reg_D2.3), nrow(data_int_sides_reg_D2.3),
    length(unique(data_cups_extra_D2.3$TournamentName)), length(unique(data_cups_extra_top5_D2.3$TournamentName)), length(unique(data_cups_extra_other_D2.3$TournamentName)), length(unique(data_int_cups_extra_D2.3$TournamentName)), length(unique(data_int_sides_extra_D2.3$TournamentName)),
    nrow(data_cups_extra_D2.3), nrow(data_cups_extra_top5_D2.3), nrow(data_cups_extra_other_D2.3), nrow(data_int_cups_extra_D2.3), nrow(data_int_sides_extra_D2.3),
    length(unique(data_cups_shootout_D2.3$TournamentName)), length(unique(data_cups_shootout_top5_D2.3$TournamentName)), length(unique(data_cups_shootout_other_D2.3$TournamentName)), length(unique(data_int_cups_shootout_D2.3$TournamentName)), length(unique(data_int_sides_shootout_D2.3$TournamentName)),
    nrow(data_cups_shootout_D2.3), nrow(data_cups_shootout_top5_D2.3), nrow(data_cups_shootout_other_D2.3), nrow(data_int_cups_shootout_D2.3), nrow(data_int_sides_shootout_D2.3),
    length(unique(data_cups_reg_D2.2$TournamentName)), length(unique(data_cups_reg_top5_D2.2$TournamentName)), length(unique(data_cups_reg_other_D2.2$TournamentName)), length(unique(data_int_cups_reg_D2.2$TournamentName)), length(unique(data_int_sides_reg_D2.2$TournamentName)),
    nrow(data_cups_reg_D2.2), nrow(data_cups_reg_top5_D2.2), nrow(data_cups_reg_other_D2.2), nrow(data_int_cups_reg_D2.2), nrow(data_int_sides_reg_D2.2),
    length(unique(data_cups_extra_D2.2$TournamentName)), length(unique(data_cups_extra_top5_D2.2$TournamentName)), length(unique(data_cups_extra_other_D2.2$TournamentName)), length(unique(data_int_cups_extra_D2.2$TournamentName)), length(unique(data_int_sides_extra_D2.2$TournamentName)),
    nrow(data_cups_extra_D2.2), nrow(data_cups_extra_top5_D2.2), nrow(data_cups_extra_other_D2.2), nrow(data_int_cups_extra_D2.2), nrow(data_int_sides_extra_D2.2),
    length(unique(data_cups_shootout_D2.2$TournamentName)), length(unique(data_cups_shootout_top5_D2.2$TournamentName)), length(unique(data_cups_shootout_other_D2.2$TournamentName)), length(unique(data_int_cups_shootout_D2.2$TournamentName)), length(unique(data_int_sides_shootout_D2.2$TournamentName)),
    nrow(data_cups_shootout_D2.2), nrow(data_cups_shootout_top5_D2.2), nrow(data_cups_shootout_other_D2.2), nrow(data_int_cups_shootout_D2.2), nrow(data_int_sides_shootout_D2.2)
), byrow = F, nrow = 5)
rownames(conv_mat_cups) <- c("national cups", "top five cups", "other cups", "international cups", "international sides")
colnames(conv_mat_cups) <- c(
    "#_in-game_D3", "n_in-game_D3", "#_extra time_D3", "n_extra time_D3", "#_shootout_D3", "n_shootout_D3",
    "#_in-game_D2", "n_in-game_D2", "#_extra time_D2", "n_extra time_D2", "#_shootout_D2", "n_shootout_D2"
)

write.csv(conv_mat_cups, "processed_data/tab.12-13.csv")

fig10 <- matrix(c(
    outcome_function(data_cups_reg_D2.3),
    outcome_function(data_cups_reg_top5_D2.3),
    outcome_function(data_cups_reg_other_D2.3),
    outcome_function(data_int_cups_reg_D2.3),
    outcome_function(data_int_sides_reg_D2.3),
    outcome_function(data_cups_reg_D2.2),
    outcome_function(data_cups_reg_top5_D2.2),
    outcome_function(data_cups_reg_other_D2.2),
    outcome_function(data_int_cups_reg_D2.2),
    outcome_function(data_int_sides_reg_D2.2)
), nrow = 1)

colnames(fig10) <- c(
    paste(
        rep(c(
            "D3: national cups", "D3: top five", "D3: other cups",
            "D3: international cups", "D3: international sides"
        ), each = 3),
        rep(c("goal", "miss", "save"), times = 5),
        sep = "_"
    ),
    c(
        "D2: national cups", "D2: top five", "D2: other cups",
        "D2: international cups", "D2: international sides"
    )
)
rownames(fig10) <- "rate"

write.csv(fig10, "processed_data/fig.10.csv")

fig11 <- matrix(c(
    outcome_function(data_cups_extra_D2.3),
    outcome_function(data_cups_extra_top5_D2.3),
    outcome_function(data_cups_extra_other_D2.3),
    outcome_function(data_int_cups_extra_D2.3),
    outcome_function(data_int_sides_extra_D2.3),
    outcome_function(data_cups_extra_D2.2),
    outcome_function(data_cups_extra_top5_D2.2),
    outcome_function(data_cups_extra_other_D2.2),
    outcome_function(data_int_cups_extra_D2.2),
    outcome_function(data_int_sides_extra_D2.2)
), nrow = 1)

colnames(fig11) <- c(
    paste(
        rep(c(
            "D3: national cups", "D3: top five", "D3: other cups",
            "D3: international cups", "D3: international sides"
        ), each = 3),
        rep(c("goal", "miss", "save"), times = 5),
        sep = "_"
    ),
    c(
        "D2: national cups", "D2: top five", "D2: other cups",
        "D2: international cups", "D2: international sides"
    )
)
rownames(fig11) <- "rate"

write.csv(fig11, "processed_data/fig.11.csv")

fig12 <- matrix(c(
    outcome_function(data_cups_shootout_D2.3),
    outcome_function(data_cups_shootout_top5_D2.3),
    outcome_function(data_cups_shootout_other_D2.3),
    outcome_function(data_int_cups_shootout_D2.3),
    outcome_function(data_int_sides_shootout_D2.3),
    outcome_function(data_cups_shootout_D2.2),
    outcome_function(data_cups_shootout_top5_D2.2),
    outcome_function(data_cups_shootout_other_D2.2),
    outcome_function(data_int_cups_shootout_D2.2),
    outcome_function(data_int_sides_shootout_D2.2)
), nrow = 1)

colnames(fig12) <- c(
    paste(
        rep(c(
            "D3: national cups", "D3: top five", "D3: other cups",
            "D3: international cups", "D3: international sides"
        ), each = 3),
        rep(c("goal", "miss", "save"), times = 5),
        sep = "_"
    ),
    c(
        "D2: national cups", "D2: top five", "D2: other cups",
        "D2: international cups", "D2: international sides"
    )
)
rownames(fig12) <- "rate"

write.csv(fig12, "processed_data/fig.12.csv")


##########  tab 3  ############


data_cups_regular_D3 <- data_regular_D2.3[data_regular_D2.3$CompetitionShortName == "cups", ]
data_int_cups_regular_D3 <- data_regular_D2.3[data_regular_D2.3$CompetitionShortName == "int_cups", ]
data_int_sides_regular_D3 <- data_regular_D2.3[data_regular_D2.3$CompetitionShortName == "nat_teams", ]
data_not_leagues_regular_D3 <- data_regular_D2.3[data_regular_D2.3$CompetitionShortName != "leag", ]

data_cups_shootout_D3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "cups", ]
data_int_cups_shootout_D3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "int_cups", ]
data_int_sides_shootout_D3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName == "nat_teams", ]
data_not_leagues_shootout_D3 <- data_shootout_D2.3[data_shootout_D2.3$CompetitionShortName != "leag", ]

tab3 <- matrix(cbind(
    c(
        length(unique(data_cups_regular_D3$TournamentName)), length(unique(data_int_cups_regular_D3$TournamentName)),
        length(unique(data_int_sides_regular_D3$TournamentName)), length(unique(data_not_leagues_regular_D3$TournamentName))
    ),
    c(
        nrow(data_cups_regular_D3), nrow(data_int_cups_regular_D3),
        nrow(data_int_sides_regular_D3), nrow(data_not_leagues_regular_D3)
    ),
    matrix(c(
        outcome_function(data_cups_regular_D3), outcome_function(data_int_cups_regular_D3),
        outcome_function(data_int_sides_regular_D3), outcome_function(data_not_leagues_regular_D3)
    ), ncol = 3, byrow = T),
    c(
        length(unique(data_cups_shootout_D3$TournamentName)), length(unique(data_int_cups_shootout_D3$TournamentName)),
        length(unique(data_int_sides_shootout_D3$TournamentName)), length(unique(data_not_leagues_shootout_D3$TournamentName))
    ),
    c(
        nrow(data_cups_shootout_D3), nrow(data_int_cups_shootout_D3),
        nrow(data_int_sides_shootout_D3), nrow(data_not_leagues_shootout_D3)
    ),
    matrix(c(
        outcome_function(data_cups_shootout_D3), outcome_function(data_int_cups_shootout_D3),
        outcome_function(data_int_sides_shootout_D3), outcome_function(data_not_leagues_shootout_D3)
    ), ncol = 3, byrow = T)
), nrow = 4)
rownames(tab3) <- c("national", "international (clubs)", "international (selections)", "combined")
colnames(tab3) <- c(
    "cups_ingame", "penalties_ingame", "goal_ingame", "miss_ingame", "save_ingame",
    "cups_shootout", "penalties_shootout", "goal_shootout", "miss_shootout", "save_shootout"
)

tab3 <- tab3[, c(
    "cups_ingame", "penalties_ingame", "goal_ingame", "save_ingame", "miss_ingame",
    "cups_shootout", "penalties_shootout", "goal_shootout", "save_shootout", "miss_shootout"
)]

write.csv(tab3, "processed_data/tab.3.csv")

#### Conversion Rates - top N players/teams ####


# conversion per player
g <- 1
while (sum(table(data_final_D2.2$player_name) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_final_D2.2$player_name)[table(data_final_D2.2$player_name) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_final_D2.2[data_final_D2.2$player_name %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "goal"], decreasing = T), ])
players_50_stats[, "penalty taker"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("penalty taker", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)

# player_care <- players_50[players_50 %in% players_careful]
# vec_duplicates <- data_regular_D2.2[,c("OfficialMatchName","FinalResult","player_name","PenaltyTakerTeamName","EventTypeName","goalie_name","GoalkeeperTeam", "PenaltyMinute","PenaltySecond","ScoreAfter")]
# data_regular_D2.2[ duplicated(vec_duplicates) | duplicated(vec_duplicates, fromLast=T),]

write.csv(players_50_stats, "processed_data/fig.2.csv")

data_frequent_players_regular <- data_final_D2.2[data_final_D2.2$player_name %in% players_50, ]
outcome_function(data_frequent_players_regular)

data_not_frequent_players_regular <- data_final_D2.2[!data_final_D2.2$player_name %in% players_50, ]

outcome_function(data_not_frequent_players_regular)

outcome_function(data_final_D2.2)


# conversion per player shootout
g <- 1
while (sum(table(data_shootout_D2.2$player_name) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_shootout_D2.2$player_name)[table(data_shootout_D2.2$player_name) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_shootout_D2.2[data_shootout_D2.2$player_name %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "penalties"], decreasing = T), ])
players_50_stats[, "penalty taker"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("penalty taker", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)


# conversion per goalies
data_final_goalies <- data_regular_D2.3[data_regular_D2.3$goalie_name != "dummy_goalie", ]
g <- 1
while (sum(table(data_final_goalies$goalie_name) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_final_goalies$goalie_name)[table(data_final_goalies$goalie_name) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_final_goalies[data_final_goalies$goalie_name %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "save"], decreasing = T), ])
players_50_stats[, "goalkeeper"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("goalkeeper", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)

write.csv(players_50_stats, "processed_data/fig.3.csv")


# test for cor save and misses
test_save_miss <- players_50_stats[, c("save", "miss")] * 0.01 * players_50_stats[, c("penalties")]
cor.test(test_save_miss[, "save"], test_save_miss[, "miss"], method = "pearson")

data_frequent_goalies_regular <- data_regular_D2.3[data_regular_D2.3$goalie_name %in% players_50, ]
outcome_function(data_frequent_goalies_regular)

data_not_frequent_goalies_regular <- data_regular_D2.3[!data_regular_D2.3$goalie_name %in% players_50, ]
outcome_function(data_not_frequent_goalies_regular)

# goalies shootout

data_final_goalies <- data_shootout_D2.3[data_shootout_D2.3$goalie_name != "dummy_goalie", ]
g <- 1
while (sum(table(data_final_goalies$goalie_name) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_final_goalies$goalie_name)[table(data_final_goalies$goalie_name) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_final_goalies[data_final_goalies$goalie_name %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "save"], decreasing = T), ])
players_50_stats[, "goalkeeper"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("goalkeeper", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)

write.csv(players_50_stats, "processed_data/fig.4.csv")

data_frequent_goalies_regular <- data_regular_D2.3[data_regular_D2.3$goalie_name %in% players_50, ]
outcome_function(data_frequent_goalies_regular)
outcome_function(data_regular_D2.3)


# conversion per team
g <- 1
while (sum(table(data_regular_D2.3$PenaltyTakerTeamName) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_regular_D2.3$PenaltyTakerTeamName)[table(data_regular_D2.3$PenaltyTakerTeamName) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_regular_D2.3[data_regular_D2.3$PenaltyTakerTeamName %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "goal"], decreasing = T), ])
players_50_stats[, "club team"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("club team", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)

write.csv(players_50_stats, "processed_data/fig.5.csv")

data_frequent_players_regular <- data_regular_D2.3[data_regular_D2.3$PenaltyTakerTeamName %in% players_50, ]
outcome_function(data_frequent_players_regular)

data_frequent_players_regular <- data_regular_D2.3[!data_regular_D2.3$PenaltyTakerTeamName %in% players_50, ]
outcome_function(data_frequent_players_regular)

# shootout
g <- 1
while (sum(table(data_shootout_D2.3$PenaltyTakerTeamName) >= g) >= 50) {
    g <- g + 1
}
n_player <- g - 1

players_50 <- names(table(data_shootout_D2.3$PenaltyTakerTeamName)[table(data_shootout_D2.3$PenaltyTakerTeamName) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("penalties", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_shootout_D2.3[data_shootout_D2.3$PenaltyTakerTeamName %in% players_50[i], ]

    players_50_stats[i, c("goal", "miss", "save")] <- table(data_players_50$EventTypeName) / sum(table(data_players_50$EventTypeName)) * 100
    players_50_stats[i, "penalties"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- data.frame(players_50_stats[order(players_50_stats[, "goal"], decreasing = T), ])
players_50_stats[, "club team"] <- rownames(players_50_stats)
players_50_stats <- players_50_stats[, c("club team", "penalties", "goal", "save", "miss")]
nrow(players_50_stats)

write.csv(players_50_stats, "processed_data/fig.6.csv")

data_frequent_players_regular <- data_shootout_D2.3[data_shootout_D2.3$PenaltyTakerTeamName %in% players_50, ]
outcome_function(data_frequent_players_regular)

data_frequent_players_regular <- data_shootout_D2.3[!data_shootout_D2.3$PenaltyTakerTeamName %in% players_50, ]
outcome_function(data_frequent_players_regular)

# overall teams

n_player <- 20

players_50 <- names(table(data_final_D2.3$PenaltyTakerTeamName)[table(data_final_D2.3$PenaltyTakerTeamName) >= n_player])
players_50_stats <- matrix(NA, ncol = 4, nrow = length(players_50))
colnames(players_50_stats) <- c("n", "goal", "miss", "save")
rownames(players_50_stats) <- str_to_title(players_50)

for (i in seq_along(players_50)) {
    data_players_50 <- data_final_D2.3[data_final_D2.3$PenaltyTakerTeamName %in% players_50[i], ]
    players_50_stats[i, c("goal", "miss", "save")] <- outcome_function(data_players_50)
    players_50_stats[i, "n"] <- sum(table(data_players_50$EventTypeName))
}

players_50_stats <- players_50_stats[order(players_50_stats[, "goal"], decreasing = T), ]
nrow(players_50_stats)



##########  Figure 5 Fatigue plot  ############

## no of penalties regular 90min ##
data_regular_fatigue <- data_regular_90_D2.3[data_regular_90_D2.3$PenaltyMinute != 0, ]
sum(data_regular_90_D2.3$PenaltyMinute == 0) # delete 71 where time is missing

data_first_half <- data_regular_fatigue[data_regular_fatigue$TypePhaseName == "First half", ]
data_second_half <- data_regular_fatigue[data_regular_fatigue$TypePhaseName == "Second half", ]

nrow(data_first_half)
nrow(data_second_half)

overall_first_mean <- outcome_function(data_first_half)
overall_second_mean <- outcome_function(data_second_half)



## first half
data_first_pen_time <- data.frame(matrix(t(table(data_first_half$EventTypeName, data_first_half$PenaltyMinute)), ncol = 3))
colnames(data_first_pen_time) <- colnames(t(table(data_first_half$EventTypeName, data_first_half$PenaltyMinute)))
rownames(data_first_pen_time) <- rownames(t(table(data_first_half$EventTypeName, data_first_half$PenaltyMinute)))
data_first_pen_time <- data_first_pen_time[order(as.numeric(rownames(data_first_pen_time))), ]
data_first_pen_time[, "penalties"] <- rowSums(data_first_pen_time)
data_first_pen_time[, "PenaltyMinute"] <- as.numeric(rownames(data_first_pen_time))

## second half
data_second_pen_time <- data.frame(matrix(t(table(data_second_half$EventTypeName, data_second_half$PenaltyMinute)), ncol = 3))
colnames(data_second_pen_time) <- colnames(t(table(data_second_half$EventTypeName, data_second_half$PenaltyMinute)))
rownames(data_second_pen_time) <- rownames(t(table(data_second_half$EventTypeName, data_second_half$PenaltyMinute)))
data_second_pen_time <- data_second_pen_time[order(as.numeric(rownames(data_second_pen_time))), ]
data_second_pen_time[, "penalties"] <- rowSums(data_second_pen_time)
data_second_pen_time[, "PenaltyMinute"] <- as.numeric(rownames(data_second_pen_time))

## loess fit for added time
loess_first_half <- loess(penalties ~ PenaltyMinute, data = data_first_pen_time[1:44, ], control = loess.control(surface = "direct"))
expected_45 <- predict(loess_first_half, newdata = data.frame("PenaltyMinute" = 45))

loess_second_half <- loess(penalties ~ PenaltyMinute, data = data_second_pen_time[1:44, ], control = loess.control(surface = "direct"))
expected_90 <- predict(loess_second_half, newdata = data.frame("PenaltyMinute" = 90))

ratio_in_46 <- (data_first_pen_time["45", "penalties"] - expected_45) / data_first_pen_time["45", "penalties"]
data_first_pen_time["46", ] <- c(ratio_in_46 * data_first_pen_time["45", c("Goal", "Missed", "Save", "penalties")], 46)
data_first_pen_time["45", ] <- c((1 - ratio_in_46) * data_first_pen_time["45", c("Goal", "Missed", "Save", "penalties")], 45)

round(data_first_pen_time["46", "penalties"] / data_first_pen_time["45", "penalties"])
data_first_pen_time["47", ] <- c(1 / 3 * data_first_pen_time["46", c("Goal", "Missed", "Save", "penalties")], 47)
data_first_pen_time["48", ] <- c(1 / 3 * data_first_pen_time["46", c("Goal", "Missed", "Save", "penalties")], 48)
data_first_pen_time["46", ] <- c(1 / 3 * data_first_pen_time["46", c("Goal", "Missed", "Save", "penalties")], 46)

ratio_in_91 <- (data_second_pen_time["90", "penalties"] - expected_90) / data_second_pen_time["90", "penalties"]
data_second_pen_time["91", ] <- c(ratio_in_91 * data_second_pen_time["90", c("Goal", "Missed", "Save", "penalties")], 91)
data_second_pen_time["90", ] <- c((1 - ratio_in_91) * data_second_pen_time["90", c("Goal", "Missed", "Save", "penalties")], 90)

round(data_second_pen_time["91", "penalties"] / data_second_pen_time["90", "penalties"])

data_second_pen_time["92", ] <- c(1 / 5 * data_second_pen_time["91", c("Goal", "Missed", "Save", "penalties")], 92)
data_second_pen_time["93", ] <- c(1 / 5 * data_second_pen_time["91", c("Goal", "Missed", "Save", "penalties")], 93)
data_second_pen_time["94", ] <- c(1 / 5 * data_second_pen_time["91", c("Goal", "Missed", "Save", "penalties")], 94)
data_second_pen_time["95", ] <- c(1 / 5 * data_second_pen_time["91", c("Goal", "Missed", "Save", "penalties")], 95)
data_second_pen_time["91", ] <- c(1 / 5 * data_second_pen_time["91", c("Goal", "Missed", "Save", "penalties")], 91)

data_first_roll <- data.frame(apply(data_first_pen_time[, c("Goal", "Missed", "Save")], 2, rollsum, k = 5, fill = NA))
data_first_rel <- data.frame(t(apply(data_first_roll[, c("Goal", "Missed", "Save")], 1, function(x) {
    x / sum(x)
})) * 100)

data_second_roll <- data.frame(apply(data_second_pen_time[, c("Goal", "Missed", "Save")], 2, rollsum, k = 5, fill = NA))
data_second_rel <- data.frame(t(apply(data_second_roll[, c("Goal", "Missed", "Save")], 1, function(x) {
    x / sum(x)
})) * 100)

data_first_rel[, "penalties"] <- data_first_pen_time$penalties
data_second_rel[, "penalties"] <- data_second_pen_time$penalties

# extra time
mean_extra_first <- outcome_function(data_regular_120_D2.3[data_regular_120_D2.3$TypePhaseName == "1st Extra Time", ])
mean_extra_second <- outcome_function(data_regular_120_D2.3[data_regular_120_D2.3$TypePhaseName == "2nd Extra Time", ])
mean_extra <- outcome_function(data_regular_120_D2.3)

data_extra_pen_time <- matrix(table(data_regular_120_D2.3$PenaltyMinute, data_regular_120_D2.3$EventTypeName), ncol = 3)
rownames(data_extra_pen_time) <- rownames(table(data_regular_120_D2.3$PenaltyMinute, data_regular_120_D2.3$EventTypeName))
colnames(data_extra_pen_time) <- colnames(table(data_regular_120_D2.3$PenaltyMinute, data_regular_120_D2.3$EventTypeName))
data_extra_pen_time <- data.frame(data_extra_pen_time[order(as.numeric(rownames(data_extra_pen_time))), ])
data_extra_pen_time[, "penalties"] <- rowSums(data_extra_pen_time)
data_extra_pen_time[, "PenaltyMinute"] <- as.numeric(rownames(data_extra_pen_time))

# normalization for extra time
data_extra_pen_time[, "penalties"] <- data_extra_pen_time[, "penalties"] * norm_extra_time

loess_first_extra <- loess(penalties ~ PenaltyMinute, data = data_extra_pen_time[1:14, ], control = loess.control(surface = "direct"))
expected_105 <- predict(loess_first_extra, newdata = data.frame("PenaltyMinute" = 105))
data_extra_pen_time["105", "penalties"] <- expected_105

# (data_extra_pen_time["105","penalties"] - expected_105) / expected_105

loess_second_extra <- loess(penalties ~ PenaltyMinute, data = data_extra_pen_time[16:29, ], control = loess.control(surface = "direct"))
expected_120 <- predict(loess_second_extra, newdata = data.frame("PenaltyMinute" = 120))
data_extra_pen_time["120", "penalties"] <- expected_120

data_extra_roll_first <- data.frame(apply(data_extra_pen_time[1:15, c("Goal", "Missed", "Save")], 2, rollsum, k = 5, fill = NA))
data_extra_roll_second <- data.frame(apply(data_extra_pen_time[16:30, c("Goal", "Missed", "Save")], 2, rollsum, k = 5, fill = NA))
data_extra_roll <- rbind(data_extra_roll_first, data_extra_roll_second)
data_extra_rel <- data.frame(t(apply(data_extra_roll[, c("Goal", "Missed", "Save")], 1, function(x) {
    x / sum(x)
})) * 100)
data_extra_rel[, "penalties"] <- data_extra_pen_time$penalties

# rbind all phases
data_rel_all <- rbind(data_first_rel, data_second_rel, data_extra_rel)
data_rel_all[, "minute"] <- c(
    1:45, "45_1", "45_2", "45_3",
    46:90, "90_1", "90_2", "90_3", "90_4", "90_5",
    91:120
)

write.csv(data_rel_all, "processed_data/fig.7.csv")


#### penalty shootouts ####
nrow(data_shootout_D2.2)
length(unique(data_shootout_D2.2$fcompMatchID))


taker_score_shoot <- matrix(NA, nrow = nrow(data_shootout_D2.2), ncol = 1)
goalie_score_shoot <- matrix(NA, nrow = nrow(data_shootout_D2.2), ncol = 1)

for (i in 1:nrow(data_shootout_D2.2)) {
    score <- str_extract_all(data_shootout_D2.2$ScoreBefore[i], "\\d+")

    score_home <- as.numeric(score[[1]][1])
    score_away <- as.numeric(score[[1]][2])

    team_taker <- data_shootout_D2.2$PenaltyTakerTeamName[i]
    team_home <- data_shootout_D2.2$homeTeamName[i]
    team_away <- data_shootout_D2.2$awayTeamName[i]

    if (team_taker == team_home) {
        taker_score_shoot[i] <- score_home
        goalie_score_shoot[i] <- score_away
    }

    if (team_taker == team_away) {
        goalie_score_shoot[i] <- score_home
        taker_score_shoot[i] <- score_away
    }
}

data_shootout_D2.2[, "taker_score"] <- taker_score_shoot
data_shootout_D2.2[, "goalie_score"] <- goalie_score_shoot

data_shoot_time <- table(data_shootout_D2.2$order, data_shootout_D2.2$EventTypeName)
shootout_mean_order <- data_shoot_time / apply(data_shoot_time, 1, sum)
shootout_mean_order <- cbind(shootout_mean_order, "size" = apply(data_shoot_time, 1, sum))

shootout_mean_order <- data.frame(shootout_mean_order[, c("Goal", "size")])
shootout_mean_order[, "order"] <- rownames(shootout_mean_order)
shootout_mean_order$Goal <- round(shootout_mean_order$Goal * 100, 2)
colnames(shootout_mean_order) <- c("goal", "penalties", "order")

write.csv(shootout_mean_order, "processed_data/fig.8.csv")

first_shooters_names <- data_shootout_D2.2[data_shootout_D2.2$order %in% 1:2, "player_name"]
outcome_function(data_regular_D2.2[data_regular_D2.2$player_name %in% first_shooters_names, ])

##### fig 10

# we'll be incrementing a lot
inc <- function(x, y) {
    eval.parent(substitute(x <- x + y))
}

# goal probabilities for teams A and B by in shooting order
fig10_data <- data_shoot_time / apply(data_shoot_time, 1, sum)
fig10_data_18 <- fig10_data[1:18, 1]

A <- fig10_data_18[seq(1, 17, 2)]
B <- fig10_data_18[seq(2, 18, 2)]

# P: 1+penalty number by goal difference A-B+4 (1:-3, 4:0, 7:+3 for A)
P <- array(0, dim = c(19, 7))
# score is level before first penalty with probability 1
P[1, 4] <- 1

# first 2*3 penalties are always taken
for (i in 1:3) {
    # Ai: max diff is -2 to +2
    for (j in 2:6)
    {
        inc(P[2 * i, j], (1 - A[i]) * P[2 * i - 1, j])
        inc(P[2 * i, j + 1], A[i] * P[2 * i - 1, j])
    }
    # Bi: A may be at +3 (j=7) after A3
    for (j in 2:7)
    {
        inc(P[2 * i + 1, j], (1 - B[i]) * P[2 * i, j])
        inc(P[2 * i + 1, j - 1], B[i] * P[2 * i, j])
    }
}

# A4: A already lost if diff=-3, won if diff=3
for (j in 2:6)
{
    inc(P[8, j], (1 - A[4]) * P[7, j])
    inc(P[8, j + 1], A[4] * P[7, j])
}
# B4: B won if diff=-3,-2, lost if diff=3
for (j in 3:6)
{
    inc(P[9, j], (1 - B[4]) * P[8, j])
    inc(P[9, j - 1], B[4] * P[8, j])
}

# A5: A shoot only if diff=-1,0,+1
for (j in 3:5)
{
    inc(P[10, j], (1 - A[5]) * P[9, j])
    inc(P[10, j + 1], A[5] * P[9, j])
}
# B5: B shoot only if diff=0,+1
for (j in 4:5)
{
    inc(P[11, j], (1 - B[5]) * P[10, j])
    inc(P[11, j - 1], B[5] * P[10, j])
}

# A6-B9
for (i in 6:length(A)) {
    # Ai (score must be level)
    inc(P[2 * i, 4], (1 - A[i]) * P[2 * i - 1, 4])
    inc(P[2 * i, 4 + 1], A[i] * P[2 * i - 1, 4])
    # Bi (level or A up by one)
    for (j in 4:5)
    {
        inc(P[2 * i + 1, j], (1 - B[i]) * P[2 * i, j])
        inc(P[2 * i + 1, j - 1], B[i] * P[2 * i, j])
    }
}

# A or B win if j (=diff+4) has the following value
WA <- c(0, 0, 0, 0, 0, 7, 7, 6, 6, 5, 0, 5, 0, 5, 0, 5, 0, 5)
WB <- c(0, 0, 0, 0, 0, 1, 2, 2, 3, 3, 0, 3, 0, 3, 0, 3, 0, 3)
W <- array(0, dim = c(18, 2))
# from sixth to tenth penalty, each may decide
for (i in 6:10) {
    W[i, 1] <- P[1 + i, WA[i]]
    W[i, 2] <- P[1 + i, WB[i]]
}
# after that, only even penalties can decide (12th to 18th)
for (i in 6:9) {
    W[2 * i, 1] <- P[1 + 2 * i, WA[2 * i]]
    W[2 * i, 2] <- P[1 + 2 * i, WB[2 * i]]
}

cumsum(W[, 1])
cumsum(W[, 2])


#### durbin watson tests
lmtest::dwtest(lm(fig10_data_18[seq(1, 17, 2)] ~ 1), alternative = "two.sided") # A
lmtest::dwtest(lm(fig10_data_18[seq(2, 18, 2)] ~ 1), alternative = "two.sided") # B
lmtest::dwtest(lm(fig10_data_18 ~ 1), alternative = "two.sided") # all


#### Potential Last Penalty ####

data_shoot_gametime <- data_shootout_D2.2
for (i in seq_len(nrow(data_shoot_gametime))) {
    taker_diff <- data_shoot_gametime[i, "taker_score"] - data_shoot_gametime[i, "goalie_score"]

    # 1. if no condition is fulfilled
    data_shoot_gametime[i, "pressure"] <- "no"

    # 2. 6th penalty
    if (data_shoot_gametime[i, "order"] == 6) {
        if (data_shoot_gametime[i, "taker_score"] == 2 & data_shoot_gametime[i, "goalie_score"] == 0) {
            data_shoot_gametime[i, "pressure"] <- "win"
        }

        if (data_shoot_gametime[i, "taker_score"] == 0 & data_shoot_gametime[i, "goalie_score"] == 3) {
            data_shoot_gametime[i, "pressure"] <- "lose"
        }
    }

    # 3. 7th penalty
    if (data_shoot_gametime[i, "order"] == 7) {
        if (taker_diff == 2) {
            data_shoot_gametime[i, "pressure"] <- "win"
        }

        if (taker_diff == -2) {
            data_shoot_gametime[i, "pressure"] <- "lose"
        }
    }

    # 4. 8th penalty
    if (data_shoot_gametime[i, "order"] == 8) {
        if (taker_diff == 1) {
            data_shoot_gametime[i, "pressure"] <- "win"
        }

        if (taker_diff == -2) {
            data_shoot_gametime[i, "pressure"] <- "lose"
        }
    }

    # 5. 9th penalty
    if (data_shoot_gametime[i, "order"] == 9) {
        if (taker_diff == 1) {
            data_shoot_gametime[i, "pressure"] <- "win"
        }

        if (taker_diff == -1) {
            data_shoot_gametime[i, "pressure"] <- "lose"
        }
    }
    # 6. > 9th penalty
    if (data_shoot_gametime[i, "order"] >= 10 & data_shoot_gametime[i, "order"] %% 2 == 0) {
        if (taker_diff == 0) {
            data_shoot_gametime[i, "pressure"] <- "win"
        }

        if (taker_diff == -1) {
            data_shoot_gametime[i, "pressure"] <- "lose"
        }
    }
}


n_pot <- table(data_shoot_gametime$pressure)

pot_lose <- outcome_function(data_shoot_gametime[data_shoot_gametime$pressure == "lose", ])
pot_no <- outcome_function(data_shoot_gametime[data_shoot_gametime$pressure == "no", ])
pot_win <- outcome_function(data_shoot_gametime[data_shoot_gametime$pressure == "win", ])


#### FMA ####

# who won more games ?
matches_shootout_all <- unique(data_shootout_D2.2$fcompMatchID)

data_shoot_winner <- matrix(NA, ncol = 9, nrow = length(matches_shootout_all))
colnames(data_shoot_winner) <- c("beginner", "winner", "home", "away", "name", "shortname", "fcompMatchID", "n_pen", "goal")
last_pen <- matrix(NA, ncol = 1, nrow = length(matches_shootout_all))
last_pen_res <- matrix(NA, ncol = 1, nrow = length(matches_shootout_all))
goalies_both <- matrix(NA, ncol = 2, nrow = length(matches_shootout_all))
last_pen_id <- list()
problem_last_goal <- matrix(0, ncol = 1, nrow = length(matches_shootout_all))

for (i in seq_along(matches_shootout_all)) {
    set <- data_shootout_D2.2[data_shootout_D2.2$fcompMatchID == matches_shootout_all[i], ]
    id_first <- set$order == 1
    data_shoot_winner[i, "beginner"] <- set[id_first, "PenaltyTakerTeamName"]
    data_shoot_winner[i, "home"] <- set[id_first, "homeTeamName"]
    data_shoot_winner[i, "away"] <- set[id_first, "awayTeamName"]
    data_shoot_winner[i, "name"] <- set[id_first, "CompetitionName"]
    data_shoot_winner[i, "shortname"] <- set[id_first, "CompetitionShortName"]
    data_shoot_winner[i, "fcompMatchID"] <- set[id_first, "fcompMatchID"]
    data_shoot_winner[i, "n_pen"] <- max(set$order)
    data_shoot_winner[i, "goal"] <- set[max(set$order), "outcome_binom"]

    score <- as.numeric(str_extract_all(set[set$order == max(set$order), "ScoreAfter"], "\\d+", simplify = T))

    if (score[1] > score[2]) {
        data_shoot_winner[i, "winner"] <- set[id_first, "homeTeamName"]
    }
    if (score[2] > score[1]) {
        data_shoot_winner[i, "winner"] <- set[id_first, "awayTeamName"]
    }
    if (max(set$order) <= 5) { # not possible
        data_shoot_winner[i, "winner"] <- NA
    }

    if (score[2] == score[1]) { # not possible
        data_shoot_winner[i, "winner"] <- NA
    }

    last_pen[i, 1] <- max(set$order)
    last_pen_res[i, 1] <- toString(set[set$order == max(set$order), "EventTypeName"])
    goalies_both[i, 1:2] <- set[set$order %in% 1:2, "goalie_name"]

    last_pen_id[[i]] <- set$order == max(set$order)

    # check if last penalty is goal and other team wins

    if (score[1] > score[2] & set[set$order == max(set$order), "Goal"] == 1 & set[set$order == max(set$order), "awayTeamName"] == set[set$order == max(set$order), "PenaltyTakerTeamName"]) {
        problem_last_goal[i, 1] <- 1
    }
    if (score[1] < score[2] & set[set$order == max(set$order), "Goal"] == 1 & set[set$order == max(set$order), "homeTeamName"] == set[set$order == max(set$order), "PenaltyTakerTeamName"]) {
        problem_last_goal[i, 1] <- 1
    }
}

problem_last_shot <- data_shootout_D2.2[data_shootout_D2.2$fcompMatchID %in% matches_shootout_all[problem_last_goal == 1], ]

data_shoot_winner <- data.frame(data_shoot_winner)
nrow(data_shoot_winner)
mean(data_shoot_winner$beginner == data_shoot_winner$winner)

mean(data_shoot_winner$winner == data_shoot_winner$home)
mean(data_shoot_winner$winner == data_shoot_winner$away)

# do winning teams have a conversion rate comparable to in-game?
data_winner_rate <- matrix(NA, ncol = ncol(data_shootout_D2.2), nrow = 0)

for (i in seq_len(nrow(data_shoot_winner))) {
    new_data <- data_shootout_D2.2[data_shootout_D2.2$fcompMatchID == data_shoot_winner$fcompMatchID[i] & data_shootout_D2.2$PenaltyTakerTeamName == data_shoot_winner$winner[i], ]
    data_winner_rate <- rbind(data_winner_rate, new_data)
}

outcome_function(data_winner_rate)

# fma per tournament
top_cups <- c("cups/cdr-copa-del-rey", "cups/frc-coupe-de-france", "cups/fac-fa-cup", "cups/cgb-efl-cup", "cups/dfb-dfb-pokal", "cups/cit-coppa-italia")

data_shoot_winner$shortname[data_shoot_winner$shortname == "cups"] <- substr(data_shoot_winner$name, 1, nchar(data_shoot_winner$name) - 12)[data_shoot_winner$shortname == "cups"]

data_shoot_winner[, "fma"] <- data_shoot_winner$beginner == data_shoot_winner$winner
data_fma_tournament <- table(data_shoot_winner$shortname, data_shoot_winner$fma)

fma_per_ter <- matrix(data_fma_tournament[, 2] / (data_fma_tournament[, 1] + data_fma_tournament[, 2]))
rownames(fma_per_ter) <- rownames(data_fma_tournament)
data_fma_final <- data.frame(fma_per_ter)
data_fma_final[, "n"] <- (data_fma_tournament[, 1] + data_fma_tournament[, 2])
data_fma_final <- data_fma_final[order(data_fma_final$fma_per_ter, decreasing = T), ]
data_fma_final_top <- data_fma_final[rownames(data_fma_final) %in% top_cups, ]
round(sum(data_fma_final_top[, 1] * data_fma_final_top[, 2]) / sum(data_fma_final_top[, 2]) * 100, 2)
sum(data_fma_final_top[, 2])

# what is the last penalty
mean(last_pen)
median(last_pen)
range(last_pen)
mean(last_pen < 10)

last_perc <- c(rep(0, sum(!(1:max(as.numeric(names(table(last_pen)))) %in% as.numeric(names(table(last_pen)))))), round(table(last_pen) / sum(table(last_pen)) * 100, 2))
names(last_perc) <- c(1:5, seq(11, 32, 2), names(table(last_pen)))
last_pen_output <- last_perc[order(as.numeric(names(last_perc)))]

#### tab 4 ####

# players only shootout on which place in order ?

data_players_only_shootout <- data_shootout_D2.2[!data_shootout_D2.2$player_name %in% data_regular_D2.2$player_name, ]
only_shoot_per_pos <- table(data_players_only_shootout$order) / table(data_shootout_D2.2$order) * 100

output_order <- shootout_mean_order[, c("penalties", "goal")]
output_order <- cbind(output_order, "last" = last_pen_output)
output_order <- cbind(output_order, "last_cumsum" = cumsum(output_order[, "last"]))
output_order[, "order"] <- as.numeric(rownames(output_order))

data_potential_win <- data_shoot_gametime[data_shoot_gametime$pressure == "win", ]
data_potential_lose <- data_shoot_gametime[data_shoot_gametime$pressure == "lose", ]

last_pot_win <- c(rep(0, 16), table(data_potential_win$order) / sum(table(data_potential_win$order)) * 100)
names(last_pot_win) <- c(1:5, seq(11, 32, 2), names(round(table(data_potential_win$order) / sum(table(data_potential_win$order)) * 100, 2)))
last_pot_pen_win <- last_pot_win[order(as.numeric(names(last_pot_win)))]

last_pot_lose <- c(rep(0, 17), table(data_potential_lose$order) / sum(table(data_potential_lose$order)) * 100)
names(last_pot_lose) <- c(1:5, seq(11, 32, 2), 32, names(round(table(data_potential_lose$order) / sum(table(data_potential_lose$order)) * 100, 2)))
last_pot_pen_lose <- last_pot_lose[order(as.numeric(names(last_pot_lose)))]

output_order <- cbind(output_order,
    "pot_win" = last_pot_pen_win, "pot_win_cumsum" = cumsum(last_pot_pen_win),
    "pot_lose" = last_pot_pen_lose, "pot_lose_cumsum" = cumsum(last_pot_pen_lose), "only_sh" = c(only_shoot_per_pos)
)

output_order <- output_order[, c("order", colnames(output_order)[colnames(output_order) != "order"])]

output_order <- round(output_order, 2)

write.csv(output_order, "processed_data/tab.4.csv")

sum((table(last_pen) / sum(table(last_pen)))[9:16]) * 100

sum((table(last_pen) / sum(table(last_pen)) * 100)[3:5]) # last3
sum((table(last_pen) / sum(table(last_pen)))[10:16]) * 100 #>18

#### tab 5 ####


# result of last penalty

data_last <- data_shoot_gametime[unlist(last_pen_id) == T, ]
data_not_last <- data_shoot_gametime[unlist(last_pen_id) == F, ]

outcome_function(data_last)
outcome_function(data_not_last)

last_analysis <- rbind(
    "last" = c(outcome_function(data_last), nrow(data_last)),
    "not_last" = c(outcome_function(data_not_last), nrow(data_not_last)),
    "potential_lose" = c(pot_lose, n_pot["lose"]),
    "no_potential" = c(pot_no, n_pot["no"]),
    "potential_win" = c(pot_win, n_pot["win"])
)

colnames(last_analysis) <- c("goal", "penalties")

last_analysis <- last_analysis[, c("penalties", "goal")]

write.csv(last_analysis, "processed_data/tab.5.csv")


sum((table(data_potential_win$order) / sum(table(data_potential_win$order)))[1:5]) # potential wins before sudden death
sum(table(data_potential_win$order)[1:5]) # potential wins before sudden death (#)

sum((table(data_potential_lose$order) / sum(table(data_potential_lose$order)))[1:5]) # potential loss before sudden death
sum(table(data_potential_lose$order)[1:5]) # potential loss before sudden death (#)


#### Outcome and  Appearance ####

all_takers <- unique(data_final_D2.3$player_name)
takers_both <- unique(data_regular_D2.3$player_name[data_regular_D2.3$player_name %in% data_shootout_D2.3$player_name])
takers_only_shoot <- unique(data_shootout_D2.3$player_name[!data_shootout_D2.3$player_name %in% data_regular_D2.3$player_name])
takers_only_reg <- unique(data_regular_D2.3$player_name[!data_regular_D2.3$player_name %in% data_shootout_D2.3$player_name])

length(takers_both) / length(all_takers) * 100 # both
length(takers_only_shoot) / length(all_takers) * 100 # only shootout
length(takers_only_reg) / length(all_takers) * 100 # only regular


nrow(data_regular_D2.3[data_regular_D2.3$player_name %in% takers_only_reg, ])
outcome_function(data_regular_D2.3[data_regular_D2.3$player_name %in% takers_only_reg, ])

nrow(data_regular_D2.3[data_regular_D2.3$player_name %in% takers_both, ])
outcome_function(data_regular_D2.3[data_regular_D2.3$player_name %in% takers_both, ])

nrow(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_both, ])
outcome_function(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_both, ])

nrow(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_only_shoot, ])
outcome_function(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_only_shoot, ])



nrow(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_only_shoot, ])
outcome_function(data_shootout_D2.3[data_shootout_D2.3$player_name %in% takers_only_shoot, ])



times <- 1

players_both <- unique(data_shootout_D2.3$player_name[data_shootout_D2.3$player_name %in% data_regular_D2.3$player_name])
data_both <- data_final_D2.3[data_final_D2.3$player_name %in% players_both, ]
nrow(data_both)

table(table(data_both$player_name))
players_both_once <- names(table(data_both$player_name)[table(data_both$player_name) == times])
players_both_more <- players_both[!players_both %in% players_both_once]

data_shootout_D2.3_both_once <- data_shootout_D2.3[data_shootout_D2.3$player_name %in% players_both_once, ]
data_shootout_D2.3_both_more <- data_shootout_D2.3[data_shootout_D2.3$player_name %in% players_both_more, ]

data_regular_D2.3_both_once <- data_regular_D2.3[data_regular_D2.3$player_name %in% players_both_once, ]
data_regular_D2.3_both_more <- data_regular_D2.3[data_regular_D2.3$player_name %in% players_both_more, ]

# decriptives
length(players_both)
nrow(data_both)
nrow(data_regular_D2.3_both_once)
nrow(data_regular_D2.3_both_more)
nrow(data_shootout_D2.3_both_once)
nrow(data_shootout_D2.3_both_more)


# regular setting
only_times_reg_tab <- outcome_function(data_regular_D2.3_both_once, digits = 10)
more_than_times_reg_tab <- outcome_function(data_regular_D2.3_both_more, digits = 10)

# shootout setting
only_times_sh_tab <- outcome_function(data_shootout_D2.3_both_once, digits = 10)
more_than_times_sh_tab <- outcome_function(data_shootout_D2.3_both_more, digits = 10)

fig9_data <- rbind(only_times_reg_tab, more_than_times_reg_tab, only_times_sh_tab, more_than_times_sh_tab)
rownames(fig9_data) <- c("= 1 in game", "> 1 in game", "= 1 shootout", "> 1 shootout")
fig9_data <- data.frame(round(fig9_data, 2))
fig9_data[, "penalties"] <- c(
    nrow(data_regular_D2.3_both_once), nrow(data_regular_D2.3_both_more),
    nrow(data_shootout_D2.3_both_once), nrow(data_shootout_D2.3_both_more)
)


write.csv(fig9_data, "processed_data/fig.9.csv")

## Table 8
# players that either appeared in one of the settings

players_either <- unique(data_final_D2.3$player_name[!data_final_D2.3$player_name %in% players_both])
length(players_either)
data_either <- data_final_D2.3[data_final_D2.3$player_name %in% players_either, ]
nrow(data_either)

data_regular_only <- data_regular_D2.3[data_regular_D2.3$player_name %in% players_either, ]
data_shootout_only <- data_shootout_D2.3[data_shootout_D2.3$player_name %in% players_either, ]

results_regular_only <- c(outcome_function(data_regular_only), nrow(data_regular_only))
results_shootout_only <- c(outcome_function(data_shootout_only), nrow(data_shootout_only))
results_only <- matrix(c(results_regular_only, results_shootout_only), ncol = 4, byrow = T)
colnames(results_only) <- c("goal", "miss", "save", "penalties")
rownames(results_only) <- c("in-game", "shootout")
results_only <- results_only[, c("penalties", "goal", "miss", "save")]

write.csv(results_only, "processed_data/tab.6.csv")


########### log reg ###########


var_shootout <- data_final_D2.2$TypePhaseName == "Penalty Shoot-Out"

# var_top_leagues <- id_top_leagues==1

# national league as reference
var_national_teams <- data_final_D2.2$CompetitionShortName == "nat_teams"
var_int_cups <- data_final_D2.2$CompetitionShortName == "int_cups"
var_nat_cups <- data_final_D2.2$CompetitionShortName == "leag"

# shooters in both as reference
only_shootout <- data_shootout_D2.2$player_name[!data_shootout_D2.2$player_name %in% data_regular_D2.2$player_name]
var_only_shoot <- data_final_D2.2$player_name %in% only_shootout

only_reg <- data_regular_D2.2$player_name[!data_regular_D2.2$player_name %in% data_shootout_D2.2$player_name]
var_only_reg <- data_final_D2.2$player_name %in% only_reg

# experience

both_players <- data_shootout_D2.2$player_name[data_shootout_D2.2$player_name %in% data_regular_D2.2$player_name]
two_both <- names(table(data_final_D2.2[data_final_D2.2$player_name %in% both_players, "player_name"])[table(data_final_D2.2[data_final_D2.2$player_name %in% both_players, "player_name"]) > 2])

var_experience <- data_final_D2.2$player_name %in% two_both

# season
var_season <- data_final_D2.2$SeasonID

# home team == taker
var_home <- data_final_D2.2$homeTeamName == data_final_D2.2$PenaltyTakerTeamName

# logreg
logreg_final <- glm(data_final_D2.2$Goal ~ var_shootout + var_national_teams + var_int_cups + var_nat_cups +
    var_only_shoot + var_only_reg + var_experience + var_season + var_home, family = binomial)

# stargazer(logreg_final)
summary_logreg <- summary(logreg_final)

options(scipen = 0)

output_reg <- data.frame(summary_logreg$coefficients)
output_reg[, "exp(estimate)"] <- exp(output_reg$Estimate)
output_reg[, "Pr...z.."] <- p.adjust(output_reg[, "Pr...z.."], method = "holm")
output_reg <- output_reg[, c("Estimate", "exp(estimate)", "Std..Error", "z.value", "Pr...z..")]
output_reg <- round(output_reg, 4)


write.csv(output_reg, "processed_data/tab.7.csv")


#### other ideas ####

## penalties awarded to home team ##

mean(data_regular_D2.2$PenaltyTakerTeamName == data_regular_D2.2$homeTeamName)
mean(data_regular_D2.3$PenaltyTakerTeamName == data_regular_D2.3$homeTeamName)

mean(data_regular_D2.2$PenaltyTakerTeamName == data_regular_D2.2$awayTeamName)


# score at penalty
sum(data_regular_D2.3$ScoreBefore == "100:100")
data_score_analysis <- data_regular_D2.3[data_regular_D2.3$ScoreBefore != "100:100", ]
data_score_split <- data.frame(str_split(data_score_analysis$ScoreBefore, pattern = ":", simplify = T))

home_id <- data_score_analysis$homeTeamName == data_score_analysis$PenaltyTakerTeamName
away_id <- data_score_analysis$awayTeamName == data_score_analysis$PenaltyTakerTeamName

data_score_analysis[home_id, "score_taker_team"] <- data_score_split[home_id, 1]
data_score_analysis[away_id, "score_taker_team"] <- data_score_split[away_id, 2]

data_score_analysis[home_id, "score_goalie_team"] <- data_score_split[home_id, 2]
data_score_analysis[away_id, "score_goalie_team"] <- data_score_split[away_id, 1]

data_score_leading <- data_score_analysis[data_score_analysis$score_taker_team > data_score_analysis$score_goalie_team, ]
data_score_draw <- data_score_analysis[data_score_analysis$score_taker_team == data_score_analysis$score_goalie_team, ]
data_score_behind <- data_score_analysis[data_score_analysis$score_taker_team < data_score_analysis$score_goalie_team, ]

outcome_function(data_score_leading)
outcome_function(data_score_draw)
outcome_function(data_score_behind)

nrow(data_score_leading)
nrow(data_score_draw)
nrow(data_score_behind)

data_tab8 <- rbind(
    c(nrow(data_score_leading), outcome_function(data_score_leading)),
    c(nrow(data_score_draw), outcome_function(data_score_draw)),
    c(nrow(data_score_behind), outcome_function(data_score_behind))
)
colnames(data_tab8) <- c("penalties", "goal", "miss", "save")
rownames(data_tab8) <- c("lead", "draw", "behind")

write.csv(data_tab8, "processed_data/tab.8.csv")


### % Q : in favor of who? not a problem if this is for the second team

id_before_10 <- matches_shootout_all[last_pen < 10]

data_before_10 <- data_shoot_winner[data_shoot_winner$fcompMatchID %in% id_before_10, ]
table(data_before_10$n_pen)

mean(data_before_10$beginner == data_before_10$winner)
sum(data_before_10$beginner == data_before_10$winner)


## shootout order which team won

data_rejected <- data_shoot_winner

data_rejected[data_rejected$winner == data_rejected$beginner, "team_win"] <- "A"
data_rejected[!data_rejected$winner == data_rejected$beginner, "team_win"] <- "B"

data_tab_rejeted <- t(table(data_rejected$team_win, data_rejected$n_pen))
data_tab_rejeted <- data_tab_rejeted[order(as.numeric(rownames(data_tab_rejeted)), decreasing = F), ]
data_all_pen <- matrix(NA, ncol = 1, nrow = nrow(data_tab_rejeted))

for (i in seq_len(nrow(data_tab_rejeted))) {
    data_all_pen[i, 1] <- shootout_mean_order[shootout_mean_order$order == rownames(data_tab_rejeted)[i], "penalties"]
}

data_tab_rejeted <- cbind(data_tab_rejeted, data_all_pen)
colnames(data_tab_rejeted) <- c("A", "B", "all")
data_tab_rejeted <- data.frame(cbind(c("B3", "A4", "B4", "A5", "B5", paste("B", 6:16, sep = "")), data_tab_rejeted))
data_tab_rejeted[, 2] <- as.numeric(data_tab_rejeted[, 2])
data_tab_rejeted[, 3] <- as.numeric(data_tab_rejeted[, 3])
data_tab_rejeted[, 4] <- as.numeric(data_tab_rejeted[, 4])

data_tab_rejeted[, "no_win"] <- data_tab_rejeted[, "all"] - rowSums(data_tab_rejeted[, c("A", "B")])

data_tab_out <- data_tab_rejeted[, c("V1", "A", "B", "no_win")]
colnames(data_tab_out) <- c("position", "A", "B", "no_win")

write.csv(data_tab_out, "processed_data/tab_rejected.csv")


## outlier 3 result ##
data_outlier_3 <- data_shootout_D2.2[data_shootout_D2.2$order == 3, ]
n_data_outlier_3 <- table(data_outlier_3$ScoreBefore)
names_3 <- names(n_data_outlier_3)
data_out_outlier3 <- matrix(NA, nrow = 2, ncol = length(names_3))
colnames(data_out_outlier3) <- names_3
rownames(data_out_outlier3) <- c("n", "goal")
data_out_outlier3["n", ] <- n_data_outlier_3

for (i in seq_along(names_3)) {
    data_outlier_3_res <- data_outlier_3[data_outlier_3$ScoreBefore == names_3[i], ]
    data_out_outlier3["goal", names_3[i]] <- outcome_function(data_outlier_3_res)
}

write.csv(data_out_outlier3, "processed_data/tab_outlier.csv")

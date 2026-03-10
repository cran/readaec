## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 8,
  fig.height = 5,
  message   = FALSE,
  warning   = FALSE
)

## ----libs---------------------------------------------------------------------
library(readaec)
library(dplyr)
library(ggplot2)
library(purrr)

## ----elections----------------------------------------------------------------
list_elections()

## ----tpp-trend----------------------------------------------------------------
# AEC CSV downloads are available from 2007 onwards
years <- list_elections()$year[list_elections()$has_downloads]

tpp_richmond <- map_dfr(years, function(yr) {
  get_tpp(yr) |>
    filter(tolower(division) == "richmond") |>
    select(division, division_id, state, alp_pct, lnp_pct, total_votes, year)
})

tpp_richmond

## ----tpp-plot-----------------------------------------------------------------
ggplot(tpp_richmond, aes(x = year)) +
  geom_line(aes(y = alp_pct, colour = "ALP"), linewidth = 1.2) +
  geom_point(aes(y = alp_pct, colour = "ALP"), size = 3) +
  geom_line(aes(y = lnp_pct, colour = "LNP/Nat"), linewidth = 1.2) +
  geom_point(aes(y = lnp_pct, colour = "LNP/Nat"), size = 3) +
  geom_hline(yintercept = 50, linetype = "dashed", colour = "grey60") +
  annotate("text", x = min(years) + 0.3, y = 51,
           label = "50% — majority", size = 3, colour = "grey50", hjust = 0) +
  scale_colour_manual(values = c("ALP" = "#E4281B", "LNP/Nat" = "#1C4F9C")) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(limits = c(30, 70),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Richmond (NSW): two-party preferred vote, 2001–2025",
    subtitle = "ALP vs LNP/National Coalition",
    x        = NULL,
    y        = "TPP vote share",
    colour   = NULL,
    caption  = "Source: Australian Electoral Commission via readaec"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

## ----members------------------------------------------------------------------
members_richmond <- map_dfr(years, function(yr) {
  tryCatch(
    get_members_elected(yr) |>
      filter(tolower(divisionnm) == "richmond") |>
      select(divisionnm, surname, givennm, partyab, stateab, year),
    error = function(e) NULL
  )
})

members_richmond |>
  select(year, given_name = givennm, surname, party = partyab) |>
  arrange(year)

## ----fp-data------------------------------------------------------------------
fp_richmond <- map_dfr(years, function(yr) {
  get_fp(yr) |>
    filter(tolower(division) == "richmond") |>
    select(year, division, surname, given_name, party, party_name, total_votes)
})

## ----fp-totals----------------------------------------------------------------
# Total formal votes per year (for calculating shares)
fp_totals <- fp_richmond |>
  group_by(year) |>
  summarise(total_formal = sum(total_votes, na.rm = TRUE))

# Major party shares
fp_major <- fp_richmond |>
  filter(party %in% c("ALP", "NAT", "NP", "LIB", "GRN")) |>
  left_join(fp_totals, by = "year") |>
  mutate(
    pct = round(total_votes / total_formal * 100, 1),
    party_label = case_when(
      party %in% c("NAT", "NP") ~ "Nationals",
      party == "ALP"             ~ "ALP",
      party == "LIB"             ~ "Liberal",
      party == "GRN"             ~ "Greens",
      TRUE                       ~ party
    )
  )

## ----fp-plot------------------------------------------------------------------
ggplot(fp_major, aes(x = year, y = pct, colour = party_label)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(
    "ALP"       = "#E4281B",
    "Nationals" = "#006644",
    "Liberal"   = "#1C4F9C",
    "Greens"    = "#10C25B"
  )) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title   = "Richmond (NSW): first preference vote shares",
    x       = NULL,
    y       = "First preference share",
    colour  = NULL,
    caption = "Source: AEC via readaec"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

## ----swing-pairs--------------------------------------------------------------
# Build consecutive election pairs from years with downloads
election_pairs <- Map(c, head(years, -1), tail(years, -1))

nsw_swings <- map_dfr(election_pairs, function(pair) {
  get_swing(pair[1], pair[2], state = "NSW") |>
    filter(!redistribution_flag) |>
    mutate(period = paste0(pair[1], "–", pair[2]))
})

## ----swing-richmond-----------------------------------------------------------
richmond_swing <- nsw_swings |>
  filter(tolower(division) == "richmond") |>
  select(period, division, alp_swing, year_from, year_to)

nsw_avg_swing <- nsw_swings |>
  group_by(period, year_from, year_to) |>
  summarise(avg_alp_swing = mean(alp_swing, na.rm = TRUE), .groups = "drop")

swing_comparison <- richmond_swing |>
  left_join(nsw_avg_swing, by = c("period", "year_from", "year_to")) |>
  mutate(relative_swing = alp_swing - avg_alp_swing)

swing_comparison |>
  select(period, richmond_swing = alp_swing, nsw_avg = avg_alp_swing,
         relative_swing)

## ----swing-plot---------------------------------------------------------------
swing_comparison |>
  select(period, richmond_swing = alp_swing, nsw_avg = avg_alp_swing) |>
  tidyr::pivot_longer(c(richmond_swing, nsw_avg),
                      names_to = "series", values_to = "swing") |>
  mutate(series = if_else(series == "richmond_swing", "Richmond", "NSW average")) |>
  ggplot(aes(x = period, y = swing, fill = series)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Richmond" = "#E4281B", "NSW average" = "grey70")) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pp")) +
  labs(
    title   = "Richmond ALP swing vs NSW average",
    x       = NULL,
    y       = "ALP swing (percentage points)",
    fill    = NULL,
    caption = "Source: AEC via readaec"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.minor = element_blank())

## ----turnout------------------------------------------------------------------
turnout_richmond <- map_dfr(years, function(yr) {
  get_turnout(yr) |>
    filter(tolower(divisionnm) == "richmond") |>
    select(divisionnm, year, everything())
})

turnout_richmond

## ----enrolment----------------------------------------------------------------
enrolment_richmond <- map_dfr(years, function(yr) {
  get_enrolment(yr) |>
    filter(tolower(divisionnm) == "richmond") |>
    select(divisionnm, year, everything())
})

enrolment_richmond


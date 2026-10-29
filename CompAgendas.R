#Edward Blaha Problem Set 8: Policy Agendas
#What do the two main parties in the United States, the United Kingdom, and Germany talk about in their platforms,
#and how has that changed over time?

#Datasets used 
#1. US Democratic Party Platforms (1948-2024)c
#2. US Republican Party Platforms (1948-2024)

#3. UK Party Manifestos (1983-2015)- Conservative, Labour Lib Dem manifestos
#4. Germany Party Manifestos (1949-2013)-each major party's manifesto


#The two main parties in each country are combined into one agenda for that country:
#US = Democrats + Republicans, UK = Labour + Conservatives, Germany = SPD + CDU/CSU


library(tidyverse)
library(ggplot2)


#Datasets downloaded from the Comparative Agendas Project (https://www.comparativeagendas.net/)
DemPlatforms <- read_csv("/Users/edwardblaha/DataWrangling/Week9/Dem_partyplatform_20.2.csv")
RepPlatforms <- read_csv("/Users/edwardblaha/DataWrangling/Week9/US-Party-Rep_Platform_19.3_0.csv")
UKManifestos <- read_csv("/Users/edwardblaha/DataWrangling/Week9/uk_manifestos_v1__1.csv")
DEManifestos <- read_csv("/Users/edwardblaha/DataWrangling/Week9/manifesto_1949-2013_website-release_2.5_2_3.csv")



DemPlatforms_clean <- DemPlatforms |>
  mutate(country = "United States", party = "Democrats") |>
  select(country, party, year, majortopic)

RepPlatforms_clean <- RepPlatforms |>
  mutate(country = "United States", party = "Republicans") |>
  select(country, party, year, majortopic)


UKManifestos_clean <- UKManifestos |>
  filter(filter_lab == 1 | filter_con == 1) |>
  mutate(country = "United Kingdom",
         party = if_else(filter_lab == 1, "Labour", "Conservatives")) |>
  select(country, party, year, majortopic)

#Germany file uses different column names for the year and topic
DEManifestos_clean <- DEManifestos |>
  filter(party_name %in% c("SPD", "CDU")) |>
  mutate(country = "Germany") |>
  rename(party = party_name, year = election_year, majortopic = cap_topic) |>
  select(country, party, year, majortopic)

AllPlatforms <- bind_rows(DemPlatforms_clean, RepPlatforms_clean, UKManifestos_clean, DEManifestos_clean)



policy_topics <- c(1:10, 12:21, 23)

#Putting the 21 topics into 5 groups
#Foreign Policy & Defense = defense (16), international affairs (19), foreign trade (18)
#Social Policy = health (3), education (6), social welfare (13), housing (14), culture (23)
#Economy = economy (1), agriculture (4), labor (5), transportation (10), banking & commerce (15), science & technology (17)
#Crime & Immigration = immigration (9), crime (12)
#Environment & Energy = environment (7), energy (8), public lands (21)
#Civil rights (2) and government operations (20) are left as Other
AllPlatforms <- AllPlatforms |>
  filter(majortopic %in% policy_topics) |>
  mutate(category = case_when(
    majortopic %in% c(16, 18, 19) ~ "Foreign Policy & Defense",
    majortopic %in% c(3, 6, 13, 14, 23) ~ "Social Policy",
    majortopic %in% c(1, 4, 5, 10, 15, 17) ~ "Economy",
    majortopic %in% c(9, 12) ~ "Crime & Immigration",
    majortopic %in% c(7, 8, 21) ~ "Environment & Energy",
    TRUE ~ "Other"))

#Number of sentences per party and year
AllPlatforms |>
  count(country, party, year) |>
  print(n = 100)


#Each group's share of each party's platform
PartyShares <- AllPlatforms |>
  count(country, party, year, category) |>
  group_by(country, party, year) |>
  mutate(share = n / sum(n) * 100) |>
  ungroup()

#Combining the two parties: averaging their shares so a longer platform doesn't count more.
#Only keeping elections where both parties have a platform (drops the US in 2020). Code AI assisted. 
CountryShares <- PartyShares |>
  group_by(country, year) |>
  filter(n_distinct(party) == 2) |>
  group_by(country, year, category) |>
  summarise(share = mean(share), .groups = "drop")

#Grouping elections by decade so one election doesn't swing the line.
#US 1948 and Germany 1949 are counted with the 1950s.
CountryDecades <- CountryShares |>
  mutate(decade = floor(year / 10) * 10,
         decade = if_else(decade == 1940, 1950, decade)) |>
  group_by(country, decade, category) |>
  summarise(share = mean(share),
            elections = n(), .groups = "drop")


CountryDecades <- CountryDecades |>
  group_by(decade) |>
  filter(n_distinct(country) > 1) |>
  ungroup()
CountryDecades


country_colors <- c("United States" = "steelblue",
                    "United Kingdom" = "purple4",
                    "Germany" = "darkorange")


#Graph 1: Foreign policy & defense over time
ForeignDecades <- CountryDecades |>
  filter(category == "Foreign Policy & Defense")
ForeignDecades

ForeignPlot <- ggplot(ForeignDecades, aes(x = decade, y = share, color = country)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4) +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(breaks = seq(1950, 2010, by = 10),
                     labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s")) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(title = "Foreign Policy & Defense in Party Platforms",
       subtitle = "Share of the two main parties' platforms, by decade",
       x = NULL, y = "Share of Platform (%)", color = NULL,
       caption = "Defense, international affairs, and foreign trade.\nSource: Comparative Agendas Project") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.background = element_rect(color = "grey40", fill = "white", linewidth = 0.8),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16))
ForeignPlot
ggsave("Graph1_ForeignPolicy.png", ForeignPlot, width = 11, height = 7)


#Graph 2: Social policy over time
SocialDecades <- CountryDecades |>
  filter(category == "Social Policy")
SocialDecades

SocialPlot <- ggplot(SocialDecades, aes(x = decade, y = share, color = country)) +
  geom_line(linewidth = 1.8) +
  geom_point(size = 4) +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(breaks = seq(1950, 2010, by = 10),
                     labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s")) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(title = "Social Policy in Party Platforms",
       subtitle = "Share of the two main parties' platforms, by decade",
       x = NULL, y = "Share of Platform (%)", color = NULL,
       caption = "Health, education, social welfare, and housing.\nSource: Comparative Agendas Project") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.background = element_rect(color = "grey40", fill = "white", linewidth = 0.8),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16))
SocialPlot
ggsave("Graph2_SocialPolicy.png", SocialPlot, width = 11, height = 7)


#Graph 3: How the three countries compare today
#Using the 2010s, the last decade all three countries have data
Recent <- CountryDecades |>
  filter(decade == 2010, category != "Other") |>
  mutate(category = factor(category, levels = c("Social Policy", "Economy", "Foreign Policy & Defense",
                                                "Crime & Immigration", "Environment & Energy")))
Recent

RecentPlot <- ggplot(Recent, aes(x = category, y = share, fill = country)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = round(share)), position = position_dodge(width = 0.8),
            vjust = -0.5, size = 5.5) +
  scale_fill_manual(values = country_colors) +
  scale_y_continuous(limits = c(0, 40)) +
  labs(title = "What Parties Focus On in the 2010s",
       subtitle = "Share of the two main parties' platforms",
       x = NULL, y = "Share of Platform (%)", fill = NULL,
       caption = "US = 2012 and 2016. UK = 2010 and 2015. Germany = 2013.\nSource: Comparative Agendas Project") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        plot.subtitle = element_text(hjust = 0.5),
        plot.title.position = "plot",
        plot.background = element_rect(color = "grey40", fill = "white", linewidth = 0.8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16))
RecentPlot
ggsave("Graph3_2010s.png", RecentPlot, width = 14, height = 7)
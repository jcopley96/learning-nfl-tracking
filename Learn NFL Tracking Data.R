options(repr.plot.width=12, repr.plot.height=8, warn = -1)
suppressMessages(library(tidyverse))


train <- read_csv("~/Desktop/train.csv", col_types = cols())

#created dummy variable to see if carrier moved to left
train <- train %>% 
  mutate(ToLeft = PlayDirection == "left", 
         IsBallCarrier = NflId == NflIdRusher)

#tweaks to team abbreviations
train$VisitorTeamAbbr[train$VisitorTeamAbbr == "ARI"] <- "ARZ"
train$HomeTeamAbbr[train$HomeTeamAbbr == "ARI"] <- "ARZ"

train$VisitorTeamAbbr[train$VisitorTeamAbbr == "BAL"] <- "BLT"
train$HomeTeamAbbr[train$HomeTeamAbbr == "BAL"] <- "BLT"

train$VisitorTeamAbbr[train$VisitorTeamAbbr == "CLE"] <- "CLV"
train$HomeTeamAbbr[train$HomeTeamAbbr == "CLE"] <- "CLV"

train$VisitorTeamAbbr[train$VisitorTeamAbbr == "HOU"] <- "HST"
train$HomeTeamAbbr[train$HomeTeamAbbr == "HOU"] <- "HST"

set.seed(200)

sample_plays <- train %>% 
  select(PlayId, ToLeft) %>% 
  group_by(ToLeft) %>%
  sample_n(3)

sample_chart_v1 <- sample_plays %>% 
  inner_join(train)

#this code is to show you what charts look like uncleaned!
sample_chart_v1 %>% 
  ggplot(aes(x = X, y = Y, fill = Team)) + 
  geom_point(pch = 21, size = 3) + 
  geom_point(data = filter(sample_chart_v1, IsBallCarrier), 
             size = 4, pch = 21,
             fill = "black") +
  scale_colour_brewer(palette = "Set2") + 
  scale_x_continuous(breaks = c(0:10)*10) + 
  labs(x = "X", y = "Y", title = "Sample plays") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank()) + 
        facet_wrap(~PlayId, nrow = 3)

train_1 <- train %>% 
  mutate(TeamOnOffense = ifelse(PossessionTeam == HomeTeamAbbr, "home", "away"),  
         IsOnOffense = Team == TeamOnOffense,  ## Is player on offense?
         YardsFromOwnGoal = ifelse(as.character(FieldPosition) == PossessionTeam, 
                                   YardLine, 50 + (50-YardLine)), 
         YardsFromOwnGoal = ifelse(YardLine == 50, 50, YardsFromOwnGoal),  
         X_std = ifelse(ToLeft, 120-X, X) - 10, ## Standardizes X
         Y_std = ifelse(ToLeft, 160/3-Y, Y))    ## Standardized Y

sample_chart_v2 <- sample_plays %>% 
  inner_join(train_1)

sample_chart_v2 %>% 
  ggplot(aes(x = X_std, y = Y_std, fill = IsOnOffense)) + 
  geom_point(pch = 21, size = 4) + 
  geom_point(data = filter(sample_chart_v2, IsBallCarrier), 
             size = 1.5, pch = 21,
             fill = "black") +
  scale_colour_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = 0)) + 
  geom_vline(aes(xintercept = 100)) + 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(0:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate", 
       title = "Sample plays, standardized", 
       subtitle = "Offense moving left to right") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank()) + 
  facet_wrap(~PlayId, nrow = 3)


#player space and Voronoi Areas:
library(ggvoronoi)
sample_chart_v2 %>% 
  filter(PlayId == 20171120000963) %>% 
  ggplot(aes(x = X_std, y = Y_std, fill = IsOnOffense)) + 
  stat_voronoi(geom="path") +
  geom_point(pch = 21, size = 4) + 
  geom_point(data = filter(sample_chart_v2, IsBallCarrier, 
                           PlayId == 20171120000963), 
             size = 1.5, pch = 21,
             fill = "black") +
  scale_colour_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = 0)) + 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(0:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate", title = "Sample plays, standardized", 
       subtitle = "Offense moving left to right") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank())

#player direction
### Angle chart
df_bc <- train_1 %>% 
  filter(IsBallCarrier) %>% 
  select(DisplayName, PossessionTeam, PlayId, Dir, ToLeft, PlayDirection,
         IsOnOffense, X_std, Y_std, YardsFromOwnGoal, Down, Distance, Yards)

options(repr.plot.width=8, repr.plot.height=5)
df_bc %>% 
  ggplot(aes(Dir)) + 
  geom_histogram() + 
  facet_wrap(~PlayDirection)+ 
  theme_bw(14)+ 
  labs(title = "Player directions, raw directional data")

df_bc <- df_bc %>% 
  mutate(Dir_std_1 = ifelse(ToLeft & Dir < 90, Dir + 360, Dir), 
         Dir_std_1 = ifelse(!ToLeft & Dir > 270, Dir - 360, Dir_std_1))


df_bc %>% 
  ggplot(aes(Dir_std_1)) + 
  geom_histogram() + 
  facet_wrap(~PlayDirection) + 
  theme_bw(14)+ 
  labs(title = "Player directions, adjusted")

df_bc <- df_bc %>% 
  mutate(Dir_std_2 = ifelse(ToLeft, Dir_std_1 - 180, Dir_std_1))

df_bc %>% 
  ggplot(aes(Dir_std_2)) + 
  geom_histogram() + 
  facet_wrap(~PlayDirection) + 
  theme_bw(14)+ 
  scale_x_continuous(breaks = c(-90, 0, 90, 180, 270), lim = c(-90, 270), 
                     labels = c("-90", "0 \n left", 
                                "90 \n straight ahead", "180 \n right", "270")) + 
  labs(title = "Player directions, adjusted")

df_bc %>% 
  filter(PlayId == 20170910001102) %>% 
  select(PlayId, DisplayName, Dir, ToLeft, PlayDirection, 
         X_std, Y_std, YardsFromOwnGoal, Dir_std_1, Dir_std_2, Yards)


train_2 <- train_1 %>% 
  mutate(Dir_std_1 = ifelse(ToLeft & Dir < 90, Dir + 360, Dir), 
         Dir_std_1 = ifelse(!ToLeft & Dir > 270, Dir - 360, Dir_std_1), 
         Dir_std_2 = ifelse(ToLeft, Dir_std_1 - 180, Dir_std_1))

train_2 <- train_2 %>% 
  mutate(X_std_end = S*cos((90-Dir_std_2)*pi/180) + X_std, 
         Y_std_end = S*sin((90-Dir_std_2)*pi/180) + Y_std)

samp_play <- "20170910001102"

options(repr.plot.width=10, repr.plot.height=6)
train_2 %>% 
  filter(PlayId == samp_play) %>% 
  ggplot(aes(X_std, Y_std, fill = IsOnOffense), colour = "black")  + 
  geom_point(size = 4, pch = 21) + 
  geom_segment(aes(x = X_std, y = Y_std, xend = X_std_end,
                   yend = Y_std_end, colour = IsOnOffense), 
               arrow = arrow(length = unit(.5,"cm"))) + 
  geom_point(data = filter(train_2, IsBallCarrier, 
                           PlayId == samp_play), 
             size = 1.5, pch = 21,
             fill = "black") +
  scale_colour_brewer(palette = "Set2")+ 
  scale_fill_brewer(palette = "Set2")+ 
  geom_vline(aes(xintercept = YardsFromOwnGoal), 
             colour = "black", lty = 2) + 
  scale_x_continuous(breaks = c(7:10)*10) + 
  labs(x = "Distance from offensive team's own end zone", 
       y = "Y coordinate", title = paste0("PlayId ", samp_play, ": player direction"), 
       subtitle = "Offense moving left to right") + 
  theme_bw(14) + 
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major.y =element_blank())


#running back sonars:
df_bc <- df_bc %>% 
  mutate(IsSuccess = ifelse(Down == 1, Yards >= Distance/2, 
                            ifelse(Down == 2, Yards >= Distance/2, Yards >= Distance)))

### Team sonars
round_angle <- 15
df_bc <- df_bc %>% 
  mutate(AngleRound = round(Dir_std_2/round_angle)*round_angle)

sonar <- df_bc %>%
  mutate(N=n()) %>%
  group_by(AngleRound, PossessionTeam)  %>%
  mutate(n_rushes = n(), n_angle=n_rushes/N) %>%
  filter(n_rushes >= 5) %>% 
  ungroup() %>%
  group_by(PossessionTeam) %>% 
  mutate(maxN = max(n_angle),
         AngleNorm = n_angle/maxN) %>%
  ungroup() %>%
  group_by(AngleRound, PossessionTeam, N)%>%
  summarize(AngleNorm = mean(AngleNorm),
            SuccessRate = mean(IsSuccess)) %>% 
  arrange(PossessionTeam)

library(viridis)
options(repr.plot.width=12, repr.plot.height=8)
p_rb <- ggplot(sonar) +
  geom_bar(aes(x=AngleRound, y = AngleNorm, fill= SuccessRate), stat="identity") +
  scale_x_continuous(breaks=seq(0, 360, by=90), limits=c(0, 360)) +
  coord_polar(start=4.58, direction=1) + ### rotate (270-15/2) degrees, or 4.58 radians
  scale_fill_viridis("Success %", na.value="#FDE725FF", 
                     limits = c(0.3, 0.7), 
                     breaks = c(0.3, 0.4, 0.5, 0.6, 0.7), 
                     labels = scales::percent) +
  labs(x='', y='') +
  theme_void(15)+
  theme(plot.title = element_text(hjust=0.5),
        legend.title = element_text(hjust = 1),
        #legend.position = "top", #uncomment to remove colorbar
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA), 
        panel.spacing = unit(0, "lines"), 
        plot.margin = margin(0, 0, 0, 0, "cm")) + 
  facet_wrap(~PossessionTeam, nrow = 4) + 
  labs(title = "Team sonars, 2017-2018", subtitle = "  ")
p_rb



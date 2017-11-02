### - SCRIPT FOR SCRAPING COMMENTS FROM OILERSNATION.COM FOR TEXT AND SENTIMENT ANALYSIS - ###

library('rvest')
library('tidytext')
library('plyr')
library('dplyr')
library('tidyr')
library('ggplot2')


### SCRAPING AND CLEANING THE COMMENTS

#Start by scraping the comments from each individual blog post, one from each game
on_post_17_10 <- read_html('https://oilersnation.com/2017/10/28/post-game-news-10-0-edmonton-oilers-vs-washington-capitals/comment-page-1/#comments')
  on_post_17_10 <- on_post_17_10%>%html_nodes(".comment-body")%>%html_text()

on_post_17_9 <- read_html('https://oilersnation.com/2017/10/26/post-game-news-9-0-edmonton-oilers-vs-dallas-stars/comment-page-1/#comments')
  on_post_17_9 <- on_post_17_9%>%html_nodes(".comment-body")%>%html_text()

on_post_17_8 <- read_html('https://oilersnation.com/2017/10/24/post-game-gdb-8-0-recap-edmonton-oilers-vs-pittsburgh-penguins/comment-page-1/#comments')
  on_post_17_8 <- on_post_17_8%>%html_nodes(".comment-body")%>%html_text()

on_post_17_7 <- read_html('https://oilersnation.com/2017/10/21/post-game-news-gdb-7-0-recap-edmonton-oilers-vs-philadelphia-flyers//comment-page-1/#comments')
  on_post_17_7 <- on_post_17_7%>%html_nodes(".comment-body")%>%html_text()

on_post_17_6 <- read_html('https://oilersnation.com/2017/10/19/post-game-news-gdb-6-0-recap-edmonton-oilers-vs-chicago-blackhawks/comment-page-1/#comments')
  on_post_17_6 <- on_post_17_6%>%html_nodes(".comment-body")%>%html_text()

on_post_17_5 <- read_html('https://oilersnation.com/2017/10/17/post-game-news-gdb-5-0-recap-edmonton-oilers-vs-carolina-hurricanes/comment-page-1/#comments')
  on_post_17_5 <- on_post_17_5%>%html_nodes(".comment-body")%>%html_text()

on_post_17_4 <- read_html('https://oilersnation.com/2017/10/14/gdb-4-0-post-game-recap-edmonton-oilers-vs-ottawa-senators/comment-page-1/#comments')
  on_post_17_4 <- on_post_17_4%>%html_nodes(".comment-body")%>%html_text()

on_post_17_3 <- read_html('https://oilersnation.com/2017/10/09/gdb-3-0-post-game-wrap-up-edmonton-oilers-vs-winnipeg-jets/comment-page-1/#comments')
  on_post_17_3 <- on_post_17_3%>%html_nodes(".comment-body")%>%html_text()

on_post_17_2 <- read_html('https://oilersnation.com/2017/10/07/canucks-3-oilers-2-post-game-oil-spills-saturday-night-starting-goalie-voodoo/comment-page-1/#comments')
  on_post_17_2 <- on_post_17_2%>%html_nodes(".comment-body")%>%html_text()

on_post_17_1 <- read_html('https://oilersnation.com/2017/10/04/gdb-1-0-post-game-recap-edmonton-oilers-vs-calgary-flames/comment-page-1/#comments')
  on_post_17_1 <- on_post_17_1%>%html_nodes(".comment-body")%>%html_text()

#combine all comment sections to one matrix/data frame
on_17 <- c(on_post_17_1, on_post_17_2, on_post_17_3, on_post_17_4, on_post_17_5, on_post_17_6, on_post_17_7, on_post_17_8, on_post_17_9,on_post_17_10)


#Use gsub to remove '\t' and '\n' from posts, and to remove any text except for the comments themselves
text_17 <- gsub("[\t\n]", "", on_17)

text_17 <- gsub("Reply", "", text_17)

text_17 <- gsub(".*CHEERS[0-9]+", "", text_17)

text_17 <- gsub("[0-9]+Trashes [0-9]+CHEERS ", "", text_17)

#Put comments in to data frame
text_17 <- data.frame(text_17)

#Convert comments from factors to characters
text_17$text_17 <- as.character((text_17$text_17))

#Mutate to add column with season year - length of this vector is hardcode due to lack of imagination
text_17 <- text_17 %>% mutate(year = rep(17, 680))

#Column created to hold the individual game that each comment was made for
text_17$game <- c(rep(1,length(on_post_17_1)), rep(2,length(on_post_17_2)), rep(3,length(on_post_17_3)), rep(4,length(on_post_17_4)), rep(5,length(on_post_17_5)), 
                  rep(6,length(on_post_17_6)), rep(7,length(on_post_17_7)), rep(8,length(on_post_17_8)), rep(9,length(on_post_17_9)), rep(10,length(on_post_17_10)))


### REPEAT FOR 2016 SEASON START

#Exact same code as above, just for comments on blog posts from the 2016 season start
on_post_16_10 <- read_html('https://oilersnation.com/2016/11/02/gdb-10-0-wrap-up/comment-page-1/#comments')
  on_post_16_10 <- on_post_16_10%>%html_nodes(".comment-body")%>%html_text()

on_post_16_9 <- read_html('https://oilersnation.com/2016/10/31/gdb-9-0-wrap-up-put-your-clothes-on-the-streak-is-over/comment-page-1/#comments')
  on_post_16_9 <- on_post_16_9%>%html_nodes(".comment-body")%>%html_text()

on_post_16_8 <- read_html('https://oilersnation.com/2016/10/29/gdb-8-0-wrap-up/comment-page-1/#comments')
  on_post_16_8 <- on_post_16_8%>%html_nodes(".comment-body")%>%html_text()

on_post_16_7 <- read_html('https://oilersnation.com/2016/10/27/gdb-7-0-wrap-up-capital-punishment/comment-page-1/#comments')
  on_post_16_7 <- on_post_16_7%>%html_nodes(".comment-body")%>%html_text()

on_post_16_6 <- read_html('https://oilersnation.com/2016/10/23/gdb-6-0-wrap-up-party-in-the-peg/comment-page-1/#comments')
  on_post_16_6 <- on_post_16_6%>%html_nodes(".comment-body")%>%html_text()

on_post_16_5 <- read_html('https://oilersnation.com/2016/10/21/gdb-5-0-wrap-up-welcome-home-yak/comment-page-1/#comments')
  on_post_16_5 <- on_post_16_5%>%html_nodes(".comment-body")%>%html_text()

on_post_16_4 <- read_html('https://oilersnation.com/2016/10/19/gdb-4-0-wrap-up/comment-page-1/#comments')
  on_post_16_4 <- on_post_16_4%>%html_nodes(".comment-body")%>%html_text()

on_post_16_3 <- read_html('https://oilersnation.com/2016/10/17/gdb-3-0-wrap-up-all-kinds-of-terrible/comment-page-1/#comments')
  on_post_16_3 <- on_post_16_3%>%html_nodes(".comment-body")%>%html_text()

on_post_16_2 <- read_html('https://oilersnation.com/2016/10/15/gdb-2-0-wrap-up/comment-page-1/#comments')
  on_post_16_2 <- on_post_16_2%>%html_nodes(".comment-body")%>%html_text()

on_post_16_1 <- read_html('https://oilersnation.com/2016/10/13/gdb-1-0-wrap-up-pulling-the-plastic-off/comment-page-1/#comments')
  on_post_16_1 <- on_post_16_1%>%html_nodes(".comment-body")%>%html_text()

on_16 <- c(on_post_16_1, on_post_16_2, on_post_16_3, on_post_16_4, on_post_16_5, on_post_16_6, on_post_16_7, on_post_16_8, on_post_16_9,on_post_16_10)

text_16 <- gsub("[\t\n]", "", on_16)

text_16 <- gsub("Reply", "", text_16)

text_16 <- gsub(".*CHEERS[0-9]+", "", text_16)

text_16 <- gsub("[0-9]+Trashes [0-9]+CHEERS ", "", text_16)

text_16 <- data.frame(text_16)

text_16$text_16 <- as.character((text_16$text_16))

text_16 <- text_16 %>% mutate(year = rep(16, 581))

text_16$game <- c(rep(1,length(on_post_16_1)), rep(2,length(on_post_16_2)), rep(3,length(on_post_16_3)), rep(4,length(on_post_16_4)), rep(5,length(on_post_16_5)), 
                  rep(6,length(on_post_16_6)), rep(7,length(on_post_16_7)), rep(8,length(on_post_16_8)), rep(9,length(on_post_16_9)), rep(10,length(on_post_16_10)))


#Set the same colnames for both data frames so they can be combined using rbind
colnames(text_16) <- c("text", "year", "game")
colnames(text_17) <- c("text", "year", "game")

#combine data frames
text_data <- rbind(text_16, text_17)


### START TEXT ANALYSIS ###


#Sets up data frame using tidytext
text_data <- text_data %>%
  unnest_tokens(word, text)

#Remove stop words
data(stop_words)

text_data <- text_data %>%
  anti_join(stop_words)

#Get the overall most used words across games and seasons
top_words <- text_data %>%
  count(word, sort = TRUE) 


#Looks at top words used by season
top_words_year <- text_data %>% group_by(year) %>%
  count(word, sort = TRUE) %>% spread(year, n, fill = 0)

#2016
top_16 <- top_words_year %>% 
  arrange(desc(`16`))

#2017
top_17 <- top_words_year %>% 
  arrange(desc(`17`))

### Matches most used words w. rosters to get most talked about players across years

#First need to get rosters from each season
#Uses rvest to scrape rosters from Eliteprospects.com
team_16 <- read_html("http://www.eliteprospects.com/team.php?year0=2017&status=stats&team=61")
team_16 <- data.frame(team_16%>%html_nodes(".tableborder2 .input a , .tableborder2 .tdbackground:nth-child(2)")%>%html_text())

#Remove top 2 rows, which are not relevant
team_16 <- tail(team_16, -2)

#Name column to something usable
colnames(team_16) <- 'player'

#Convert player names to character and change to lower case
team_16 <- tolower(as.character(team_16$player))

#Split player names by first and surnames
team_16 <- unlist(strsplit(team_16, " "))

#Matches list of most used words with roster to get rank of most talked about players in 2016
top_player_16 <- top_16[top_16$word %in% team_16, ]


### Same thing for 2017 season
team_17 <- read_html("http://www.eliteprospects.com/team.php?year0=2018&status=stats&team=61")
team_17 <- data.frame(team_17%>%html_nodes(".tableborder2 .input a , .tableborder2 .tdbackground:nth-child(2)")%>%html_text())
team_17 <- tail(team_17, -2)
colnames(team_17) <- 'player'
team_17 <- tolower(as.character(team_17$player))

team_17 <- unlist(strsplit(team_17, " "))

#Matches list of most used words with roster to get rank of most talked about players in 2017
top_player_17 <- top_17[top_17$word %in% team_17, ]


#Plots names of most mentioned players in 2017
top_17_plot <- ggplot(top_player_17[1:5,], aes(x=reorder(word, -`17`), y = `17`)) + geom_bar(stat = "identity", fill = '#FC4C02') +
                labs(x="Player", y='# Mentions 2017') +
                scale_y_continuous(expand = c(0,0)) +
                theme_bw() +
                theme(
                  plot.margin = margin(0.5,0.5,1,1, "cm"),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.line =  element_line(color="black", size = 1),
                  axis.text = element_text(size = 22),
                  axis.title.y = element_text(size = 28, margin = margin(0,20,0,0)),
                  axis.title.x = element_text(size = 28, margin = margin(20,0,0,0)),
                  legend.position = "none"
                )


#Plots names of most mentioned players in 2016
top_16_plot <- ggplot(top_player_16[1:5,], aes(x=reorder(word, -`16`), y = `16`)) + geom_bar(stat = "identity", fill = '#1E22AA') +
                  labs(x="Player", y='# Mentions 2016') +
                  scale_y_continuous(expand = c(0,0)) +
                  theme_bw() +
                  theme(
                    plot.margin = margin(0.5,0.5,1,1, "cm"),
                    panel.background = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank(),
                    axis.line =  element_line(color="black", size = 1),
                    axis.text = element_text(size = 22),
                    axis.title.y = element_text(size = 28, margin = margin(0,20,0,0)),
                    axis.title.x = element_text(size = 28, margin = margin(20,0,0,0)),
                    legend.position = "none"
                  )

#Save plots
ggsave('top_17_plot.png', plot = top_17_plot, width = 30, height = 20, units = "cm")
ggsave('top_16_plot.png', plot = top_16_plot, width = 30, height = 20, units = "cm")



### WORD CORRELATION BETWEEN SEASONS


frequency <- text_data %>% 
  count(year, word) %>%
  group_by(year) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(year, proportion) %>% na.omit()

corr_plot <- ggplot(frequency, aes(x = `16`, y = `17`, color = abs(`16`-`17`))) + geom_jitter(alpha = 0.5, size = 4) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 6) +
  geom_abline(color = "gray40", lty = 2) +
  labs(x = "2016 Season", y = "2017 Season") +
  theme_bw() +
  theme(
    plot.margin = margin(0.5,0.5,1,1, "cm"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line =  element_line(color="black", size = 1),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 28, margin = margin(0,20,0,0)),
    axis.title.x = element_text(size = 28, margin = margin(20,0,0,0)),
    legend.position = "none"
  )

#Word correlation minus the 3 most frequently used words
frequency_minus <- frequency %>% filter(!(word == "team" | word == "game" | word == "oilers"))

corr_plot_minus <- ggplot(frequency_minus, aes(x = `16`, y = `17`, color = abs(`16`-`17`))) + geom_jitter(alpha = 0.5, size = 6) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 8) +
  geom_abline(color = "gray40", lty = 2) +
  labs(x = "2016 Season", y = "2017 Season") +
  theme_bw() +
  theme(
    plot.margin = margin(0.5,0.5,1,1, "cm"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line =  element_line(color="black", size = 1),
    axis.text = element_text(size = 18),
    axis.title.y = element_text(size = 28, margin = margin(0,20,0,0)),
    axis.title.x = element_text(size = 28, margin = margin(20,0,0,0)),
    legend.position = "none"
  )

ggsave('corr_plot.png', plot = corr_plot, width = 35, height = 20, units = "cm")
ggsave('corr_plot_minus.png', plot = corr_plot_minus, width = 35, height = 20, units = "cm")



### SENTIMENT ANALYSIS ###



#Ratio of positive and negative words for seasons 16/17 and 17/18

sentiment_year <- text_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

sentiment_year$year <- as.factor(sentiment_year$year)

#Sentiment plot
sent_plot <- ggplot(sentiment_year, aes(x=year, y=sentiment)) + geom_bar(stat = "identity", fill = c('#1E22AA', '#FC4C02')) +
              geom_hline(yintercept = 0, size = 1) +
              coord_flip() +
              labs(x="Season", y="Pos-Neg sentiment ratio") +
              scale_x_discrete(labels = c("2016", "2017")) +
              theme_bw() +
              theme(
                plot.margin = margin(0.5,0.5,1,1, "cm"),
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.ticks.y = element_blank(),
                axis.line.x =  element_line(color="black", size = 1),
                axis.text = element_text(size = 22),
                axis.title.y = element_text(size = 28, margin = margin(0,20,0,0)),
                axis.title.x = element_text(size = 28, margin = margin(20,0,0,0))
              )

ggsave('sent_plot.png', plot = sent_plot, width = 35, height = 20, units = "cm")
  
#Ratio of positive to negative words between seasons and across games
positive_sentiment <- text_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(game, year, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

  #Convert variables to factors for plotting
  positive_sentiment$year <- as.factor(positive_sentiment$year)
  positive_sentiment$game <- as.factor(positive_sentiment$game)

#Positive-negative sentiment across games plot
pos_neg_plot <- ggplot(positive_sentiment, aes(x=game, y=sentiment, group=year, fill = year)) + geom_bar(position = "dodge", stat = "identity") +
                  geom_hline(yintercept = 0, size = 1) +
                  labs(x = 'Game', y = 'Sentiment', fill = 'Year') +
                  scale_fill_manual(values = c('#1E22AA', '#FC4C02')) +
                  theme_bw() +
                  theme(
                    panel.background = element_blank(),
                    panel.border = element_blank(),
                    panel.grid = element_blank(),
                    axis.line =  element_line(color="black", size = 1),
                    axis.text = element_text(size = 18),
                    axis.title = element_text(size = 24),
                    legend.title=element_text(size=18, margin=margin(0,0,15,0)),
                    legend.text=element_text(size=14), 
                    legend.key.size = unit(1, "cm")
                  )

ggsave('pos_neg_plot.png', plot = pos_neg_plot, width = 30, height = 20, units = "cm")



### PLOTS NUMBER OF ANGER WORDS


#Use of emotional words between seasons and across games
emotions <- text_data %>%
  inner_join(get_sentiments("nrc")) %>%
  count(game, year, sentiment) %>% filter(sentiment == "anger")

  #Convert variables to factors for plotting
  emotions$year <- as.factor(emotions$year)
  emotions$game <- as.factor(emotions$game)

#Anger words plot
anger_plot <- ggplot(emotions, aes(x=game, y=n, group=year, fill = year)) + geom_bar(position = "dodge", stat = "identity") + 
                scale_y_continuous(expand = c(0,0)) +
                labs(x = 'Game', y = '# anger words', fill = 'Year') +
                scale_fill_manual(values = c('#1E22AA', '#FC4C02')) +
                theme_bw() +
                theme(
                  plot.margin = margin(1, 0.5, 0.5, 1, "cm"),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  panel.grid = element_blank(),
                  axis.line =  element_line(color="black", size = 1),
                  axis.text = element_text(size = 18),
                  axis.title.x = element_text(size = 24),
                  axis.title.y = element_text(size = 24, margin=margin(0,20,0,0)),
                  legend.title=element_text(size=18, margin=margin(0,0,15,0)),
                  legend.text=element_text(size=14), 
                  legend.key.size = unit(1, "cm")
                )

ggsave('anger_plot.png', plot = anger_plot, width = 30, height = 20, units = "cm")



### PLOTS NUMBER OF JOY WORDS


joy <- text_data %>%
  inner_join(get_sentiments("nrc")) %>%
  count(game, year, sentiment) %>% filter(sentiment == "joy")

  #Convert variables to factors for plotting
  joy$year <- as.factor(joy$year)
  joy$game <- as.factor(joy$game)

#Joy words plot
joy_plot <- ggplot(joy, aes(x=game, y=n, group=year, fill = year)) + geom_bar(position = "dodge", stat = "identity") + 
              scale_y_continuous(expand = c(0,0)) +
              labs(x = 'Game', y = '# joy words', fill = 'Year') +
              scale_fill_manual(values = c('#1E22AA', '#FC4C02')) +
              theme_bw() +
              theme(
                plot.margin = margin(1, 0.5, 0.5, 1, "cm"),
                panel.background = element_blank(),
                panel.border = element_blank(),
                panel.grid = element_blank(),
                axis.line =  element_line(color="black", size = 1),
                axis.text = element_text(size = 18),
                axis.title.x = element_text(size = 24),
                axis.title.y = element_text(size = 24, margin=margin(0,20,0,0)),
                legend.title=element_text(size=18, margin=margin(0,0,15,0)),
                legend.text=element_text(size=14), 
                legend.key.size = unit(1, "cm")
              )

ggsave('joy_plot.png', plot = joy_plot, width = 30, height = 20, units = "cm")

# Script to produce an animated visualization of tweets vs followers and change over time

library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)
library(gganimate)

# Clone 538 GH repo to this location and read in the files
iraTweets <- map_dfr(list.files('~/git-repos/fivethirtyeight/russian-troll-tweets/', full.names = TRUE, pattern = '*.csv'), function(f) {
  read_csv(f, col_types=cols(.default = col_character()))
}) %>% mutate(TweetID=row_number(), publish_date=as_date(mdy_hm(publish_date)), followers=as.integer(followers)) %>%
  filter(publish_date >= '2014-10-01')

accounts <- iraTweets %>% group_by(author) %>% mutate(followers=as.integer(followers)) %>% summarize(tweets=n(), followers=max(followers)) %>%
  filter(followers > 0) %>%
  inner_join(iraTweets %>% select(author, account_category) %>% distinct(), by='author') %>%
  mutate(account_category_high=case_when(
    !(account_category %in% c('RightTroll', 'LeftTroll', 'NewsFeed', 'Fearmonger')) ~ 'Commercial/HashtagGamer/Unknown',
    TRUE ~ account_category
  ))

# shared characteristics between static and animated plot
createBasePlot <- function(accountsDf) {
  accountsDf %>% filter(account_category != 'NonEnglish') %>%
    ggplot() +
    geom_point(aes(x=followers, y=tweets, color=account_category_high)) +
    scale_x_continuous(trans='log', breaks=c(1, 10, 150, 3000, 60000), labels=comma) +
    scale_y_continuous(trans='log', breaks=c(1, 10, 150, 3000, 60000), labels=comma) +
    scale_color_brewer(type = 'qual', palette = 'Dark2') +
    theme_economist_white() +
    theme(panel.grid.minor = element_blank(), legend.text = element_text(size=12)) +
    labs(x='Maximum Cumulative Followers of the Account (Log Scale)', y='Total Tweets by the Account (Log Scale)',
         title='Volume of Tweets and Number of Followers of Russian IRA Twitter Accounts',
         subtitle='Tweets published on or after October 1, 2014',
         caption='Source: FiveThirtyEight/Clemson University Dataset of Russian IRA Tweets\nNote: Excludes "NonEnglish" category accounts and accounts with no followers',
         color='Account Category')
}

# static plot (no fun!)
createBasePlot(accounts) +
  geom_vline(xintercept=60, color='blue', alpha=.5, linetype=2) +
  geom_vline(xintercept=1000, color='blue', alpha=.5, linetype=2) +
  geom_text(aes(x=65, y=70000), label='60 followers', color='blue', alpha=.5, hjust='left', size=3) +
  geom_text(aes(x=1025, y=70000), label='1000 followers', color='blue', alpha=.5, hjust='left', size=3)

# need to create a grid of dates so the animation is smooth and continuous
authors <- unique(iraTweets$author)
dates <- seq(from=min(iraTweets$publish_date)-1, to=max(iraTweets$publish_date), by='days')
accountDaysGrid <- tibble(
  author=rep(authors, each=length(dates)),
  publish_date=rep(dates, length(authors))
)

accountDays <- iraTweets %>% group_by(author, publish_date) %>%
  summarize(tweets=n(), followers=max(followers)) %>%
  group_by(author) %>%
  mutate(idx=row_number(), priorIdx=idx-1) %>% ungroup()

accountDays <- accountDays %>%
  left_join(accountDays %>% select(nextIdx=idx, -priorIdx, priorFollowers=followers, author), by=c('author', 'priorIdx'='nextIdx')) %>%
  mutate(followerDelta=case_when(idx==1 ~ followers, TRUE ~ followers-priorFollowers)) %>%
  select(author, publish_date, followerDelta, tweets) %>%
  right_join(accountDaysGrid, by=c('author', 'publish_date')) %>%
  mutate(followers=case_when(is.na(followerDelta) ~ 0, TRUE ~ followerDelta), tweets=case_when(is.na(tweets) ~ 0L, TRUE ~ tweets)) %>%
  arrange(author, publish_date) %>%
  group_by(author) %>%
  mutate_at(vars(tweets, followers), cumsum) %>%
  filter(followers > 0) %>%
  inner_join(iraTweets %>% select(author, account_category) %>% distinct(), by='author') %>%
  mutate(account_category_high=case_when(
    !(account_category %in% c('RightTroll', 'LeftTroll', 'NewsFeed', 'Fearmonger')) ~ 'Commercial/HashtagGamer/Unknown',
    TRUE ~ account_category
  )) %>% select(-followerDelta)

# animated plot!
animate(createBasePlot(accountDays) + transition_time(publish_date) +
          labs(subtitle='Cumulative tweets published vs. followers as of {frame_time}', x='Followers of the Account (Log Scale)'),
        nframes = 120, length = 20, height=700, width=800)

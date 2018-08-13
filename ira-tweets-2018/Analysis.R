# Script to produce analysis and visualizations for blog post at http://www.cascadia-analytics.com/2018/08/12/ira-tweets1.html

library(tidyverse)
library(tidytext)
library(topicmodels)
library(text2vec)
library(Rtsne)
library(lubridate)
library(ggthemes)
library(scales)
library(tidyquant)

# Clone 538 GH repo to this location and read in the files
iraTweets <- map_dfr(list.files('~/git-repos/fivethirtyeight/russian-troll-tweets/', full.names = TRUE, pattern = '*.csv'), function(f) {
  read_csv(f, col_types=cols(.default = col_character()))
}) %>% mutate(TweetID=row_number(), publish_date=mdy_hm(publish_date))

# Clean up messy text
iraTweetsClean <- iraTweets %>%
  mutate(content=iconv(content, to='ASCII', sub=' '),
         content=gsub(x=content, perl=TRUE, pattern='http(?:s)?:.+(?: |$)', replacement=' '),
         content=gsub(x=content, pattern='@.+ ', replacement=' '),
         content=gsub(x=content, pattern='[:/\\\\"\\.,?!\\-\\$()*%_=<>|]', replacement=' '),
         content=gsub(x=content, pattern='[0-9]+', replacement=' '),
         content=gsub(x=content, pattern='&amp;', replacement='and'),
         content=gsub(x=content, perl=TRUE, pattern='(?:&gt;)+', replacement=' '),
         content=gsub(x=content, perl=TRUE, pattern='(?:&lt;)+', replacement=' '),
         content=gsub(x=content, pattern='[&;\\-]', replacement=' '),
         content=gsub(x=content, pattern='^[\\[\\] \\-]+$', replacement=' '),
         content=gsub(x=content, pattern='\\s+', replacement=' '),
         content=gsub(x=content, pattern='# ', replacement=' '),
         content=gsub(x=content, pattern="'", replacement=''),
         content=trimws(tolower(content)),
         content=gsub(x=content, pattern=' [a-hj-z] ', replacement=' '),
         content=gsub(x=content, pattern='demndebate', replacement='demdebate'),
         content=gsub(x=content, pattern='new york', replacement='new_york'),
         content=case_when(content %in% c('',"'") ~ NA_character_, TRUE ~ content)
  ) %>% filter(!is.na(content))

# Subset/sample as desired
iraTweetsCleanSample <- iraTweetsClean %>% filter(!(account_category %in% c('Unknown', 'Commercial', 'NonEnglish'))) # %>% sample_frac(.35)

# chart consistency
THEME = theme_economist()
CAPTION = 'Source: FiveThirtyEight/Clemson University Dataset of Russian IRA Tweets'

# Overview vizzes

ggplot(iraTweets %>% mutate(publish_date=as.Date(publish_date)) %>% filter(publish_date > '2014-10-01') %>% group_by(publish_date) %>%summarize(count=n())) +
  geom_line(mapping=aes(x=publish_date, y=count)) +
  geom_ma(mapping=aes(x=publish_date, y=count), n=30, color='red', linetype='solid', size=1.1) +
  scale_x_date(date_breaks='6 months', date_labels='%b-%Y') + scale_y_continuous(labels=comma) +
  THEME + labs(title='Tweets Per Day, October 2014 - June 2018', x=NULL, y=NULL,
                           subtitle='30-day moving average shown in red',
                           caption=CAPTION)

ggplot(iraTweetsClean %>% group_by(account_category) %>% summarize(count=n())) + geom_bar(aes(x=reorder(account_category, count), y=count), stat='identity') + coord_flip() +
  scale_y_continuous(labels=comma) +
  THEME + labs(title='Count of Tweets By Category', x=NULL, y=NULL,
                           caption=CAPTION)

#rm(iraTweets)
#rm(iraTweetsClean)

# Select specific subset to analyze, and specify sample as desired
iraTweetsCleanSmallSample <- iraTweetsCleanSample %>% filter(account_category=='RightTroll') %>% sample_frac(.35)

stopWords <- bind_rows(stop_words %>% select(word), tibble(word=c('im', 'dont', 'rt', 'ft', 'gt')))

# create text2vec iterator and vocabulary
tweetIter <- itoken(iraTweetsCleanSmallSample$content)
vocab <- create_vocabulary(tweetIter, stopwords = stopWords$word)
# find co-locating terms, join only two, and they must occur at least 50 times to be valid co-locations
coloc <- Collocations$new(collocation_count_min = 50, vocabulary=vocab)
coloc$fit(tweetIter, n_iter = 2)
tweetIter <- coloc$transform(tweetIter)
# get rid of terms that don't occur frequently
vocab <- create_vocabulary(tweetIter) %>% prune_vocabulary(term_count_min = 5)

# create document-term matrix to feed into LDA
dtm <- create_dtm(tweetIter, vocab_vectorizer(vocab))

# LDA fit
N_TOPICS = 20
lda <- LDA$new(n_topics = N_TOPICS)
doc_topic <- lda$fit_transform(dtm)

# join LDA results onto Tweet sample for further analysis by Tweet features
doc_topic_df <- bind_cols(iraTweetsCleanSmallSample %>% select(-account_category, -content), as.data.frame(doc_topic))

doc_topic_df <- doc_topic_df %>%
  # select only those tweets that are confidently assigned a topic
  gather(key='topic', value='prob', starts_with('V')) %>% filter(prob >= .4) %>%
  group_by(TweetID) %>%
  filter(prob==max(prob)) %>%
  # in determining likeliest topic where there's a tie, just take the first one
  ungroup() %>% group_by(TweetID) %>% filter(row_number()==1) %>%
  select(TweetID, MaxTopic=topic, MaxProb=prob) %>%
  inner_join(doc_topic_df %>% mutate(idx=row_number()), by='TweetID')

doc_topic_filtered <- doc_topic[doc_topic_df %>% .$idx, ]

# Run t-sne dimensionality reduction
doc_topic_filtered_tsne <- Rtsne(doc_topic_filtered, check_duplicates = FALSE, verbose = TRUE, perplexity=30)

# join two reduced features onto tweet sample for further analysis/visualization
doc_topic_df <- doc_topic_df %>%
  bind_cols(as_data_frame(doc_topic_filtered_tsne$Y) %>% rename(T1=V1, T2=V2)) %>%
  mutate(Topic=str_pad(gsub(x=MaxTopic, pattern='V([0-9]+)', replacement='\\1'), 2, 'left', '0'))

# t-SNE plot
doc_topic_df %>% ggplot() + geom_point(aes(x=T1, y=T2, color=Topic)) + theme_void()

# Most "influential" words per topic
topWords <- lda$get_top_words(n=12, topic_number = 1:N_TOPICS)
topWordsHtmlTableText <- array_branch(topWords, 2) %>%
  imap_chr(function(words, idx) { paste0('<tr><td>', idx, '</td><td>Label</td><td>', paste0(words, collapse=', '), '</td></tr>')}) %>%
  writeLines()

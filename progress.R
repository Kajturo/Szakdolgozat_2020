library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyr)

#tweets elsődleges tulajdonságok----
glimpse(tweets)

ptweets <- tweets %>%
  select(user_id, created_at, screen_name, text, is_retweet, favorite_count, retweet_count) %>%
  arrange(desc(created_at))

head(ptweets)





#ptweets sima eloszlások időben----
ptweets %>%
filter(screen_name=="SAPNews" | screen_name=="Google") %>%
ggplot(aes(x = created_at, fill = screen_name)) +
  theme_light()+
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name)+
  labs(y = "Tweetek száma",
       x = "Tweetek létrehozásának dátuma")





#tokenizálás és stop szavak szűrése ID nélkül----
regex <- "&amp;|&lt;|&gt;"
tidy_ptweets <- ptweets %>%
  filter(is_retweet==FALSE) %>%
  select(created_at, screen_name, text) %>%
  mutate(text = str_remove_all(text, regex)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))





#szógyakoriságok----
frequency <- tidy_ptweets %>% 
  group_by(screen_name) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_ptweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#szógyakoriságok bar charton
frequency %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(word, freq), y=freq, fill=screen_name))+
  theme_light()+
  geom_col(show.legend = FALSE)+
  facet_wrap(~screen_name, scales = "free")+
  labs(x=NULL, y="gyakoriság")+
  coord_flip()

#szétdarabolás oszlopkra screen_name szerint
frequency_jitter <- frequency %>% 
  select(screen_name, word, freq) %>% 
  spread(screen_name, freq) %>% 
  arrange(Accenture, AccentureAI, AccentureTech, AccentureDigi,
          SAP, SAPNews, SAPhightech, SAPCommunity, Google, Microsoft,
          IBM, IBMResearch, IBMAnalytics, Uptake,
          generalelectric, PTC, ROKAutomation, Siemens, siemensindustry, Tesla,
          intel, nvidia, Cognex_Corp,
          KPMG, PwC)

#szógyakoriságok összehasonlítása pontokkal (moduláris)
library(scales)

ggplot(frequency_jitter, aes(AccentureTech, SAPhightech)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")





#PTC-SIE szóhasználat összehasonlítás 2019----
PTC_SIE <- tidy_ptweets %>%
  filter(screen_name=="PTC" | screen_name=="Siemens") %>%
  filter(created_at >= as.POSIXct("2019-01-01 00:00:00"),
         created_at < as.POSIXct("2020-01-01 00:00:00"))

#adattisztítás és lográta készítés
wr_PTC_SIE <- PTC_SIE %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(PTC / Siemens)) %>%
  arrange(desc(logratio))

#egyenlően használt szavak
wr_PTC_SIE %>% 
  arrange(abs(logratio))

#top15 jellegzetes szó
wr_PTC_SIE %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("esélyhányados") +
  scale_fill_discrete(name = "", labels = c("PTC", "Siemens"))





#NVI-COG szóhasználat összehasonlítás 2019----
NVI_COG <- tidy_ptweets %>%
  filter(screen_name=="nvidia" | screen_name=="Cognex_Corp") %>%
  filter(created_at >= as.POSIXct("2019-01-01 00:00:00"),
         created_at < as.POSIXct("2020-01-01 00:00:00"))

wr_NVI_COG <- NVI_COG %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(nvidia / Cognex_Corp)) %>%
  arrange(desc(logratio))

wr_NVI_COG %>% 
  arrange(abs(logratio))

wr_NVI_COG %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("esélyhányados") +
  scale_fill_discrete(name = "", labels = c("nvidia", "Cognex_Corp"))





#IBMA-ACCAI szóhasználat összehasonlítás 2019----
IBMA_ACCAI <- tidy_ptweets %>%
  filter(screen_name=="IBMAnalytics" | screen_name=="AccentureAI") %>%
  filter(created_at >= as.POSIXct("2019-01-01 00:00:00"),
         created_at < as.POSIXct("2020-01-01 00:00:00"))

wr_IBMA_ACCAI <- IBMA_ACCAI %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(IBMAnalytics / AccentureAI)) %>%
  arrange(desc(logratio))

wr_IBMA_ACCAI %>% 
  arrange(abs(logratio))

wr_IBMA_ACCAI %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("esélyhányados") +
  scale_fill_discrete(name = "", labels = c("IBMAnalytics", "AccentureAI"))





#szóhasználat változása 2018-2019----
library(lubridate)
library(purrr)

#szavak előfordulása havonta, s_n szerint csoportosítva (25-nél több)
words_by_time <- tidy_ptweets %>%
  filter(created_at >= as.POSIXct("2019-01-01 00:00:00"),
         created_at < as.POSIXct("2020-01-01 00:00:00")) %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created_at, unit = "1 month")) %>%
  count(time_floor, screen_name, word) %>%
  group_by(screen_name, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 25)

#dataframe-ben dataframe szavak és s_n szerint
nested_data <- words_by_time %>%
  nest(-word, -screen_name)

#modelkapcsolás glm
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total)
                                  ~ time_floor, .,
                                  family = "binomial")))

#időbeli változások kiszedése, és összehasonlitás
library(broom)

slopes <- nested_models %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

#leginkább megváltozott gyakoriságú szavak (jelenleg nem szükséges)
top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.05)

#vonaldiagram (moduláris)
library(viridis)

words_by_time %>%
  inner_join(slopes, by = c("word", "screen_name")) %>%
  filter(str_detect(word, "#iot|^data$|#ai$")) %>%
  ggplot(aes(time_floor, count/time_total, color = screen_name)) +
  geom_line(size = 1.3) +
  geom_smooth(method = "lm", se = FALSE, color="darkgreen")+
  facet_wrap(~word)+
  labs(x = NULL, y = "szógyakoriság")+
  scale_color_viridis(discrete = TRUE, option = "C")+
  theme(legend.title = element_blank())





#retweets----
tidy_withid <- tweets %>%
  filter(is_retweet==FALSE) %>%
  select(status_id, created_at, retweet_count, favorite_count, screen_name, text) %>%
  filter(!str_detect(text, "^(RT|@)")) %>%
  mutate(text = str_remove_all(text, regex)) %>%
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"))

#összes retweet s_n szerint
totals <- tidy_withid %>% 
  group_by(screen_name, status_id) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name) %>% 
  summarise(total_rts = sum(rts)) %>% 
  arrange(desc(total_rts))

#szavak retweet mediánja és használatának száma
words_by_rts <- tidy_withid %>% 
  group_by(status_id, word, screen_name) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(screen_name, word) %>% 
  summarise(retweets = median(rts), uses = n()) %>%
  left_join(totals) %>%
  filter(retweets != 0) %>%
  ungroup()

words_by_rts %>% 
  filter(uses >= 50) %>%
  arrange(desc(retweets))

#oszlop diagram retweetekről (moduláris)
words_by_rts %>%
  filter(uses >= 100) %>%
  filter(grepl('AccentureTech|SAPNews|Microsoft|siemensindustry|IBMResearch|nvidia', screen_name)) %>%
  group_by(screen_name) %>%
  top_n(10, retweets) %>%
  arrange(retweets) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweets, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = NULL)





#favourites (mirror)----
totals_f <- tidy_withid %>% 
  group_by(screen_name, status_id) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(screen_name) %>% 
  summarise(total_favs = sum(favs)) %>%
  arrange(desc(total_favs))

words_by_favs <- tidy_withid %>% 
  group_by(status_id, word, screen_name) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(screen_name, word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals_f) %>%
  filter(favorites != 0) %>%
  ungroup()

words_by_favs %>%
  filter(uses >= 100) %>%
  filter(grepl('AccentureTech|SAPNews|Microsoft|siemensindustry|IBMResearch|nvidia', screen_name)) %>%
  group_by(screen_name) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = NULL)





#tf-idf----
#ez a frequencyben is benne van
words_by_screen_name <- tidy_ptweets %>%
  count(screen_name, word, sort = TRUE) %>%
  ungroup()

#tf_idf-ek kapcsolása
tf_idf <- words_by_screen_name %>%
  bind_tf_idf(word, screen_name, n) %>%
  arrange(desc(tf_idf))

#legfontosabb szavak bar charton (moduláris)
tf_idf %>%
  filter(str_detect(screen_name, "PTC|AccentureAI|Cognex_Corp|IBMAnalytics")) %>%
  group_by(screen_name) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ screen_name, scales = "free") +
  ylab("tf-idf") +
  xlab(NULL) +
  coord_flip()

#screen_namek közötti korrelációk
library(widyr)

screen_name_cors <- words_by_screen_name %>%
  pairwise_cor(screen_name, word, n, sort = TRUE)

#korrelációs hálózat
library(ggraph)
library(igraph)
set.seed(1010)

screen_name_cors %>%
  filter(correlation > 0.5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE, color="red") +
  theme_void()





#topicmodelling----
#screen_name+status_id = dokumentum volt az eredeti verzió
words_for_topicmodelling <- tidy_withid %>%
  select(-status_id,-created_at,-retweet_count,-favorite_count) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  unite(document, screen_name) %>%
  count(document, word)

# document-term-matrix konvertálás
all_dtm <- words_for_topicmodelling %>%
  cast_dtm(document, word, n)

#LDA
library(topicmodels)
all_lda <- LDA(all_dtm, k = 5, control = list(seed = 1337))

all_beta <- tidy(all_lda, matrix = "beta")

#legkülönbözőbb szavak
beta_spread <- all_beta %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

#top kifejezések kategóriánként
all_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()

#cégek szavai melyik kategóriákba estek
all_gamma <- tidy(all_lda, matrix = "gamma")

all_gamma %>%
  separate(document, c("screen_name"), sep = "_") %>%
  mutate(screen_name = reorder(screen_name, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma, color=gamma)) +
  geom_boxplot(show.legend = FALSE) +
  scale_color_gradient(low="black", high="red") +
  facet_wrap(~ screen_name) +
  labs(x="téma")





#kifejezések együttes előfordulása----
library(widyr)

word_pairs <- tidy_withid %>%
  pairwise_count(word, status_id, sort = TRUE, upper = FALSE)

library(igraph)
library(ggraph)

set.seed(1010)
word_pairs %>%
  filter(!str_detect(item1, "sap")) %>%
  filter(!str_detect(item2, "sap")) %>%
  filter(n >= 150) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "#CC33CC") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

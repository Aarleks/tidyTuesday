# What about a soundwave that changes colour depending on who is speaking with y-axis for how many words or proportion of screen time?

# Another option could be to look at IMDB ratings by main character per chapter
library(tidyverse)
library(tvthemes)
library(extrafont)
library(ggtext)

# Load extra fonts for aesthetic joy
loadfonts()
import_avatar() ## "Slayer" font

tuesdata  <- tidytuesdayR::tt_load(2020, week = 33)

words <- tuesdata$avatar %>%
  filter(character_words != "NA") %>%
    select(book, book_num, chapter, chapter_num, character, character_words, imdb_rating)

# Choose the characters to show and get the number of lines for each by 
    # book-chapter
characters <- c("Aang","Katara", "Sokka", "Zuko", "Iroh", "Toph")

thing <- words %>% 
  mutate(chap = case_when(book == "Water" ~ chapter_num,
                          book == "Earth" ~ chapter_num + 20,
                          book == "Fire" ~ chapter_num + 41)) %>%
  filter(character %in% characters) %>%
  group_by(book, chap, character, imdb_rating) %>%
  summarise(num_lines = n()) %>%
  mutate(neg_lines = num_lines - (2 * num_lines)) %>%
  ungroup()

# Make a column graph that looks like a soundwave
thing %>%
  ggplot(aes(chap, fill = num_lines)) +
  geom_col(aes(y = num_lines), width = 0.6, position = position_dodge(0.1)) +
  theme_avatar(text.font = "Slayer",
               title.font = "Slayer") +
  scale_fill_avatar(type = "continuous") +
  facet_grid(rows = vars(character)) +
  geom_text(aes(-2.5, 15, label = character), family = "Slayer", size = 4) +
  theme(plot.title = element_markdown(size = 20,
                                      hjust = 0.06),
        plot.subtitle = element_markdown(hjust = 0.1),
        plot.caption = element_text(margin = margin(t = 10)),
        #panel.spacing.y = unit(1, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 10),
        #axis.title.y = element_text(size = 11,
        #                            margin = margin(r = 10)),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 11,
                                    margin = margin(t = 10))) +
  labs(x = "Chapters", y = "Lines", 
       title = " <br>Lines of dialogue<br><b>  Avatar: The Last Airbender</b>",
       subtitle = "Aang, Katara, and Sokka have fewer lines per chapter as the series progresses",
       caption = "#TidyTuesday | Alex Norman | Source: (appa) https://github.com/averyrobbins1/appa") +
  guides(fill = FALSE)

ggsave(here::here("2020-33", "plots", "temp",
                  paste0("last-airbender_", format(Sys.time(), "%Y%m%d_%H%M%S"),
                         ".png")), dpi = 320, width = 9, height = 9)

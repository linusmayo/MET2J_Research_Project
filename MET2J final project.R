library(tidyverse)
library(tidyr)
full_data <-read_csv('MET2J_filtered_instruments.csv')
new_dataset <- read_csv("Genre.csv")

shaped_new_dataset <- new_dataset |>
  gather("Year", "Percentage", -"Primary Genre") |>
  pivot_wider(names_from = "Primary Genre", values_from = "Percentage")

filtered_full_data <- full_data |>
  na.omit() |>
  filter(`Date of Birth`>= 1900, `Date of Birth` <= 1999) |>
  mutate(Decade = case_when(
    startsWith(as.character(`Date of Birth`), "190") ~ "1900s",
    startsWith(as.character(`Date of Birth`), "191") ~ "1910s",
    startsWith(as.character(`Date of Birth`), "192") ~ "1920s",
    startsWith(as.character(`Date of Birth`), "193") ~ "1930s",
    startsWith(as.character(`Date of Birth`), "194") ~ "1940s",
    startsWith(as.character(`Date of Birth`), "195") ~ "1950s",
    startsWith(as.character(`Date of Birth`), "196") ~ "1960s",
    startsWith(as.character(`Date of Birth`), "197") ~ "1970s",
    startsWith(as.character(`Date of Birth`), "198") ~ "1980s",
    startsWith(as.character(`Date of Birth`), "199") ~ "1990s",
  TRUE ~ as.character(`Date of Birth`)  )) |>
  mutate(Instrument = recode(Instrument,"Singing"= "Voice", "Microphone" = "Voice", 
                                                    "Lead vocalist" = "Voice", "Human voice" = "Voice", 
                                                    "Vocal music" = "Voice", "Soprano" = "Voice", 
                                                    "Rapping" = "Voice", "Singer-songwriter"="Voice",
                                                    "Tenor" = "Voice", "Scat singing" = "Voice",
                                                    "Vocal percussion" = "Voice",  
                                                    "Vocoder"="Voice", "Beatboxing"="Voice")) |>
  mutate(Instrument = recode(Instrument,"pianist" = "Piano", "Steinway & Sons" = "Piano", 
                            "Rhodes piano" = "Piano", "Baldwin Piano Company" = "Piano", 
                            "List of Korg products" = "Piano", "Keyboard instrument" = "Piano", 
                            "Fairlight CMI" = "Piano")) |>
  mutate(Instrument = recode(Instrument, "Guitarist" = "Guitar", "PRS guitars" = "Guitar", 
                             "Ibanez" = "Guitar", "Acoustic guitar" = "Guitar", 
                             "ESP Guitars" = "Guitar", "Yamaha electric guitar models" = "Guitar", 
                             "Pedal steel guitar" = "Guitar", "Gibson Les Paul" = "Guitar", 
                             "Takamine guitars" = "Guitar", "Fender Stratocaster" = "Guitar", 
                             "Gibson ES-150" = "Guitar", "Dobro" = "Guitar", "C. F. Martin & Company" = "Guitar",
                             "Avalon Guitars" = "Guitar", "Bentar" = "Guitar", "Fender Jazzmaster" = "Guitar", 
                             "Steel-string acoustic guitar" = "Guitar", "Dean Guitars" = "Guitar", 
                             "Fernandes Guitars" = "Guitar", "Gretsch" = "Guitar", "Fender Telecaster" = "Guitar", 
                             "B.C. Rich, Harmony Company" = "Guitar", "Rhythm guitar" = "Guitar", "Hagström" = "Guitar", 
                             "Rickenbacker" = "Guitar", "Fender Telecaster" = "Guitar", "Twelve-string guitar" = "Guitar", 
                             "Yamaha Corporation" = "Guitar",  "Slide guitar" = "Guitar", "Ovation Guitar Company" = "Guitar", 
                             "First Act" = "Guitar", "Lap steel guitar" = "Guitar", "Electric guitar" = "Guitar", 
                             "Steinberger" = "Guitar", "Gibson ES-335" = "Guitar", "Gibson Ripper" = "Guitar", 
                             "Jackson Guitars" = "Guitar", "Gibson Flying V" = "Guitar", "Fender Jaguar" = "Guitar", 
                             "Classical guitar" = "Guitar")) |>
  mutate(Instrument = recode(Instrument, "Music Man (company)" = "Bass guitar", 
                             "Fender Jazz Bass" = "Bass guitar", "Slapping (music)" = "Bass guitar", 
                             "Fender Precision Bass" = "Bass guitar", "Double bass" = "Bass guitar", 
                             "Fender Mustang Bass" = "Bass guitar", "Wal (bass)" = "Bass guitar",
                             "Fender Roscoe Beck Bass" = "Bass guitar", "Ampeg" = "Bass guitar")) |>
  mutate(Instrument = recode(Instrument, "Electric violin" = "Violin", "Antonio Stradvan" = "Violin")) |>
  mutate(Instrument = recode(Instrument, "Multi-instrumentalist" = "Orchestra")) |>
  mutate(Instrument = recode(Instrument, "Hohner" = "Harmonica")) |>
  mutate(Instrument = recode(Instrument, "Programming (music)" = "Music software",  
                             "Deejay (Jamaican)" = "Music software", "Reason (software)" = "Music software", 
                             "Vox (musical equipment)" = "Music software", "Logic Pro" = "Music software",  
                             "Sampler (musical instrument)" = "Music software", 
                             "DJ mixer" = "Music software", "Turntablism" = "Music software", "FL Studio" = "Music software",  
                             "Skoog" = "Music software", "Sampler (musical instrument)" = "Music software", 
                             "Electronic musical instrument" = "Music software", 
                             "Digital audio workstation" = "Music software", "Programmer" = "Music software", 
                             "Game Boy music" = "Music software")) |>
  mutate(Instrument = recode(Instrument, "Drum" = "Drums" , "Bass drum" = "Drums", 
                               "Drum machine" = "Drums", "Sabian" = "Drums", 
                               "Drum Workshop" = "Drums", "Drum" = "Drums", 
                               "Gretsch Drums" = "Drums", "Percussion instrument" = "Drums", 
                               "Noble & Cooley" = "Drums", "Tom-tom drum" = "Drums", 
                               "Mapex Drums" = "Drums", "Drum kit" = "Drums")) |>
  mutate(Instrument = recode(Instrument, "Types of trombone" = "Trombone", "C.G. Conn" = "Trombone")) |>
  mutate(Genre = recode(Genre, "Hard rock" = "Rock music", "Folk" = "Rock music", 
                        "Pop rock" = "Rock music", "Punk rock" = "Rock music", "Indie rock" = "Rock music",)) |>
  mutate(Genre = recode(Genre, "Modal Jazz" = "Jazz", "Free jazz" = "Jazz", 
                        "Smooth jazz" = "Jazz", "Mainstream jazz"= "Jazz")) |>
  mutate(Genre = recode(Genre, "Electric blues" = "Blues", "Country Blues" = "Blues", 
                        "Chicago Blues" = "Blues")) |>
  mutate(Genre = recode(Genre, "House music" = "Electronic music")) |>
  select(-`Date of Birth`)|>
  write_csv("Cleaned_Instruments.csv")

Filtered_instruments <- filtered_full_data |>
  filter(Instrument == "Piano" | Instrument == "Guitar" | Instrument =="Music software" | 
           Instrument == "Voice" | Instrument == "Drums")

Filtered_genres <- filtered_full_data |>
  filter(Genre == "Jazz" | Genre == "Country music" | Genre == "Rock music"|
         Genre == "Pop music" | Genre == "Electronic music" | Genre == "Blues")

plot_data1 <- Filtered_instruments |>
  group_by(Decade, Instrument) |>
  summarize(count1 = n()) |>
  ungroup()

plot_data2 <- Filtered_genres |>
  group_by(Decade, Genre) |>
  summarize(count2 = n()) |>
  ungroup()

ggplot(data = plot_data1) + 
  aes(x = Decade, y = count1, color = Instrument, group = Instrument) +
  geom_line() +
  labs(title = "Instrument Counts in the 20th Century",
       x = "Decade",
       y = "Instrument Counts") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("Instrument count per decade.pdf")

ggplot(data = plot_data2) +
  aes(x = Decade, y = count2, color = Genre, group = Genre) + 
  geom_line() +
  labs(title = "Genre Count in the 20th Century",
       x = "Decade",
       y = "Genre Count") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Genre count per decade.pdf")






 



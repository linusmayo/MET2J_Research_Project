library(tidyverse)
library(tidyr)
full_data <-read_csv('MET2J_filtered_instruments.csv')
genre_data <- read_csv("Genre.csv")

genres_filter <- genre_data |>
  pivot_longer(cols=-'Primary Genre', names_to="Year", values_to="Percentage") |>
  mutate(Year1 = as.numeric(Year)) |>
  filter(Year1 >= 1960, Year1 <= 2000) |>
  mutate(Decade1 = round(Year1/5)*5) |>
  select(-Year, -Year1) |>
  mutate(Percentage2 = as.numeric(Percentage))
  group_by("Decade1") |>
  summarise(sum_perc = sum(Percentage2))
  

print(genres_filter)

filtered_full_data <- full_data |>
  na.omit() |>
  mutate(Year2 = as.numeric(`Date of Birth`)) |>
  filter(Year2>= 1960, Year2 <= 2000) |>
  mutate(Decade2 = round(Year2/5)*5) |>
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
  write_csv("Cleaned_Instruments.csv")

Filtered_instruments <- filtered_full_data |>
  filter(Instrument == "Piano" | Instrument == "Guitar" | Instrument =="Music software" | 
           Instrument == "Voice" | Instrument == "Drums")


plot_instruments <- Filtered_instruments |>
  group_by(Decade2, Instrument) |>
  summarize(count1 = n()) |>
  ungroup()

print(genres_filter)



ggplot(data = plot_instruments) + 
  aes(x = Decade2, y = count1, color = Instrument, group = Instrument) +
  geom_line() +
  labs(title = "Instrument Counts in the 20th Century",
       x = "Decade",
       y = "Instrument Counts") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("Instrument count per decade.pdf")


ggplot(data = pivoted_genre_dataset) + 
  aes(x = Decade1, y = Percentage, color = Genre) +
  geom_line() 









 



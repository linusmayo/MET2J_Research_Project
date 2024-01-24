library(tidyverse)
library(tidyr)

#------------------------------------GENRES-------------------------------------

genre_data <- read_csv("Genre.csv")

genres_filter <- genre_data |>
  pivot_longer(cols=-'Primary Genre', names_to="Year", values_to="Percentage") |>
  mutate(Year = as.numeric(Year)) |>
  filter(Year >= 1958, Year <= 2002) |>
  mutate(Quinquiennial = round(Year/5)*5) |>
  select(-Year) |>
  mutate(Percentage = as.numeric(str_replace(Percentage, "%", ""))) |>
  mutate(Percentage = if_else(is.na(Percentage), 0, Percentage)) |>
  group_by(Quinquiennial, `Primary Genre`) |>
  summarise(Percentage = mean(Percentage)) |>
  pivot_wider(names_from="Primary Genre", values_from = "Percentage")

ggplot(data = genres_filter, aes(x = Quinquiennial)) + 
  geom_line(aes(y= `House/Electronic/Trance`), color = "navyblue") +
  geom_line(aes(y= Jazz), color = "gold") +
  geom_line(aes(y= Pop), color = "purple3") +
  geom_line(aes(y= Rock), color = "forestgreen") 

print(genres_filter)

#----------------------------------INSTRUMENTS----------------------------------

#Reading in the data from the csv with the data we extracted from Wiki source
full_data <-read_csv('MET2J_filtered_instruments.csv')

filtered_full_data <- full_data |>
  na.omit() |>
  mutate(`Date of Birth` = as.numeric(`Date of Birth`)) |>
  filter(`Date of Birth` >= 1960, `Date of Birth` <= 2000) |>
  mutate(Quinquiennial = round(`Date of Birth`/5)*5) |>
  select(-`Date of Birth`) |>
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
  #Choosing the relevant instruments 
  filter(Instrument == "Piano" | Instrument == "Guitar" | Instrument =="Music software" | 
           Instrument == "Voice") |>
  #Counting instruments per every five years to plot e.g. 'total guitars in 1965' 
  group_by(Quinquiennial, Instrument) |>
  summarize(count1 = n()) |>
  ungroup()

#Each instrument is a line, x-axis is years (every 5), y-axis is number of instruments
ggplot(data = filtered_full_data) + 
  aes(x = Quinquiennial, y = count1, color = Instrument, group = Instrument) +
  geom_line() +
  labs(title = "Instrument Counts in the 20th Century",
       x = "Quinquiennial",
       y = "Instrument Counts") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))








 



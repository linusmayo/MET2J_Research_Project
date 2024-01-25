library(tidyverse)
library(tidyr)
library(cowplot)

#----------------------------------INSTRUMENTS----------------------------------

#Reading in the data from the csv with the data we extracted from Wiki source
full_data <-read_csv('MET2J_filtered_instruments.csv')

#Shaping data: remove NAs, values to numeric, measurement every 5 years, group instruments
filtered_instruments <- full_data |>
  na.omit() |>
  mutate(`Date of Birth` = as.numeric(`Date of Birth`)) |>
  filter(`Date of Birth` >= 1956, `Date of Birth` <= 2012) |>
  mutate(Quinquennial = round(`Date of Birth`/5)*5) |>
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
                             "Fairlight CMI" = "Piano", "Pianist" = "Piano")) |>
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
                             "Classical guitar" = "Guitar", "Guild Guitar Company" = "Guitar", "Modulus Guitars" = "Guitar")) |>
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
                             "Game Boy music" = "Music software", "Ableton Live" = "Music software", "Ableton Live" = "Music software","Synthesizer" = "Music software" )) |>
  mutate(Instrument = recode(Instrument, "Drum" = "Drums" , "Bass drum" = "Drums",
                             "Drum machine" = "Drums", "Sabian" = "Drums",
                             "Drum Workshop" = "Drums", "Drum" = "Drums",
                             "Gretsch Drums" = "Drums", "Percussion instrument" = "Drums",
                             "Noble & Cooley" = "Drums", "Tom-tom drum" = "Drums",
                             "Mapex Drums" = "Drums", "Drum kit" = "Drums", "Ludwig Drums" = "Drums" )) |>
  mutate(Instrument = recode(Instrument, "Types of trombone" = "Trombone", "C.G. Conn" = "Trombone")) |>
  #Choosing the relevant instruments
  group_by(Quinquennial, Instrument) |>
  summarize(count1 = n()) |>
  ungroup() |>
  group_by(Quinquennial) |>
  mutate(counttotal = sum(count1)) |>
  ungroup() |>
  filter(Instrument == "Piano" | Instrument == "Guitar" | Instrument =="Music software" |
           Instrument == "Banjo" | Instrument == "Voice") |>
  mutate(PercentageInstrument = (count1/counttotal)*100)


#Each instrument is a line, x-axis is years (every 5), y-axis is number of instruments
plot_instruments <- ggplot(data = filtered_instruments) +
  aes(x = Quinquennial, y = PercentageInstrument, color = Instrument, group = Instrument) +
  geom_line(linewidth = 1) +
  labs(x = "Years", y = "Percentage") +
  xlim(c(1960, 1995)) +
  ylim(c(0, 35)) +
  theme_light() +
  theme(plot.title = element_text(face="bold"))+
  scale_color_manual(values = c(
    "Music software" = "purple3",
    "Piano" = "blue3",
    "Banjo" = "orange3",
    "Guitar" = "darkgreen",
    "Voice" = "indianred3"
  ))
ggsave("Instruments.pdf", width = 12, height = 8, units = "cm")

#------------------------------------GENRES-------------------------------------

#Reading in the data from the csv with the data we exported from theDataFace
genre_data <- read_csv("Genre_New.csv")

#Shaping data: pivoting, values to numeric, measurement every 5 years & summarise
filtered_genres <- genre_data |>
  pivot_longer(cols=-'Primary Genre', names_to="Year", values_to="Percentage") |>
  mutate(Year = as.numeric(Year)) |>
  filter(Year >= 1956, Year <= 2012) |>
  mutate(Quinquennial = round(Year/5)*5) |>
  select(-Year) |>
  mutate(Percentage = as.numeric(str_replace(Percentage, "%", ""))) |>
  mutate(Percentage = if_else(is.na(Percentage), 0, Percentage)) |>
  group_by(Quinquennial, `Primary Genre`) |>
  summarise(Percentage = mean(Percentage))

#Plotting data with geom line
plot_genres <- ggplot(data = filtered_genres, aes(x = Quinquennial, y = Percentage, color = `Primary Genre`)) +
  geom_line(linewidth = 1) +
  labs(x= "Years", y="Percentage of songs in Billboard 100") +
  xlim(c(1960, 1995)) +
  theme_light() +
  theme(plot.title = element_text(face="bold"))+
  scale_color_manual(values = c(
    "House/Electronic/Trance" = "grey",
    "Jazz" = "gold",
    "Pop" = "lightgreen",
    "Rock" = "lightblue",
    "Folk" = "magenta3"
  ))
ggsave("Genres.pdf", width = 12, height = 8, units = "cm")

#----------------------DOUBLE PLOT: INSTRUMENTS & GENRES------------------------

plot_grid(plot_genres, plot_instruments, nrow = 2, align = "v", rel_heights = 1)
ggsave("Comparison_Plot.pdf",height = 10, width = 8)

#----------------OVERLAP PLOT: INSTRUMENT AND CORRESPONDING GENRE---------------

guitar_data <- filtered_instruments |>
  filter(Instrument == "Guitar")

rock_data <- filtered_genres |>
  filter(`Primary Genre` == "Rock")

plot_guitar <- ggplot() +
  geom_line(data = guitar_data, aes(x = Quinquennial, y = PercentageInstrument, color = "Guitar"), linewidth = 1.5) +
  geom_line(data = rock_data, aes(x = Quinquennial, y = Percentage, color = "Rock"), linewidth = 1.5) +
  scale_y_continuous(sec.axis = dup_axis(name = "Guitar prevalence (%)")) +
  scale_color_manual(values = c("Guitar" = "darkgreen", "Rock" = "lightblue")) +
  labs(x= "Years", y="Rock prevalence (%)", title="", color = "Legend") +
  labs(fill = "Legend") +
  theme_light() +
  theme(plot.title = element_text(face="bold")) +
  xlim(c(1960, 1995)) +
  geom_segment(aes(x = 1990, y = 0, xend = 1990, yend = 30),linetype="dashed", color = "black") +
  geom_segment(aes(x = 1985, y = 0, xend = 1985, yend = 58),linetype="dashed", color = "black")

ggsave("Rock-Guitar.pdf")

#----------- - - - - -

banjo_data <- filtered_instruments |>
  filter(Instrument == "Banjo")

folk_data <- filtered_genres |>
  filter(`Primary Genre` == "Folk")

plot_banjo <- ggplot() +
  geom_line(data = banjo_data, aes(x = Quinquennial, y = PercentageInstrument, color = "Banjo"), linewidth = 1.5) +
  geom_line(data = folk_data, aes(x = Quinquennial, y = Percentage, color = "Folk"), linewidth = 1.5) +
  scale_y_continuous(sec.axis = dup_axis(name = "Banjo prevalence (%)")) +
  scale_color_manual(values = c("Banjo" = "orange3", "Folk" = "magenta3")) +
  labs(x= "Years", y="Folk prevalence (%)", title="", color = "Legend") +
  labs(fill = "Legend") +
  theme_light() +
  theme(plot.title = element_text(face="bold")) +
  xlim(c(1960, 1995)) +
  geom_segment(aes(x = 1975, y = 0, xend = 1975, yend = 6.9),linetype="dashed", color = "black") +
  geom_segment(aes(x = 1980, y = 0, xend = 1980, yend = 5.1),linetype="dashed", color = "black")

ggsave("Folk-Banjo.pdf")

#----------- - - - - -

software_data <- filtered_instruments |>
  filter(Instrument == "Music software")

house_data <- filtered_genres |>
  filter(`Primary Genre` == "House/Electronic/Trance")

plot_software <- ggplot() +
  geom_line(data = software_data, aes(x = Quinquennial, y = PercentageInstrument, color = "Music software"), linewidth = 1.5) +
  geom_line(data = house_data, aes(x = Quinquennial, y = Percentage, color = "House/Electronic/Trance"), linewidth = 1.5) +
  scale_y_continuous(sec.axis = dup_axis(name = "Music software prevalence (%)")) +
  scale_color_manual(values = c("Music software" = "purple", "House/Electronic/Trance" = "grey")) +
  labs(x= "Years", y="Electronic Music prevalence (%)", color = "Legend") +
  labs(fill = "Legend") +
  theme_light() +
  theme(plot.title = element_text(face="bold")) +
  xlim(c(1960, 1995)) +
  geom_segment(aes(x = 1990, y = 0, xend = 1990, yend = 11.25),linetype="dashed", color = "black") +
  geom_segment(aes(x = 1990, y = 0, xend = 1990, yend = 4),linetype="dashed", color = "black")

ggsave("Electronic-Software.pdf")

#----------- - - - - -

voice_data <- filtered_instruments |>
  filter(Instrument == "Voice")

pop_data <- filtered_genres |>
  filter(`Primary Genre` == "Pop")

plot_voice <- ggplot() +
  geom_line(data = voice_data, aes(x = Quinquennial, y = PercentageInstrument, color = "Voice"), linewidth = 1.5) +
  geom_line(data = pop_data, aes(x = Quinquennial, y = Percentage, color = "Pop"), linewidth = 1.5) +
  scale_y_continuous(sec.axis = dup_axis(name = "Voice prevalence (%)")) +
  scale_color_manual(values = c(Pop = "lightgreen", Voice = "red3")) +
  labs(x= "Years", y="Pop prevalence (%)", title= "", color = "Legend") +
  labs(fill = "Legend") +
  theme_light() +
  theme(plot.title = element_text(face="bold")) +
  xlim(c(1960, 1995)) +
  geom_segment(aes(x = 1995, y = 0, xend = 1995, yend = 32),linetype="dashed", color = "black") +
  geom_segment(aes(x = 1990, y = 0, xend = 1990, yend = 24),linetype="dashed", color = "black")

ggsave("Pop-Voice.pdf")

#----------- - - - - -

piano_data <- filtered_instruments |>
  filter(Instrument == "Piano")

jazz_data <- filtered_genres |>
  filter(`Primary Genre` == "Jazz")

plot_piano <- ggplot() +
  geom_line(data = piano_data, aes(x = Quinquennial, y = PercentageInstrument, color = "Piano"), linewidth = 1.5) +
  geom_line(data = jazz_data, aes(x = Quinquennial, y = Percentage, color = "Jazz"), linewidth = 1.5) +
  scale_color_manual(values = c(Piano = "blue3", Jazz = "gold")) +
  scale_y_continuous(sec.axis = dup_axis(name = "Piano prevalence (%)")) +
  labs(x= "Years", y="Jazz prevalence (%)", title="", color = "Legend") +
  labs(fill = "Legend") +
  theme_light() +
  theme(plot.title = element_text(face="bold")) +
  xlim(c(1960, 1995)) +
  geom_segment(aes(x = 1960, y = 0, xend = 1960, yend = 3),linetype="dashed", color = "black") +
  geom_segment(aes(x = 1980, y = 0, xend = 1980, yend = 12.3),linetype="dashed", color = "black")

ggsave("Jazz-Piano.pdf")


#--------------------COMBINING ALL DIFFERENT INDIVIDUAL GRAPHS------------------

plot_grid(plot_guitar, plot_banjo, plot_software, plot_voice, plot_piano, nrow = 5, align = "v", rel_heights = 1)

ggsave("Comparison_Plot1.pdf",height = 10, width = 8)




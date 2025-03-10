
# file: PFNACorgangraphs.R
# purpose: statistical analysis Organ Wt Data 90-day (CSS401.6.7)
# 1/15/25 - EMK edit to make this file for generating graphics only, use in ms & poster
# uses data file OrganWeightsPFAS.xlsx
#
# 3/5/2025
# Renamed LiveryRel1m.png as PFNAC_Liver_Male.png
# Renamed LiverRel1f as PFNAC_Liver_Male

# 2/27/2025
# increase sizes of axis labels and plot title by modifying the theme's axis text and title, and centering the heading and increasing its font size

# 2/11/2025
# uncomment stat_summary so that mean can be displayed as a red triangle

# Load needed R packages
library(ggplot2)
library(dplyr)
library(readxl)

# Read in the data file
PFNACo <- read_excel("OrganWeightsPFAS.xlsx", sheet = 'PFNAC', na = c("", "NA"))

# Remove rows 63 through the last row and eliminate Females
PFNACom <- PFNACo[-c(61:nrow(PFNACo)), ]
#View(PFNACom)

# Ensure Group is a factor to align correctly
PFNACom$Group <- as.factor(PFNACom$Group)
top_doses$Group <- as.factor(top_doses$Group)

# Identify the top 4 highest relative liver weights
top_doses <- PFNACom %>%
  group_by(Group) %>%
  summarise(max_LiverRel = max(LiverRel, na.rm = TRUE)) %>% # Find max per group
  arrange(desc(max_LiverRel)) %>% # Sort by highest values
  slice(1:4) %>% # Select top 4 highest values
  mutate(y_position = max_LiverRel + 0.05) # Adjust y-position for asterisk placement



# BOX PLOTS - Male Rat Relative Liver Weight Data PFNAC
#LiverRel1m
PFNAC_Liver_Male<- ggplot(
  PFNACom,
  aes(
    x = factor(Group),
    y = LiverRel,
    fill = factor(Group) # Fill for violin plot
  )
) +
  geom_violin(trim = TRUE, alpha = 0.7, color = "black") + # Violin plot
  geom_boxplot(
    aes(fill = factor(Group)), # Separate fill for box plot
    width = 0.1,
    color = "black",
    alpha = 0.9
  ) +
  # Add median red triangle marker
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "red") +
  # Add asterisks above the top 4 doses
  geom_text(
    data = top_doses,
    aes(x = Group, y = y_position, label = "*"),
    size = 6, color = "black"
  ) +
  labs(
    title = "PFNAC - Male Rat Liver/Body Weight 90-Day Study",
    x = "Dose Group (mg/kg-day)",
    y = "Liver/BW Ratio(%)"
  ) +
  scale_fill_brewer(palette = "Set2", name = "Group") + # Colorful palette
  theme_bw() +
  theme(legend.position = "none") +
  theme(
    axis.text = element_text(size = 14), # increase tick label size
    axis.title = element_text(size = 16), # increase axis title size
    plot.title = element_text(size = 18, hjust = 0.5) # increase plot title size
  )


# Save the plot as a PNG file
ggsave(
  filename = "PFNAC_Liver_Male.png",
  plot = PFNAC_Liver_Male,
  width = 8,
  height = 6,
  dpi = 300
)

# Display the plot
#LiverRel1m
PFNAC_Liver_Male

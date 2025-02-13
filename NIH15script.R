library(tidyverse)
require(ggplot2)
library(RColorBrewer)
library(ggrepel)
library(ggalt)
library(ggtext)
library(factoextra)
library(scales)
library(stringr)
library(magick)
library(broom)
library(ggstats)
library(extrafont)

rm(list=ls(all=TRUE))


Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

theme_AVP <- function(){
  font <- "Calibri Light"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_line(color="gray75"),    #major gridlines, but can be turned off
      #panel.grid.minor = element_blank(),    #strip minor gridlines
      #axis.ticks = element_blank(),          #strip axis ticks
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 14,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2,
        margin=margin(5, b=5)),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 12,
        hjust = 0,
        vjust = 2,
        margin=margin(0, b=5)),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1,
        color="gray40"),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 11),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis family
        size = 10),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

# data preparation --------------------------------------------------------


# read in data https://reporter.nih.gov/exporter

NIHdat <- read.csv("RePORTER_PRJ_C_FY2024.csv")

# clean up the data
dat_IDC_All <- NIHdat |>
  mutate(activity_code = substr(ACTIVITY, 1, 1)) |>
  mutate(inst_type = ifelse(ED_INST_TYPE != "", "Higher Ed", "Not Higher Ed")) |>
  filter(INDIRECT_COST_AMT != 0) |>
  mutate(org_name = str_to_title(ORG_NAME)) |>
  mutate(IDC = as.numeric(INDIRECT_COST_AMT / DIRECT_COST_AMT)) |>
  mutate(activity_grouped = ifelse(activity_code == "R" | activity_code == "P" | activity_code == "U", activity_code,
                                   ifelse(activity_code == "K" | activity_code == "T" | activity_code == "D", "Training (K,T,D)", "Other (O,S,G)"))) |>
  mutate(activity_grouped = factor(activity_grouped, levels=c("R", "U", "P", "Training (K,T,D)", "Other (O,S,G)")))





# summarize current state by institution for all grants
group_All_total <- dat_IDC_All |>
  group_by(org_name, inst_type) |>
  summarize(n_total_grants = n(), 
            meanIDC = mean(IDC, na.rm = TRUE),
            amt_total_direct = sum(DIRECT_COST_AMT), 
            amt_total_indirect = sum(INDIRECT_COST_AMT))

# calculate inferred IDC negotiated rate
# method: utilize Research projects (activity_code == "R") to avoid subprojects, training grants, etc.

dat_IDC_All_R <- dat_IDC_All |>
  filter(activity_code == "R")

group_IDC_all <- dat_IDC_All_R  |>
  group_by(org_name) |>
  summarize(modeIDC = Mode(IDC))

group_IDC_complete <- left_join(group_All_total, group_IDC_all)

# select only higher education institutions
group_IDC_HE <- group_IDC_complete |>
  filter(inst_type == "Higher Ed")


# calculate NIH cap impact ------------------------------------------------

# NIH proposal is for a 15% cap
# interpret this as a cap, rather than raising low-IDC grants (e.g., training grants) up to 15%

IDC_cap <- 0.15
mmlabel <- 1000000

proforma_IDC <- dat_IDC_All |>
  mutate(newIDC = as.numeric(ifelse(IDC <= IDC_cap, IDC, IDC_cap)*DIRECT_COST_AMT)) |>
  mutate(deltaIDC = INDIRECT_COST_AMT - newIDC)

group_proforma_IDC <- proforma_IDC |>
  group_by(inst_type, org_name, ORG_STATE) |>
  summarize(IDC_decline = sum(deltaIDC), oldIDC = sum(INDIRECT_COST_AMT), 
            totaldirect = sum(DIRECT_COST_AMT)) |>
  mutate(IDC_pct_decline = IDC_decline / oldIDC) |>
  mutate(IDC_decline_mm = IDC_decline / mmlabel) |>
  mutate(totaldirect_mm = totaldirect / mmlabel) |>
  mutate(oldIDC_mm = oldIDC / mmlabel) |>
  filter(IDC_decline >= 0) |>
  mutate(IDC_plot_name = ifelse(IDC_decline_mm >= 10, org_name, "Other")) |>
  arrange(IDC_decline_mm, decreasing = TRUE, .by_group = TRUE)

write.csv(group_proforma_IDC, "group_proforma_IDC15_all.csv")

group_proforma_IDC_HE <- group_proforma_IDC |>
  filter(inst_type == "Higher Ed")

write.csv(group_proforma_IDC_HE, "group_proforma_IDC15_HE.csv")

proforma_forplots <- left_join(group_proforma_IDC, group_IDC_all)

proforma_forplots <- proforma_forplots |>
  ungroup() |>
  mutate(IDC_negotiated_bins = cut(modeIDC, quantile(modeIDC, 0:4/4, na.rm=TRUE))) |>
  mutate(direct_bins = cut(totaldirect_mm, quantile(totaldirect_mm, 0:4/4, na.rm=TRUE))) |>
  mutate(direct_bins_deciles = cut(totaldirect_mm, quantile(totaldirect_mm, 0:10/10, na.rm=TRUE))) |>
  arrange(totaldirect_mm, decreasing = TRUE, .by_group = TRUE)


# plot outcomes -----------------------------------------------------------


# LinkedIn post plot 2/8/2025 ------------------------------------------------------


plot_IDC_inst_15risk <- ggplot(data=subset(proforma_forplots, inst_type == "Higher Ed"), aes(x=modeIDC, y=totaldirect_mm, label=org_name, size=IDC_decline_mm))
plot_IDC_inst_15risk + geom_point(aes(color=IDC_decline_mm)) + 
  geom_text_repel(size=2) + 
  theme_AVP() + 
  scale_size_continuous(labels=scales::dollar_format()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  geom_vline(xintercept=0.15, color='gray50', linetype=2) +
  scale_y_continuous(labels=scales::dollar_format(), breaks=seq(0,700,100)) +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,0.8,0.05)) +
  scale_color_distiller(palette="Spectral") +
  theme(legend.position = "inside", legend.position.inside = c(0.15, 0.75),
        legend.background = element_rect(fill = 'gray95', color='black')) +
  guides(color="none") +
  #guides(size="Impact") +
  labs(title="Impact of NIH 15% Indirect Rate", 
       subtitle="Negotiated IDC Rate and Total Direct Funding",
       x="Inferred* Negotiated IDC Rate", y="Total Direct",
       size="Impact of NIH 15% ($M)",
       caption ="Note * Inferred from mode R grant awarded IDC\nSource: NIH ExPorter (FY2024)\nAd Verum Partners analysis")
ggsave("NIH15_risk.png", width=10, height=6.5)

# follow-up 2/13/2025 HE versus non-HE
plot_IDC_inst_15risk_notHE <- ggplot(data=subset(proforma_forplots, inst_type == "Not Higher Ed"), aes(x=modeIDC, y=totaldirect_mm, label=org_name, size=IDC_decline_mm))
plot_IDC_inst_15risk_notHE + geom_point(aes(color=IDC_decline_mm)) + 
  geom_text_repel(size=2) + 
  theme_AVP() + 
  scale_size_continuous(labels=scales::dollar_format()) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.background = element_rect(fill = 'gray95')) +
  geom_vline(xintercept=0.15, color='gray50', linetype=2) +
  scale_y_continuous(labels=scales::dollar_format(), breaks=seq(0,700,100)) +
  scale_x_continuous(labels = scales::percent_format(), breaks=seq(0,1.7,0.1)) +
  scale_color_distiller(palette="Spectral") +
  theme(legend.position = "inside", legend.position.inside = c(0.15, 0.75),
        legend.background = element_rect(fill = 'gray95', color='black')) +
  guides(color="none") +
  #guides(size="Impact") +
  labs(title="Impact of NIH 15% Indirect Rate - Non-Higher Ed Institutions", 
       subtitle="Negotiated IDC Rate and Total Direct Funding",
       x="Inferred* Negotiated IDC Rate", y="Total Direct",
       size="Impact of NIH 15% ($M)",
       caption ="Note * Inferred from mode R grant awarded IDC\nSource: NIH ExPorter (FY2024)\nAd Verum Partners analysis")
ggsave("NIH15_risk_notHE.png", width=10, height=6.5)


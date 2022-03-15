# Importing the collection
hec_dataset <- read.csv("hec_dataset.csv", header=T, sep=",")

hec_dataset

summary(hec_dataset)

library(ggplot2)
library(viridis)

# Setting up the Giovannelli Lab plot theme

theme_glab <- function(base_size = 11,
                    base_family = "",
                    base_line_size = base_size / 180,
                    base_rect_size = base_size / 180) {
   
    font <- "Helvetica" #assign font family up front
   
    theme_bw(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
        legend.background =  element_blank(),
        legend.title =       element_text(color = rgb(100, 100, 100, maxColorValue = 255),
                                          size = rel(0.65),
                                         hjust = 0),
        legend.text =        element_text(color = rgb(100, 100, 100, maxColorValue = 255),
                                          size = rel(0.65)),
        legend.key.size =    unit(0.8, "lines"),
     
      plot.title = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        hjust = 0),
       
      axis.title = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.65)),
      axis.text = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.65)),
       
      plot.caption = element_text(
        color = rgb(100, 100, 100, maxColorValue = 255),
        size = rel(0.35),
        hjust = 1),
       
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.border = element_rect(fill = NA, colour = rgb(100, 100, 100, maxColorValue = 255)),

     
      complete = TRUE
    )
}

# Example of plot options with the giovannelli lab theme
ggplot(sample_data(prok_data), aes(temp,din)) +
geom_point(aes(fill=po4), shape = 21, size=3, alpha=0.8, stroke=0.2) +
scale_fill_viridis(option="viridis") +
labs(title="",
     subtitle="",
     fill=expression(paste(PO[4])),
     y="Dissolved Inorganic Nitrogen (µM)",
     x="Temperature (°C)",
     caption = 'Giovannelli Lab / @d_giovannelli') +
theme_glab() 

ggsave("diversity_phylum.svg", height=8, width=8)

# Setting bigger plots in the session
#library(repr)
options(repr.plot.width=12, repr.plot.height=8)

names(hec_dataset)

# He isotopes vs lat

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(rc_ra,lat)) +
geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     x="³He/⁴He (Rc/Ra)",
     y="Latitude (°N)") +
theme_glab(base_size = 22)

ggsave(file="Figure2_He isotopes vs lat.svg", width=12, height=8)

#Co2/3he vs lat

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(co2_he3,lat)) +
geom_rect(aes(xmin = 1e+09, xmax = 1.6e+09, ymin = -45, ymax = -15), fill = "lightgrey", alpha=0.3) +
annotate(geom="text", x=1.25e+09, y=-30, label="mantle", color="black", size=6, alpha=0.6, angle = 90) +
geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
scale_x_continuous(trans='log10') +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     x="CO₂/³He",
     y="Latitude (°N)") +
theme_glab(base_size = 22)

ggsave(file="Figure3_Co2-3he vs lat.svg", width=12, height=8)

# Importing mixing lines
mix_m_a <- read.csv("mix_mantle_air.csv", sep=",", header=T)
mix_c_a <- read.csv("mix_crust_air.csv", sep=",", header=T)

# 3/4 vs 4/20 fig

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(r420, r_ra)) +

geom_line(data=mix_m_a, aes(r420mix,r_ramix)) +
geom_line(data=mix_c_a, aes(r420mix,r_ramix)) +

geom_rect(aes(xmin = 0.9e+03, xmax = 1.1e+03, ymin = 7.9, ymax = 8.15), fill = "red", color="black", size=0.2, alpha=0.3) +
geom_rect(aes(xmin = 0.9e+03, xmax = 1.1e+03, ymin = -0.05, ymax = 0.25), fill = "salmon4", color="black", size=0.2, alpha=0.3) +
geom_rect(aes(xmin = 0.24, xmax = 0.30, ymin = 0.95, ymax = 1.2), fill = "royalblue3", color="black", size=0.2, alpha=0.3) +

annotate(geom="text", x=1e+03, y=7.7, label="mantle", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=1e+03, y=-0.3, label="crust", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=0.26, y=0.8, label="air", color="black", size=6, alpha=0.6) +

geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +

scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
scale_x_continuous(trans='log10') +

labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="³He/⁴He (R/Ra)",
     x="⁴He/²⁰Ne") +
theme_glab(base_size = 22)

ggsave(file="Figure4_3-4 vs 4-20.svg", width=12, height=8)

# Importing mixing lines
mix_m_s <- read.csv("mix_mantle_sediment.csv", sep=",", header=T)
mix_m_l <- read.csv("mix_mantle_limestone.csv", sep=",", header=T)
mix_s_l <- read.csv("mix_limestone_sediment.csv", sep=",", header=T)

# Co2/3he vs d13 – all data

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(d13c, co2_he3)) +

#geom_line(data=mix_m_s, aes(d13cmix,co2_3hemix)) +
#geom_line(data=mix_m_l, aes(d13cmix,co2_3hemix)) +
#geom_line(data=mix_s_l, aes(d13cmix,co2_3hemix)) +

#geom_rect(aes(xmin = -6.6, xmax = -5.4, ymin = 1.2e+09, ymax = 2.2e+09), fill = "red", color="black", size=0.2, alpha=0.3) +
#geom_rect(aes(xmin = -30.6, xmax = -29.4, ymin = 0.7e+13, ymax = 1.2e+13), fill = "salmon4", color="black", size=0.2, alpha=0.3) +
#geom_rect(aes(xmin = -0.6, xmax = 0.5, ymin = 0.7e+13, ymax = 1.2e+13), fill = "lightgrey", color="black", size=0.2, alpha=0.3) +

#annotate(geom="text", x=-6, y=0.8e+09, label="mantle", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=-30, y=1.65e+13, label="sediment", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=0, y=1.65e+13, label="crust", color="black", size=6, alpha=0.6) +

#geom_line(data=calcite_lines, aes(d13C_122,co2_he3_f), color="darkgrey", size=1) +
#geom_line(data=calcite_lines, aes(d13C_65,co2_he3_f), linetype="dashed", color="grey") +
#geom_line(data=calcite_lines, aes(d13C_150,co2_he3_f), linetype="dashed", color="grey") +

#annotate(geom="text", x=-32, y=1e+08, label="122 °C", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=-42, y=1e+09, label="65 °C", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=-20, y=1e+08, label="150 °C", color="black", size=6, alpha=0.6) +

geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
xlim(-50,1.5) +
scale_y_continuous(trans='log10') +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="CO₂/³He",
     x="δ¹³C") +
theme_glab(base_size = 22)

ggsave(file="Figure6a_Co2-3he vs d13.svg", width=12, height=8)

# Co2/3he vs d13 (gas only)

ggplot(subset(hec_dataset, phase %in% c("G")), aes(d13c, co2_he3)) +

geom_line(data=mix_m_s, aes(d13cmix,co2_3hemix)) +
geom_line(data=mix_m_l, aes(d13cmix,co2_3hemix)) +
geom_line(data=mix_s_l, aes(d13cmix,co2_3hemix)) +

geom_rect(aes(xmin = -6.6, xmax = -5.4, ymin = 1.2e+09, ymax = 2.2e+09), fill = "red", color="black", size=0.2, alpha=0.3) +
geom_rect(aes(xmin = -30.6, xmax = -29.4, ymin = 0.7e+13, ymax = 1.2e+13), fill = "salmon4", color="black", size=0.2, alpha=0.3) +
geom_rect(aes(xmin = -0.6, xmax = 0.5, ymin = 0.7e+13, ymax = 1.2e+13), fill = "lightgrey", color="black", size=0.2, alpha=0.3) +

annotate(geom="text", x=-6, y=0.8e+09, label="mantle", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=-30, y=1.65e+13, label="sediment", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=0, y=1.65e+13, label="limestone", color="black", size=6, alpha=0.6) +

geom_point(aes(fill=region, alpha=origin), shape=24, size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=24))) +
xlim(-50,1.5) +
#ylim(1e+06,1e+13) +
scale_y_continuous(trans='log10') +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="CO₂/³He",
     x="δ¹³C") +
theme_glab(base_size = 22)

ggsave(file="Figure6b_Co2-3he vs d13.svg", width=12, height=8)

# Importing calcite precipitation lines
calcite_lines <- read.csv("calcite_lines.csv", sep=",", header=T)
names(calcite_lines)

# Co2/3he vs d13 (water only)

ggplot(subset(hec_dataset, phase %in% c("W")), aes(d13c, co2_he3)) +

#geom_line(data=mix_m_s, aes(d13cmix,co2_3hemix)) +
#geom_line(data=mix_m_l, aes(d13cmix,co2_3hemix)) +
#geom_line(data=mix_s_l, aes(d13cmix,co2_3hemix)) +

#geom_rect(aes(xmin = -6.6, xmax = -5.2, ymin = 1.2e+09, ymax = 2.2e+09), fill = "red", color="black", size=0.2, alpha=0.3) +
#geom_rect(aes(xmin = -30.6, xmax = -29.2, ymin = 0.7e+13, ymax = 1.2e+13), fill = "salmon4", color="black", size=0.2, alpha=0.3) +
#geom_rect(aes(xmin = -0.6, xmax = 0.7, ymin = 0.7e+13, ymax = 1.2e+13), fill = "lightgrey", color="black", size=0.2, alpha=0.3) +

#annotate(geom="text", x=-6, y=0.8e+09, label="mantle", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=-30, y=1.65e+13, label="sediment", color="black", size=6, alpha=0.6) +
#annotate(geom="text", x=0, y=1.65e+13, label="crust", color="black", size=6, alpha=0.6) +

geom_line(data=calcite_lines, aes(d13C_122,co2_he3_f), color="darkgrey", size=1) +
geom_line(data=calcite_lines, aes(d13C_65,co2_he3_f), linetype="dashed", color="grey") +
geom_line(data=calcite_lines, aes(d13C_150,co2_he3_f), linetype="dashed", color="grey") +

annotate(geom="text", x=-32, y=1e+08, label="122 °C", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=-42, y=1e+09, label="65 °C", color="black", size=6, alpha=0.6) +
annotate(geom="text", x=-20, y=1e+08, label="150 °C", color="black", size=6, alpha=0.6) +

geom_point(aes(fill=region, alpha=origin), shape=21, size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
xlim(-50,1.5) +
scale_y_continuous(trans='log10') +
#ylim(1e+06,1e+13) +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="CO₂/³He",
     x="δ¹³C") +
theme_glab(base_size = 22)

ggsave(file="Figure6c_Co2-3he vs d13.svg", width=12, height=8)

# Co2/3he vs 3he/4he

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(rc_ra, co2_he3)) +

geom_rect(aes(xmin = 7.9, xmax = 8.1, ymin = 0.75e+09, ymax = 1.25e+09), fill = "red", color="black", size=0.2, alpha=0.3) +

annotate(geom="text", x=8, y=0.6e+09, label="mantle", color="black", size=6, alpha=0.6) +

geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +

scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
scale_y_continuous(trans='log10') +

labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="CO₂/³He",
     x="³He/⁴He (Rc/Ra)") +
theme_glab(base_size = 22)

ggsave(file="Figure7_Co2-Co2-3he vs 3he-4he.svg", width=12, height=8)

save.image()

install.packages('ggtern')

# Importing the collection
ternary_dataset <- read.csv("ternary_dataset.csv", header=T, sep=",")

summary(ternary_dataset)

library(ggtern)

reference_tern <- subset(ternary_dataset, origin %in% c("ref"))

reference_tern

ggtern::ggtern(subset(ternary_dataset, Region %in% c("CVZ", "SVZ")), aes(x3he*1.0e+06,xco2*0.0001,x4he*2)) +
geom_point(aes(fill=Region, alpha=origin, shape=Phase),size=6, stroke=0.3)+
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +

geom_point(data=reference_tern, fill=c("red","lightgrey","lightblue","yellow"), shape=22, size=8, stroke=0.3) +

guides(fill = guide_legend(override.aes=list(shape=21)))+

labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     y="CO₂×10⁻⁴",
     x="³He",
     z="⁴He")+

theme_hidemask()+
theme_bw(base_size = 16) +
theme_hidegrid_major()+
theme_nogrid_minor()+
theme_nomask()

ggsave(file="Figure5_ternary.svg", width=12, height=8)

#Rc/Ra vs LAB Depth

ggplot(subset(hec_dataset, region %in% c("CVZ", "SVZ")), aes(LabDepth,rc_ra)) +
geom_rect(aes(xmin = 1e+09, xmax = 1.6e+09, ymin = -45, ymax = -15), fill = "lightgrey", alpha=0.3) +
annotate(geom="text", x=1.25e+09, y=-30, label="mantle", color="black", size=6, alpha=0.6, angle = 90) +
geom_point(aes(fill=region, alpha=origin, shape=phase),size=6, stroke=0.3) +
scale_fill_manual(values=c("#00a2ff", "#ffb100")) +
scale_shape_manual(values = c(24, 21), labels = c("gas", "water")) +
scale_alpha_manual(values = c(0.3, 0.9),labels = c("literature", "this study")) +
guides(fill = guide_legend(override.aes=list(shape=21))) +
#scale_x_continuous(trans='log10') +
labs(title="",
     subtitle="",
     shape="Phase",
     alpha="Data from",
     fill="Volcanic Zone",
     x="LAB Depth (km)",
     y="³He/⁴He (Rc/Ra)") +
theme_glab(base_size = 22)

ggsave(file="Figure5_Rc-Ra vs LAB.svg", width=12, height=8)

save.image()

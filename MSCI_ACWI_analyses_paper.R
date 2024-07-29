### Analysis of footprinting results from MSCI ACWI companies and S&P Physical Asset data

# Load libraries
library(tidyverse)
library(cowplot) #for combining plots
library(svglite) #to save as svg

# Function to get date and time to append to filename
getdate<-function(){
  date = gsub(":","-",Sys.time()) #get date and time to append to filename
  date = gsub(" ","_",date) 
  return(date)
}

# Import company-level data
comps <-read.csv("msci_acwi_company_results.csv")


#check the file
head(comps)
str(comps)
dim(comps) #should have 2173 companies (rows) and 56 variables (columns)

#calculate some new variables
comps$total_flags <- with(comps, coastal_risk_reduction_service_flagged+nitrogen_retention_service_flagged+sediment_retention_service_flagged+
                            nature_access_flagged+endemic_biodiversity_flagged+redlist_species_flagged+endemic_biodiversity_flagged+
                            species_richness_flagged+kba_within_1km_flagged) #total flags; can be greater than number of assets

comps$total_flags_noNA <- with(comps, coastal_risk_reduction_service_flagged+nitrogen_retention_service_flagged+sediment_retention_service_flagged+
                                 endemic_biodiversity_flagged+redlist_species_flagged+endemic_biodiversity_flagged+
                                 species_richness_flagged+kba_within_1km_flagged) #total flags excluding nature access; can be greater than number of assets

comps$frac_flagged_assets<-comps$total_flagged/comps$total_assets

comps$frac_flagged_assets_noNA<-comps$total_flagged_no_nature_access/comps$total_assets

comps$flags_per_asset<-comps$total_flags/comps$total_assets

comps$flags_per_asset_noNA<-comps$total_flags_noNA/comps$total_assets

#Summary total impacts by sector for Table 1 of paper
sector_summary<-comps %>% 
  group_by(GICS.Sector) %>%
  summarise(companies=n(), assets=sum(total_assets), footprint_area=sum(total_area), 
            impact_crr=sum(coastal_risk_reduction_service_adj_sum), impact_nitrogen=sum(nitrogen_retention_service_adj_sum),
            impact_sed=sum(sediment_retention_service_adj_sum), impact_endemic=sum(endemic_biodiversity_adj_sum),
            impact_redlist=sum(redlist_species_adj_sum), impact_sr=sum(species_richness_adj_sum),
            impact_kba=sum(kba_within_1km_adj_sum))# , flags_per_asset=sum(total_flags_noNA)/sum(total_assets)#,
# flags_to_sales=sum(total_flags_noNA)/sum(SALES_USD, na.rm=T))


write.csv(sector_summary, file=paste0("sector_summary", "_", getdate(), ".csv"))

sector_summary$nit_per_asset<-sector_summary$impact_nitrogen/sector_summary$assets
sector_summary$mean_asset_size<-sector_summary$footprint_area/sector_summary$assets
sector_summary$nit_perkm2<-sector_summary$impact_nitrogen/sector_summary$footprint_area
sector_summary$sed_perkm2<-sector_summary$impact_sed/sector_summary$footprint_area
sector_summary$sr_perkm2<-sector_summary$impact_sr/sector_summary$footprint_area

#Figure 2: variation among companies within sectors
comps_plot<-comps %>% select("ISIN", "total_assets", "total_area", "SALES_USD", "GICS.Sector",
                             ends_with("_adj_sum") & !starts_with("nature_access"))

comps_plot_long<- comps_plot %>%   pivot_longer(
  cols = ends_with("_sum"),
  names_to="impact_metric",
  values_to="value"
) %>% mutate(impact_metric = case_when(impact_metric %in% "coastal_risk_reduction_service_adj_sum" ~ "Coastal risk reduction",
                                       impact_metric %in%  "nitrogen_retention_service_adj_sum" ~ "Nitrogen retention",
                                       impact_metric %in%  "sediment_retention_service_adj_sum" ~ "Sediment retention",
                                       impact_metric %in%  "endemic_biodiversity_adj_sum" ~ "Endemic species",
                                       impact_metric %in%  "redlist_species_adj_sum" ~ "Red List species",
                                       impact_metric %in%  "species_richness_adj_sum" ~ "Species richness",
                                       impact_metric %in% "kba_within_1km_adj_sum" ~ "KBAs"))

comps_plot_long$impact_metric<-as.factor(comps_plot_long$impact_metric)
comps_plot_long$impact_metric<-fct_relevel(comps_plot_long$impact_metric, "Coastal risk reduction", "Nitrogen retention", "Sediment retention", "Endemic species", "Red List species", "Species richness","KBAs")
comps_plot_long$value_revadj<-comps_plot_long$value/comps_plot_long$SALES_USD
comps_plot_long$value_revadj[comps_plot_long$value_revadj<0]<-NA #change to NA where sales is negative
comps_plot_long$value_km2<-comps_plot_long$value/comps_plot_long$total_area

#Total impact per company by sector
sector_total<-ggplot(data=comps_plot_long, aes(x=GICS.Sector, y=value+0.0000001))+ 
  geom_boxplot(aes(fill=GICS.Sector))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab("Total impact")+
  theme_minimal_hgrid(12)+theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  theme(plot.margin = margin(5.5,5.5,10,5.5))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

#Revenue-adjusted impact per company by sector
sector_rev_adj<-ggplot(data=comps_plot_long, aes(x=GICS.Sector, y=value_revadj+0.0000001))+
  geom_boxplot(aes(fill=GICS.Sector))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab("Revenue-adjusted impact")+
  theme_minimal_hgrid(12)+theme(legend.position = "none", strip.text.x = element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  theme(plot.margin = margin(5.5,5.5,10,5.5))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.000000001, 0.0000001, 0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

#Impact per area (answers question of whether sectors/companies tend to operate in higher impact areas)
sector_per_area<-ggplot(data=comps_plot_long, aes(x=GICS.Sector, y=value_km2+0.0000001))+
  geom_boxplot(aes(fill=GICS.Sector))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab(expression(paste("Impact per ",km^2)))+labs(fill="GICS Sector")+
  theme_minimal_hgrid(12)+theme(strip.text.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  labs(fill="Sector")+
  theme(plot.margin = margin(5.5,5.5,10,5.5))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.000000001, 0.0000001,0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

sector_legend<-get_legend(sector_per_area+theme(legend.justification = "center"))
sector_legend_h<-get_legend(sector_per_area+theme(legend.justification = "center", legend.direction="horizontal"))
sector_per_area<-sector_per_area+theme(legend.position = "none")

sector_plot<-plot_grid(plot_grid(sector_total, sector_rev_adj, sector_per_area, ncol=1, align="v"), sector_legend_h, ncol=1, align="v", rel_heights=c(5,1))

ggsave(file=paste0("sector_plot", "_", getdate(), ".png"), sector_plot,
       width = 14, height = 7, dpi = 300, units = "in", device='png', bg="white")


#Flags per asset
flags_plot<-comps %>% select("ISIN", "total_assets", "total_area", "SALES_USD", "GICS.Sector", "flags_per_asset_noNA", "frac_flagged_assets_noNA")

ggplot(data=flags_plot, aes(x=GICS.Sector, y=flags_per_asset_noNA))+
  geom_boxplot(aes(fill=GICS.Sector))+ylab("Mean flags per asset")

ggplot(data=flags_plot, aes(x=GICS.Sector, y=frac_flagged_assets_noNA))+
  geom_boxplot(aes(fill=GICS.Sector))+ylab("Proportion flagged assets")


#Figure 3: Industry-level deep dive
materials_summary<-comps %>% 
  filter(GICS.Sector=="Materials") %>%
  group_by(GICS.Industry) %>%
  summarise(companies=n(), assets=sum(total_assets), footprint_area=sum(total_area),
            impact_crr=sum(coastal_risk_reduction_service_adj_sum), 
            crr_per_km2=sum(coastal_risk_reduction_service_adj_sum)/sum(total_area),
            crr_per_revenue=sum(coastal_risk_reduction_service_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_nitrogen=sum(nitrogen_retention_service_adj_sum), 
            nit_per_km2=sum(nitrogen_retention_service_adj_sum)/sum(total_area),
            nit_per_revenue=sum(nitrogen_retention_service_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_sed=sum(sediment_retention_service_adj_sum), 
            sed_per_km2=sum(sediment_retention_service_adj_sum)/sum(total_area),
            sed_per_revenue=sum(sediment_retention_service_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_endemic=sum(endemic_biodiversity_adj_sum), 
            end_per_km2=sum(endemic_biodiversity_adj_sum)/sum(total_area),
            end_per_revenue=sum(endemic_biodiversity_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_redlist=sum(redlist_species_adj_sum),  
            rl_per_km2=sum(redlist_species_adj_sum)/sum(total_area),
            rl_per_revenue=sum(redlist_species_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_sr=sum(species_richness_adj_sum),
            sr_per_km2=sum(species_richness_adj_sum)/sum(total_area),
            sr_per_revenue=sum(species_richness_adj_sum)/sum(SALES_USD, na.rm=T),
            impact_kba=sum(kba_within_1km_adj_sum), 
            kba_per_km2=sum(kba_within_1km_adj_sum)/sum(total_area),
            kba_per_revenue=sum(kba_within_1km_adj_sum)/sum(SALES_USD, na.rm=T))

materials_summary$mean_asset_size<-materials_summary$footprint_area/materials_summary$assets

write.csv(materials_summary, file=paste0("materials_summary", "_", getdate(), ".csv"))


materials_long<-materials_summary %>%
  pivot_longer(
    cols = impact_crr:kba_per_revenue,
    names_to="impact_metric",
    values_to="value"
    
  )

GICS.I.cats<-length(materials_summary$GICS.Industry) #number of GICS categories

materials_long<-materials_long%>% add_column(impact_type=c(rep(c(rep("crr",3), rep("nitrogen",3), rep("sediment",3),
                                                                 rep("endemic",3),rep("redlist",3),rep("sp_rich",3),rep("kba",3)), GICS.I.cats)),
                                             metric_type=c(rep(c(rep(c("total", "per_km", "per_revenue"),7)), GICS.I.cats)))

materials_long$impact_type<-as.factor(materials_long$impact_type)
materials_long$impact_type<-fct_relevel(materials_long$impact_type, "crr", "nitrogen", "sediment", "endemic", "redlist", "sp_rich","kba")

ggplot(data=materials_long%>%filter(metric_type=="total"), aes(x=GICS.Industry, y=value))+
  geom_col(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_type), scales="free", ncol=7)+ylab("Total impact")

ggplot(data=materials_long%>%filter(metric_type=="per_km"), aes(x=GICS.Industry, y=value))+
  geom_col(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_type), scales="free", ncol=7)+ylab("Impact per km2")


ggplot(data=materials_long%>%filter(metric_type=="per_revenue"), aes(x=GICS.Industry, y=value))+
  geom_col(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_type), scales="free", ncol=7)+ylab("Impact per revenue")

#Figure to show variation among companies by industry within Materials sector

mats_plot<-comps %>% filter(GICS.Sector=="Materials") %>% select("ISIN", "total_assets", "total_area", "SALES_USD", "GICS.Industry",
                                                                 ends_with("_adj_sum") & !starts_with("nature_access"))

mats_plot_long<- mats_plot %>%   pivot_longer(
  cols = ends_with("_sum"),
  names_to="impact_metric",
  values_to="value"
) %>% mutate(impact_metric = case_when(impact_metric %in% "coastal_risk_reduction_service_adj_sum" ~ "Coastal risk reduction",
                                       impact_metric %in%  "nitrogen_retention_service_adj_sum" ~ "Nitrogen retention",
                                       impact_metric %in%  "sediment_retention_service_adj_sum" ~ "Sediment retention",
                                       impact_metric %in%  "endemic_biodiversity_adj_sum" ~ "Endemic species",
                                       impact_metric %in%  "redlist_species_adj_sum" ~ "Red List species",
                                       impact_metric %in%  "species_richness_adj_sum" ~ "Species richness",
                                       impact_metric %in% "kba_within_1km_adj_sum" ~ "KBAs"))

mats_plot_long$impact_metric<-as.factor(mats_plot_long$impact_metric)
mats_plot_long$impact_metric<-fct_relevel(mats_plot_long$impact_metric, "Coastal risk reduction", "Nitrogen retention", "Sediment retention", "Endemic species", "Red List species", "Species richness","KBAs")
mats_plot_long$value_revadj<-mats_plot_long$value/mats_plot_long$SALES_USD
mats_plot_long$value_revadj[mats_plot_long$value_revadj<0]<-NA #change to NA where sales is negative to exclude from analysis
mats_plot_long$value_km2<-mats_plot_long$value/mats_plot_long$total_area

#Total impact per company by industry
mat_total<-ggplot(data=mats_plot_long, aes(x=GICS.Industry, y=value+0.0000001))+
  geom_boxplot(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab("Total impact")+
  theme_minimal_hgrid(12)+theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  theme(plot.margin = margin(10,5.5,5.5,10))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

#Revenue-adjusted impact per company by industry
mat_rev_adj<-ggplot(data=mats_plot_long, aes(x=GICS.Industry, y=value_revadj+0.0000001))+
  geom_boxplot(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab("Revenue-adjusted impact")+
  theme_minimal_hgrid(12)+theme(legend.position = "none", strip.text.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  theme(plot.margin = margin(10,5.5,5.5,10))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.0000001, 0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), limits = c(0.0000001, NA), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

#Impact per area (answers question of whether industry/companies tend to operate in higher impact areas)
mat_per_area<-ggplot(data=mats_plot_long, aes(x=GICS.Industry, y=value_km2+0.0000001))+
  geom_boxplot(aes(fill=GICS.Industry))+facet_wrap(facets=vars(impact_metric), scales="free", ncol=7)+ylab(expression(paste("Impact per ", km^2)))+
  labs(fill="Industry")+
  theme_minimal_hgrid(12)+theme(strip.text.x = element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())+
  theme(plot.margin = margin(10,5.5,10,5.5))+ #add space at bottom of graph
  coord_trans(y = "log10")+scale_y_continuous(breaks=c(0.0000001, 0.00001, 0.001,0.1, 10,1000,100000,10000000,1000000000,100000000000), limits = c(0.0000001, NA), expand = c(0,0)) #use coord_trans NOT scale_y_log10, otherwise, will transform first then calculate boxplot stats, which will be wrong!

mat_legend<-get_legend(mat_per_area+theme(legend.justification = "center"))
mat_legend_h<-get_legend(mat_per_area+theme(legend.justification = "center", legend.direction="horizontal"))
mat_per_area<-mat_per_area+theme(legend.position = "none")

mat_plot<-plot_grid(plot_grid(mat_total, mat_rev_adj, mat_per_area, ncol=1, align="v"), mat_legend_h, ncol=1, align="v", rel_heights=c(5,1))

ggsave(file=paste0("materials_plot", "_", getdate(), ".png"), mat_plot,
       width = 14, height = 7, dpi = 300, units = "in", device='png', bg="white")


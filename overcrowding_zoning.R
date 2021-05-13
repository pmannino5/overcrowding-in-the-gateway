#This script begins to examine overcrowding in the Gateway Cities region of Los Angeles. It relates single family zoning to overcrowding by race/ethnicity and then examines the correlation between SFZ and other sociodemographic factors on overcrowding.

#In order to run the script, you need an api key from the census bureau.

library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(tidyr)
library(censusapi)
library(fixest)
library(readr)

#set working directory
setwd('/Users/petermannino/Documents/School/Classes/Practicum/overcrowding_zoning')

##### Spatial Join parcel data to tract data #####

#layers in gdb
layers<-st_layers('data/scag_parcel_data.gdb')

#import LA tract shapefile and limit to LA
la_tracts<-st_read('data/scag_parcel_data.gdb', layer=layers$name[4]) %>%
               filter(STATEFP == '06' & COUNTYFP=='037') 

#convert to projected coordinate system
utm<-'+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0'
la_tracts<-st_transform(la_tracts,utm)


#import LA parcel data, convert crs and set geometry to centroid
gw_land<-st_read('data/scag_parcel_data.gdb', layer=layers$name[1]) %>%
                  st_transform(st_crs(la_tracts)) %>%
                  mutate(centroid=st_centroid(SHAPE)) %>%
                  st_set_geometry('centroid')

#spatial join the tract info to each parcel based on which tract the parcel is in
gw_parcels<-st_join(gw_land,la_tracts,join=st_within) %>% st_drop_geometry()

##### Calculate SF zoning by tract #####

#Import zoning code lookup
zn_code<-read_csv('data/scag_zoning_variables.csv', 
                  col_types=cols(Code=col_character()))

#join zoning codes to parcel data and summarize by tract
zn_tract<-gw_parcels[c('APN','SCAG_ZN_CO','GEOID')] %>% 
          left_join(zn_code, by=c('SCAG_ZN_CO' = 'Code')) %>%
          group_by(GEOID,Category,Type) %>%
          summarize(num_parcels=n()) %>% ungroup()

#filter and summarize to single and multi-family parcels
sf_mf<-zn_tract %>% 
      mutate(Category=ifelse(Category == 'Single Family', 'SingleFamily',Category)) %>%
      filter(Category %in% c('SingleFamily', 'Multi-Family')) %>% 
      select(-Type) %>%
      spread(Category,num_parcels)

#filter and summarize residential and all parcels and join sf_mf
res_tot<-zn_tract %>% 
        group_by(GEOID,Type) %>% 
        summarize(num_parcels=sum(num_parcels)) %>% 
        drop_na(Type) %>%
        mutate(Type=replace(Type, stringr::str_detect(Type,' '),'NonResidential')) %>%
        spread(Type, num_parcels) %>%
        mutate(all_parcels=replace_na(Residential,0) + replace_na(NonResidential,0)) %>%
        select(-NonResidential) %>%
        left_join(sf_mf) %>%
        replace_na(list(Residential = 0, all_parcels = 0, SingleFamily = 0, 'Multi-Family' = 0 ))


##### Load Census data #####

Sys.setenv(CENSUS_KEY="[insert api key]")

#get api list
apis <- listCensusApis()
View(apis)

#show variables in api
vars <- listCensusMetadata(
  name = "acs/acs5", 
  vintage = 2018,
  type = "variables")
View(vars)

#get census variables
variables<-c("B25014_002E", # total owner
             "B25014_005E", # owner crowding
             "B25014_006E", # owner crowding
             "B25014_007E", # owner crowding
             "B25014_008E", # total renter 
             "B25014_011E", # renter crowding
             "B25014_012E", # renter crowding
             "B25014_013E", # renter crowding
             "B25014H_001E", # total white nonhisp
             "B25014H_003E", # white nonhisp crowding
             "B25014B_001E", # total black
             "B25014B_003E", # black crowding
             "B25014I_001E", # total hispanic
             "B25014I_003E", # hispanic crowding
             "B19119_001E", # median family income
             "B19019_001E", # median household income
             "B25064_001E", # median gross rent
             "B25070_007E", # gross rent 30-35% income
             "B25070_008E", # 35-40
             "B25070_009E", # 40-50
             "B25070_010E", # >50
             "B25070_001E", # all units
             "B03002_001E", # total pop
             "B03002_003E", # nonhisp white
             "B03002_004E", # nonhisp black
             "B03002_006E", # nonhisp asian
             "B03002_012E", # hispanic
             "C17002_001E", # total inc ratio
             "C17002_002E", # earning half poverty level
             "C17002_003E") # .5 to 1 of poverty level
             
#get data from api
census_df<-getCensus(name='acs/acs5',
                     vintage = 2018, 
                     vars = variables, 
                     region = "tract:*", 
                     regionin = "state:06+county:037")

#calculate overcrowding, rent burden and demographic metrics
census_df<- census_df %>%
            mutate(white_overcrowding=B25014H_003E/B25014H_001E,
                   black_overcrowding=B25014B_003E/B25014B_001E,
                   hispanic_overcrowding=B25014I_003E/B25014I_001E,
                   owner_overcrowding=(B25014_005E+B25014_006E+B25014_007E)/B25014_002E,
                   renter_overcrowding=(B25014_011E+B25014_012E+B25014_013E)/B25014_008E,
                   rent_burden=(B25070_007E+B25070_008E+B25070_009E+B25070_010E)/B25070_001E,
                   pct_white=B03002_003E/B03002_001E,
                   pct_black=B03002_004E/B03002_001E,
                   pct_asian=B03002_006E/B03002_001E,
                   pct_hispanic=B03002_012E/B03002_001E,
                   pct_poverty=(C17002_002E+C17002_003E)/C17002_001E)

#generate GEOID
census_df$GEOID <- stringr::str_c(census_df$state,census_df$county, census_df$tract, sep="")

##### Merge census and zoning data and plot #####

#merge zoning and census data

cols<-c('GEOID','white_overcrowding','black_overcrowding','hispanic_overcrowding',
'owner_overcrowding','renter_overcrowding','rent_burden','pct_white',
'pct_black','pct_asian','pct_hispanic','pct_poverty','B19019_001E','B25064_001E')

sf_census_df<-res_tot %>% 
                 left_join(census_df[cols]) %>%
                 rename(med_hh_inc=B19019_001E, med_rent_price=B25064_001E) %>%
                 mutate(med_hh_inc=med_hh_inc/1000,
                        percent_sf=SingleFamily/all_parcels)


#make scatterplots with best fit lines for overcrowding and zoning

#the columns
colnames<-names(sf_census_df)[6:10]

#title of each graph
titles<-c('White Overcrowding','Black Overcrowding','Hispanic Overcrowding','Owner Overcrowding','Renter Overcrowding')

#list to hold plots
plots<-list()

#loop to create plots of sfz and overcrowding
for (i in 1:5) {

  p1<-ggplot(sf_census_df,aes_string(x='percent_sf', y=colnames[i])) + 
    geom_point() + 
    geom_smooth(method=lm) + 
    labs(x="Percent Zoned For SF", y='Overcrowding', title=titles[i])

  plots[[i]]<-p1
}

#Arrange plots in one figure
grph_grid<-ggarrange(plotlist=plots,ncol=3,nrow=2)

grph_grid #show grid

#save plots on one page
ggsave("overcrowding_plots.jpg",plot=grph_grid,width=8.5, height=7)


# plot other demographic correlates of overcrowding
ggplot(sf_census_df, aes_string(x='rent_burden',y='renter_overcrowding')) +
  geom_point() + 
  geom_smooth(method=lm)

# plot sf policy correlate
ggplot(sf_census_df, aes_string(x='percent_sf',y='pct_white')) +
  geom_point() + 
  geom_smooth(method=lm) +
  coord_cartesian(xlim =c(-.01, 1.03), ylim = c(0, 1), expand=FALSE) +
  labs(x='Percent Single Family Zoned',y='Percent White',title='Zoning and Race') +
  theme_pubr()

# Median Household Income and Rent Burden
ggplot(sf_census_df %>% filter(med_hh_inc>0), aes_string(x='med_hh_inc',y='rent_burden')) +
  geom_point() + 
  geom_smooth(method=lm) +
  labs(x='Median Household Income (in Thousands)',y='Rent Burden',title='Income and Housing') +
  scale_x_continuous(breaks=seq(0,200,20)) +
  theme_stata()

##### Link tract to gateway city #####

#import shp with tract to city lookup
gw_cities<-st_read('data/scag_parcel_data.gdb', layer=layers$name[5]) %>% select(GEOID, CITY, CITY_ID)

#merge city names into tract data
sf_census_city_df<- sf_census_df %>%
                            left_join(gw_cities %>% st_drop_geometry())

#summarize single family zoning by city
city_sf<-sf_census_city_df[c('CITY','SingleFamily','all_parcels')] %>% 
                              group_by(CITY) %>% 
                              summarize(sf=sum(SingleFamily), allp=sum(all_parcels)) %>% 
                              mutate(sf_share=sf/allp)

#merge city level single family zoning into tract data
sf_census_city_df<-sf_census_city_df %>%
                            left_join(city_sf)

##### Regression Model on determinants of overcrowding #####

#model - main explanatory variable is tract percent sfz, includes city fixed effects
model<-feols(renter_overcrowding ~  percent_sf + med_hh_inc + med_rent_price + pct_black + pct_hispanic + pct_asian + rent_burden | CITY, data=sf_census_city_df) 

#summary
summary(model)

#results show that a positive coefficient on single family zoning, so that SFZ is associated with increased renter overcrowding
#however, the p-value is .09

##### Maps #####

#merge tracts with census data and remove islands
islands<-c('06037599100','06037599000')

columns<-c('GEOID','white_overcrowding','black_overcrowding','hispanic_overcrowding',
        'owner_overcrowding','renter_overcrowding','rent_burden','pct_white',
        'pct_black','pct_asian','pct_hispanic','pct_poverty','med_hh_inc','med_rent_price', 'percent_sf')


la_gdf<-la_tracts %>%
        filter(ALAND>0, !GEOID %in% islands) %>%
        left_join(sf_census_df[columns])

#Get gateway cities outline
gateway<-st_read('data/scag_parcel_data.gdb', layer=layers$name[3]) %>%
  st_transform(utm)
gateway<-gateway[3:10,] # remove islands


#get map limits
gateway_bbox<-gateway %>% st_bbox()
xlim<-c(gateway_bbox['xmin']-7000,gateway_bbox['xmax'])
ylim<-c(gateway_bbox['ymin']-1000,gateway_bbox['ymax']+9000)

#overcrowding maps with gateway outline, zoom into gateway city region

for (i in 1:5) {

  print(ggplot(data=la_gdf) + geom_sf(aes_string(fill=colnames[i]), size=.1) +
  geom_sf(data=gateway, fill=NA,size=1,color='black') +
  scale_fill_viridis_c(limits=c(0,1)) + 
  theme_bw() +
  labs(title=titles[i], fill=' ') + 
  coord_sf(xlim=xlim, ylim=ylim) + 
    theme(panel.background = element_rect(fill='aliceblue')))
}

#rent burden map
ggplot(data=la_gdf) + geom_sf(aes_string(fill='rent_burden'), size=.1) +
  geom_sf(data=gateway, fill=NA,size=1,color='black') +
  scale_fill_viridis_c(limits=c(0,1)) + 
  theme_bw() +
  labs(fill=' ',title='Rent Burden in Gateway') + 
  coord_sf(xlim=xlim, ylim=ylim) +
  theme(panel.background = element_rect(fill='aliceblue'))

#sigle_fam_zoning map
ggplot(data=la_gdf) + geom_sf(aes_string(fill='percent_sf'), size=.1) +
  geom_sf(data=gateway, fill=NA,size=1,color='black') +
  scale_fill_viridis_c(limits=c(0,1)) + 
  theme_bw() +
  labs(fill=' ',title='Single Family Zoning in Gateway') + 
  coord_sf(xlim=xlim, ylim=ylim) +
  theme(panel.background = element_rect(fill='aliceblue'))













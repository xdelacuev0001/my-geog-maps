# Xenia Dela Cueva
# Mapping Schools in Los Angeles


library(sf)     
library(terra)   
library(dplyr) 
library(sp)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)  
library(leaflet)
library(ggplot2)



#income per capita, based on LA County
la_icp = st_read("/Users/xeniadelacueva/downloads/Income_per_Capita_(census_tract)/Income_per_Capita_(census_tract).shp")
#plot(la_icp)
#View(la_icp)


#demographics for city of Los Angeles
la_gen_dem = st_read("/Users/xeniadelacueva/downloads/Los_Angeles_Demographics_2018/LAcityDemo.shp")
la_race_dem_SF = la_gen_dem[,c(2,6,20:23,48:56)] #keeping rsacial demographics, census tract, and total pop
#creating columns for total pop for ages 0-19 (for kids and education), white, non-white
mut_la_race_dem_SF = la_race_dem_SF %>%
  dplyr::mutate(pop_under20 = (POP0_CY + POP5_CY+ POP10_CY+ POP15_CY),
                pop_under20_dens = (POP0_CY + POP5_CY+ POP10_CY+ POP15_CY)/ TOTPOP_CY,
                white_pop_dens = (NHSPWHT_CY/TOTPOP_CY) * 100 ,
                min_pop = ((HISPPOP_CY +NHSPBLK_CY + NHSPAI_CY + NHSPASN_CY + NHSPPI_CY +NHSPOTH_CY + NHSPMLT_CY)/ TOTPOP_CY)* 100)

la_race_dem=st_drop_geometry(mut_la_race_dem_SF) # dropping sticky geometry
View(la_race_dem)

dev.off()

#demographics and income per capita together, 
#since demographics is only based in City of Los Angeles,
#will cut out all rows that don't have info for demographic columns, so my map is based solely on
#city of Los Angeles, not LA County
la_icp_dem0= left_join(la_icp, la_race_dem, by = c(tract = "ID") )
#View(la_icp_dem0)
la_icp_dem00 = la_icp_dem0[!is.na(la_icp_dem0$HISPPOP_CY), ] #this is just arbitary col
la_icp_dem1 = la_icp_dem00[!is.na(la_icp_dem00$income_per), ]
#that I used to get rid of census tracts in LA County not in Los Angeles City
plot(st_geometry(la_gen_dem))
plot(la_icp_dem1, add = TRUE)
dev.off()



#public schools in Los Angeles
public_schools0 = st_read("/Users/xeniadelacueva/downloads/School_Campuses_(LAUSD)/School_Campuses_(LAUSD).shp")
public_schools= public_schools0[,7] %>%
  dplyr::rename(Name = LABEL)
public_schools$cat2 = "Public School"
#plot(public_schools0)
#View(public_schools0)
View(public_schools)
#private schools in Los Angeles
priv_char_schools0 = st_read("/Users/xeniadelacueva/downloads/Private_and_Charter_Schools/Private_and_Charter_Schools.shp")
priv_char_schools = priv_char_schools0[, c(5,8)]
View(priv_char_schools)

#combining both public and private/charter schools,
la_schools = rbind(public_schools, priv_char_schools)
#View(la_schools)


#filtering out schools within our interested census tracts
#creating new area using geometries of all our interested census tracts
new_schools = st_filter(la_schools, la_icp_dem1)
#View(new_schools)
total_schools_perTract = aggregate(new_schools, la_icp_dem1, length)

#getting the proportion of academic resources per census tract population for people 1-19
View(total_schools_perTract)

la_icp_dem1 = cbind(la_icp_dem1, schools = total_schools_perTract$cat2)
View(la_icp_dem1)

#making sure all census tracts I look at has at least one academic resource,
# I chose not to change it to just 0 since I am looking at tracts with schools and there are a lot
#more tracts without schools and a lot of the reasons why is because it is a residential place
# and I am interested in the demographics of the census tracts with little/most densities of schools
#la_icp_dem1[is.na(la_icp_dem1)] = 0
#View(la_icp_dem1)

la_icp_dem1 = la_icp_dem1[!is.na(la_icp_dem1$schools), ]  

#schools_perTract = aggregate(la_icp_dem1, pub_schools_la, length)
View(la_icp_dem1)

# Plotting out General Academic Densities in Most LA census tracts that had at least one school
# in it
academics_la = la_icp_dem1 %>%
  dplyr::mutate(school_density = (schools/ pop_under20)* 100) #%>%
  #filter(dense_rank(ac_res_density) <= 20 | dense_rank(desc(ac_res_density)) <= 20) 
View(academics_la)


#this is my initial attempt to plot out academic densities when I didn't have tmaps
# I had to manually create the legend since it wouldn't show if added on top of each other
dev.off()
plot(la_gen_dem$geometry)
title("Academic Resource Densities in LA")
plot(academics_la["school_density"], add = TRUE)
legend("bottomleft",  title = "Percent", col =c("blue4",  "blue", "darkviolet", "deeppink2", "darkorange1", "darkgoldenrod1", "yellow"), pch =15, legend= c(0, 1.0, 1.5, 2.0, 2.5, 3.0, "3+"), cex = 0.75)

dev.off()
#I was able to get tmaps by redownloaded the most updated version of R in my computer
tm_academics = tm_shape(la_gen_dem) + tm_polygons(col ="school_density")
tm_academics2 = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(academics_la) +
  tm_polygons(col ="school_density", title = "Percent") + 
  tm_layout(legend.position = c("right", "top"), legend.text.size = 0.3, legend.title.size = 0.5 ,title ="School Densities in \n Los Angeles By Census Tract", title.position = c("left", "bottom"), title.size = 0.7)
tm_academics2



#Plotting out the incomes_per for census tracts with at least one school
topincome_la = academics_la%>%
  dplyr::mutate(ac_res_density = (schools/ pop_under20)* 100) %>%
  top_n(50, wt = income_per)
  #filter(dense_rank(ac_res_density) <= 20 | dense_rank(desc(ac_res_density)) <= 20) 

botincome_la = academics_la %>%
  dplyr::mutate(ac_res_density = (schools/ pop_under20)* 100) %>%
  top_n(-50, wt = income_per)


#both the top and bottom incomes in one data frame
ext_incomes = academics_la  %>%
  filter(dense_rank(income_per) <= 50 | dense_rank(desc(income_per)) <= 50) 
View(ext_incomes)

#filtering out the private schools in the extreme income tracts
ext_schools = st_filter(la_schools, ext_incomes)
View(ext_schools)

#plotted out richest (warm colors) and poorest (blue) census tracts and plotted all their school densities
# Ichose the school density instead of school counts because census tracts can have more or less people
#and I wanted to show the academic resources based on its population

#based on school density in tract
tm_census_income2 = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(topincome_la) + 
  tm_polygons(col ="income_per", n = 3, title = "Income per Capita") + tm_shape(botincome_la) + 
  tm_polygons(col ="income_per", palette= "Blues",n = 3, title = "") + tm_layout(legend.title.size = 0.6, legend.text.size = 0.4) +
  tm_shape(ext_incomes) + tm_symbols(col = "black", size = "school_density") + 
  tm_layout(title ="Schools Densities Based on                   \n Richest & Poorest Tracts in LA  ", title.position = c("right", "top"), title.size = 0.5)
tm_census_income2 

#based on number of schools in tract
tm_census_income3 = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(topincome_la) + 
  tm_polygons(col ="income_per", n = 3, title = "Income per Capita") + tm_shape(botincome_la) + 
  tm_polygons(col ="income_per", palette= "Blues",n = 3, title = "") + tm_layout(legend.title.size = 0.6, legend.text.size = 0.4) +
  tm_shape(ext_incomes) + tm_symbols(col = "black", size = "schools") + 
  tm_layout(title ="Schools Based on Richest             \n & Poorest Tracts in LA  ", title.position = c("right", "top"), title.size = 0.6)
tm_census_income3 


#Schools Based on Race
View(la_icp_dem1)
#plotting out not white race
tm_tract_wRace = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(academics_la) + tm_polygons(col ="white_pop_dens", title = "Percent of White Pop")
tm_tract_wRace

#Mapping both minority percents and school densities
tm_cesus_nwRace = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(academics_la) + 
  tm_polygons(col ="min_pop", title = "Minority Percent", palette = "Greens") +
  tm_symbols(col = "black", size = "school_density") +
  tm_layout(title ="School Densities with        \n Minority Populations", title.position = c("right", "top"), title.size = 0.6, legend.title.size = 0.6, legend.text.size = 0.4)
tm_cesus_nwRace
View(academics_la)
#most diverse tracts
top_div = academics_la  %>%
  top_n(-50, wt = min_pop) 
View(top_div)


#I ended up not using the bottom code
#least diverse tracts 
least_div = academics_la  %>%
  top_n(-50, wt = min_pop) 
View(least_div)

#both the top and bottom minority pops
ext_rac_demog = academics_la  %>%
  filter(dense_rank(min_pop) <= 50 | dense_rank(desc(min_pop)) <= 50) 
View(ext_rac_demog)

schools_div = tm_shape(la_gen_dem) + tm_polygons() + tm_shape(ext_rac_demog) + 
  tm_polygons(col ="min_pop", title = "Minority Percent", palette = "Greens") +
  tm_symbols(col = "black", size = "school_density") + 
  tm_layout(title ="Schools Densities Based with                   \n  Diversity", title.position = c("right", "top"), title.size = 0.5)
schools_div


tm_cesus_nwRace

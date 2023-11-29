
joined_data <- inner_join(WorkplaceRights_2017, TUDR_2017, by = c("ref_area", "time"))

# View the structure of the combined data
str(joined_data)
View(joined_data)

joined_data2017 <- inner_join(joined_data, CBCR_2017, by = c("ref_area","time"))

View(joined_data2017)


joined_data2017 <- joined_data2017 %>% 
  rename("National_Compliance_wth_Labour_Rights" = obs_value.x,
         "Union Density"  = obs_value.y,
         "Collective Bargaining Coverage"   = obs_value
  )

joined_data2017 %>% 
  select(National_Compliance_wth_Labour_Rights,`Collective Bargaining Coverage`)


#full joined data is below
joined_data4 <- inner_join(WorkplaceRights, TUDR, by = c("ref_area", "time"))
joined_full_data_set <- inner_join(joined_data4, CBCR, by = c("ref_area","time"))

joined_full_data_set <- joined_full_data_set %>% 
  rename("National_Compliance_wth_Labour_Rights" = obs_value.x,
         "Union Density" = obs_value.y,
         "Collective Bargaining Coverage"    = obs_value
  )


TradeUnionDensity <- TradeUnionDensity %>% 
  rename("Year" = Time)

count_CollectiveBargaining

OldData <- inner_join(TradeUnionDensity, CollectiveBargaining, by =c("Country","Year"))

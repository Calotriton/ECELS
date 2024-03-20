ECELS <- function(data) {
  print("Starting calculation.")

  # Check the structure of the input data
  print("Data structure:")
  print(str(data))

  # Component 1: Pond morphology
  component1 <- rowSums(
    cbind(
      ifelse(data$Slope < 25, 20, ifelse(data$Slope < 50, 10, ifelse(data$Slope < 75, 5, 0))),
      ifelse(data$Weirs == "Soil", -5,
             ifelse(data$Weirs %in% c("Concrete", "Plastic", "Rocks"), -10, 0)),
      ifelse(data$Burial == "Buried", -10, 0),
      ifelse(data$Imp_liner == "Impermeable", -15, 0)
    )
  )
  component1[component1 < 0] <- 0
  component1[component1 > 20] <- 20

  print("Component 1 calculated.")


  # Component 2: Human activity
  component2 <- rowSums(
    cbind(
      ifelse(data$Hydr_infrastr == "Presence", 0, 5),
      ifelse(data$Road == "Paved", 0, ifelse(data$Road == "Unpaved", 3, 5)),
      ifelse(data$CS_WTP_F_IP == "Presence", 0,
             ifelse(data$GCF == "Presence", 2,
                    ifelse(data$Area < 0.5,
                           ifelse(data$Other_buildings == 0, 5,
                                  ifelse(data$Other_buildings <= 10, 0, 0)),
                           ifelse(data$Area <= 5,
                                  ifelse(data$Other_buildings == 0, 5,
                                         ifelse(data$Other_buildings <= 10, 1, 0)),
                                  ifelse(data$Other_buildings == 0, 5,
                                         ifelse(data$Other_buildings <= 10, 3, 0)))))),
      ifelse(data$A_L_P == "Inside", 0,
             ifelse(data$A_L_P == "Littoral", 1,
                    ifelse(data$A_L_P == "Around", 3, 5))),
      ifelse(data$Freq_people == "High", -5,
             ifelse(data$Freq_people == "Medium", -3, 0)),
      ifelse(data$Vis_rubish == "Water", -5,
             ifelse(data$Vis_rubish == "Around", -3, 0)),
      max(
        ifelse(data$IP_H_LO == "Presence", 1, 0),
        ifelse(data$PA == "Protected", 3, 0),
        ifelse(data$Management == "Managed", 5, 0),
        ifelse(data$Dom_fauna == "Presence", -5, 0)
      )
    )
  )
  component2[component2 < 0] <- 0
  component2[component2 > 20] <- 20

  print("Component 2 calculated.")


  # Component 3: Water aspects
  component3 <- rowSums(
    cbind(
      ifelse(data$Transparency < 5, 5, ifelse(data$Transparency < 15, 2, 0)),
      ifelse(data$Odour == "Absence", 5, 0),
      ifelse(data$Nat_Turb == "Presence", 5, 0)
    )
  )
  component3[component3 < 0] <- 0
  component3[component3 > 10] <- 10

  print("Component 3 calculated.")


  # Component 4: Emergent vegetation
  component4 <- rowSums(
    cbind(
      ifelse(data$Emerse_per < 25, 5,
             ifelse(data$Emerse_per < 90, 10, 15)),
      ifelse(data$Emerse_pond < 25, 15,
             ifelse(data$Emerse_pond < 50, 10,
                    ifelse(data$Emerse_pond < 90, 5, 0))),
      ifelse(data$Dominant_com == "Giant_reed", -10,
             ifelse(data$Dominant_com == "Common_reed", -5,
                    ifelse(data$Dominant_com == "Multispecific", 10,
                           ifelse(data$Dominant_com == "Exotic", -10, 0)))),
      ifelse(data$Shrub_stratum == "Aut_iso", 5,
             ifelse(data$Shrub_stratum == "Aut_comp", 10,
                    ifelse(data$Shrub_stratum == "Allo_iso", -5,
                           ifelse(data$Shrub_stratum == "Allo_comp", -10,
                                  ifelse(data$Shrub_stratum == "Plantation", -10, 0))))),
      ifelse(data$Water_perm == "Temporary", 15,
             ifelse(data$Water_perm == "Permanent" | data$Water_perm == "Semipermanent", 10, 0))
    )
  )
  component4[component4 < 0] <- 0
  component4[component4 > 30] <- 30

  print("Component 4 calculated.")

  # Component 5: Hydrophytic vegetation
  component5 <- rowSums(
    cbind(
      ifelse(data$Root_veg == 0, 0,
             ifelse(data$Root_veg < 25, 5,
                    ifelse(data$Root_veg < 90, 10, 15))),
      ifelse(data$Nonroot_veg == 0, 0,
             ifelse(data$Nonroot_veg < 25, 3,
                    ifelse(data$Nonroot_veg < 90, 5, 0))),
      ifelse(data$Dom_plants_algae %in% c("Plants_Charo", "Similar"), 10,
             ifelse(data$Dom_plants_algae %in% c("Algae", "Duckweed"), -5,
                    ifelse(data$Dom_plants_algae == "Allochthonous", -10, 0)))
    )
  )
  component5[component5 < 0] <- 0
  component5[component5 > 20] <- 20

  print("Component 5 calculated.")

  # Total ECELS calculation
  ecels_index <- rowSums(cbind(component1, component2, component3, component4, component5))

  print("ECELS index calculated successfully.")

  # Categorization
  category <- cut(ecels_index, breaks = c(-Inf, 30, 49, 69, 89, Inf),
                  labels = c("Bad", "Poor", "Moderate", "Good", "High"))

  # Create result data frame
  result <- data.frame(
    ID = data$Pond_ID,
    ECELS_Index = ecels_index,
    Category = category,
    Component1 = component1,
    Component2 = component2,
    Component3 = component3,
    Component4 = component4,
    Component5 = component5
  )

  print("Print the resulting object or export it to a .csv for its visualization")

  return(result)
}


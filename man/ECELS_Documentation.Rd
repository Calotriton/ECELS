\name{ECELS}

\alias{ECELS}

\title{Calculating ECELS: Conservation Status of Mediterranean Wetlands}

\description{Obtains the ECELS value, category, and the values of each of its
components, from a dataset that includes multiple ponds as rows and the required
variables to consider as columns. Based on Sala et al. (2004)
<doi:10.5169/seals-738445>}

\usage{
ECELS(data)
}

\arguments{
  \item{data}{A data frame containing the variables required for calculating the
  ECELS index.}
}

\details{
It is mandatory for this package to work to follow the protocol for the proper
calculation for this index. Refer to the references section to find the
peer-reviewed paper on which this package is based on.

For this package to work, you should input a dataframe, which can be just a
direct table from Excel (properly formated), or a data.frame object from R.
The structure should be:
- First column: pond name. Must be "Pond_ID"
- First row: variable names. Specified next.
- Each row: each pond, with the values for each variable.
- Each column: each variable values.

Moreover, it is mandatory that the names of the variables to consider, and their
possible values, follow exactly the following list:

Component 1:
  -	“Slope”: Littoral slope (0 to 100)
  - “Weirs”: Presence of weirs, levees or embankments. Can be “Soil”, “Concrete”
    , “Plastic”, “Rocks” or “Absence”.
  -	“Burial”: Evidence of burial of the wetland. Can be “Not_buried”, or
    “Buried”.
  -	“Imp_liner”: The bottom of the wetland is covered with an impermeable liner.
    Can be “Permeable”, or “Impermeable”.

Component 2:
  -	“Hydr_infrastr”: Hydraulic infrastructures related to water uses of the
    wetland. Can be “Absence” or “Presence”.
  - “Road”: Transport infrastructures at less than 100 m. Can be “Absence”,
    “Unpaved” or “Paved”.
  - “CS_WTP_F_IP”: Presence of campsites, wastewater treatment plants, farms,
    industrial plants, etc. Can be “Presence” or “Absence”.
  - “GCF”: Presence of golf course facilities. Can be “Presence” or “Absence”.
  - “Other_buildings”: Presence of buildings. It will be a number from 0 to 10.
    To be determined according to the wetland surface (“Area”).
  - "Area": Must be specified in hectares.
  -	“A_L_P”: Agricultural uses, livestock or plantations. Can be “Absence”,
    “Around”, “Littoral”, or “Inside”.
  -	“Freq_people”: Frequency of visits in the wetland. It can be “Never”,
    “Medium”, or “High”.
  -	“Vis_rubish”: Visible rubbish in the pond. It can be “Absence”, “Water”, or
    “Around".
  - “IP_H_LO”: Informative panels, hides or lookouts. Can be “Absence” or
    “Presence”.
  - “PA”: Protected area. Can be “Protected” or “Unprotected”.
  - “Management”: Managed pond. Can be “Managed” or “Unmanaged”.
  -	“Dom_fauna”: Presence of allochthonous or domestic fauna. Can be “Presence”
    or “Absence”.

Component 3:
  -	“Transparency”: water transparency, measured in NTUs.
  - “Odour”: odour of the water. Can be “Presence” or “Absence”.
  -	“Nat_Turb”: water turbidity of natural origin. It can be “Presence” or
    “Absence”.

Component 4:
  -	“Emerse_per”: Cover of the vegetation in the wetland perimeter. Value from
    0 to 100.
  -	“Emerse_pond”: Cover of the vegetation inside wetland. Value from 0 to 100.
  -	“Dominant_com”: dominance of the vegetation by multiple groups. It can be
    “Giant_reed”, “Common_reed”, “Multispecific”, or “Exotic”.
  -	“Shrub_stratum”: at less than 10m from the wetland. It can be “Aut_iso”,
    “Aut_comp”, “Allo_iso”, “Allo_comp”,  “Plantation”, or “Absence”.
  -	“Water_perm”: Water permanence. It can be “Temporary”, “Permanent”, or
    “Semipermanent”.

Component 5:
  - “Root_veg”: Quantity of  submersed or rooted floating-leaf vegetation. Value
    from 0 to 100.
  -	“Nonroot_veg”: Quantity of non-rooted floating vegetation on the surface of
    the wetland. Value from 0 to 100.
  -	“Dom_plants_algae”: Communities of hydrophytic or floating-leaf vegetation.
    Can be “Plants_Charo”, “Similar”, “Algae”, “Duckweed”, or  “Allochthonous”.

The function will print a message once each component, and the final ECELS
value, are calculated. If it does not appear, some problem might be occurring
on that certain component. Be sure that the names follow the instructions,
otherwise it won't work properly.

}
\value{
Returns a dataframe that includes each assessed pond, its ECELS value, category,
and the value for each of its components.

The function can be applied to create an object that can be printed, exported as
a .csv, or worked on in another ways all dataframe related.
}
\references{
Sala, J., Gascón, S., Boix, D., Gesti, J. and Quintana, X.D. (2004). Proposal of
a rapid methodology to assess the conservation status of Mediterranean wetlands
and its application in Catalunya (NE Iberian Peninsula). Arch. Sci. 57: 141-152.
}
\author{
Alejandro López-de Sancha.
}
\note{
Don't hesitate to contact me for any doubts or issues that might arise!
email: 93lopez.alejandro@gmail.com
}

\examples{

#Example dataset, can be created or imported from Excel:

data <- data.frame(
  Pond_ID = c("Pond1", "Pond2", "Pond3", "Pond4", "Pond5", "Pond6"),
  Slope = c(20, 20, 20, 20, 35, 20),
  Weirs = c("Absence", "Absence", "Absence", "Absence", "Absence", "Absence"),
  Burial = c("Not_buried", "Not_buried", "Not_buried", "Not_buried",
  "Not_buried", "Not_buried"),
  Imp_liner = c("Permeable", "Permeable", "Permeable", "Permeable", "Permeable",
  "Permeable"),
  Hydr_infrastr = c("Absence", "Presence", "Absence", "Absence", "Absence",
  "Absence"),
  Road = c("Unpaved", "Unpaved", "Unpaved", "Unpaved", "Unpaved", "Unpaved"),
  CS_WTP_F_IP = c("Absence", "Absence", "Absence", "Absence", "Absence",
  "Absence"),
  GCF = c(0, 0, 0, 0, 0, 0),
  Other_buildings = c(0, 0, 0, 0, 0, 0),
  Area = c(0.0882, 0.099, 0.425, 0.0336, 0.0702, 0.018),
  A_L_P = c("Inside", "Inside", "Around", "Around", "Inside", "Around"),
  Freq_people = c("Medium", "Medium", "Never", "Never", "Medium", "Medium"),
  Vis_rubish = c("Absence", "Absence", "Absence", "Absence", "Absence",
  "Absence"),
  IP_H_LO = c("Protected", "Protected", "Unprotected", "Unprotected",
  "Unprotected", "Protected"),
  PA = c("Unmanaged", "Unmanaged", "Unmanaged", "Unmanaged", "Unmanaged",
  "Unmanaged"),
  Management = c("Absence", "Absence", "Absence", "Presence", "Presence",
  "Absence"),
  Dom_fauna = c(0.701333333, 2.794333333, 8.067666667, 19.68333333, 1.423,
  6.493333333),
  Transparency = c("Absence", "Absence", "Absence", "Presence", "Presence",
  "Absence"),
  Odour = c("Presence", "Absence", "Presence", "Presence", "Absence",
  "Presence"),
  Nat_Turb = c(15, 0, 90, 20, 2, 0),
  Emerse_per = c(55, 75, 55, 50, 95, 0),
  Emerse_pond = c(0, 0, 95, 0, 20, 0),
  Dominant_com = c("Plants_Charo", "Plants_Charo", "Plants_Charo",
  "Plants_Charo", "Plants_Charo", "Plants_Charo"),
  Shrub_stratum = c("Aut_iso", "Aut_iso", "Absence", "Aut_comp", "Aut_iso",
  "Absence"),
  Water_perm = c("Temporary", "Permanent", "Temporary", "Temporary",
  "Permanent", "Temporary"),
  Root_veg = c(55, 50, 55, 50, 95, 0),
  Nonroot_veg = c(0, 0, 95, 0, 20, 0),
  Dom_plants_algae = c("Plants_Charo", "Plants_Charo", "Plants_Charo",
  "Plants_Charo", "Plants_Charo", "Plants_Charo")
)

}

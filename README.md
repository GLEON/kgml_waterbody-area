
<!-- README.md is generated from README.Rmd. Please edit that file -->

# realsat

## Vector fields

ID: the unique ID for water body. ID values are used as names of
shapefiles that contain monthly shapes.

RESERVOIR: categorizes water bodies into two sets - 1 represents
reservoirs manually verified by visual validation, and 0 represents
other water bodies. Note that the reservoir list is not exhaustive and
water bodies with 0 value could be reservoirs. The reservoir subset was
created using a machine learning methodology. Please refer to
<http://umnlcc.cs.umn.edu/realsat/reservoirs/data/ReaLSAT-R-2.0.pdf> for
more details.

TAG: categorizes the water bodies into two sets - 1 represents water
bodies for which changes made by the ORBIT algorithm (correcting
erroneous labels in GSW dataset and filling missing labels) are more
reliable. Please refer to the paper
(<https://www.cs.umn.edu/sites/cs.umn.edu/files/tech_reports/20-002.pdf>)
for more details.

AREA: number of LANDSAT 30-m pixels covered by the water body. This area
was calculated by counting pixels in a water body that exists as water
atleast 10 % of the time.

Hylak\_id: The water body ID based on the HydroLAKES dataset. Water
bodies that are not present in HdyroLAKEs but are available in ReaLSAT
have ID as 0.

Hylak\_frc: % overlap between ReaLSAT and HydroLAKES database.

CONTINENT: between 0 and 8 representing different continents -

0:Other 1:Asia 2:North America 3:Europe 4:Africa 5:South America
6:Oceania 7:Australia 8:Antarctica

geometry: reference shape of the reservoir

-----

## Links

<http://umnlcc.cs.umn.edu/realsat/>

<http://umnlcc.cs.umn.edu/realsat/data/ReaLSAT.html>

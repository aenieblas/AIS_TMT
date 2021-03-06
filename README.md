# AIS_TMT
Anne-Elise Nieblas
11/11/2019

This script prepares, filters, and uses raw Exact Earth AIS data to calculate the fishing activity for trawlers within an area of interest. This script was created as part of a capacity-development workshop for AIS use in fisheries management and monitoring in the SRFC of West Africa. Codes were presented at the workshop as an example of how to calculate fishing activity for trawlers using R. These codes use specifically ExactEarth data and investigate potential fishing in the Guinea Bissau EEZ and Joint Marine Area with Senegal. These codes can be adapted to alternate areas of interest with some understanding of R.

# Description: 
This script prepares, filters, and uses raw AIS data to calculate the fishing activity within an area of interest following 8 steps:

                A. Customisation of path, filenames, processing options                 
                1. Set up working environment 
                2. Load and set up data 
                3. Vessel verification
                4. Data quality check
                5. Define “Speed Rule”
                6. Identify probable fishing events based on the Speed Rule
                7. Effect of spatial resolution on calculations of fishing activity
                8. Explore spatio-temporal patterns of fishing activity

# Inputs : 
## AIS data

Raw Exact Earth AIS data in the format as extracted by Trygg Mat Tracking (TMT). These data should be stored in the data/raw_AIS folder, which is currently empty, as data are sensitive. :

    col 1 : MMSI
    col 2 : Timestamp
    col 3 : Name
    col 4 : CallSign
    col 5 : ImoNumber
    col 6 : Length
    col 7 : Breadth
    col 8 : DeviceClass
    col 9 : ShipType
    col 10: Destination
    col 11: Draught
    col 12: Latitude
    col 13: Longitude
    col 14: SOG (Speed over Ground)
    col 15: COG (Course over Ground to the next waypoint)
    col 16: NavStatus (0 = under way using engine; 1 = at anchor; 2 = not under command; 3 = restricted maneuverability;

## shapefiles of the region(s) of interest. 

Here, EEZ of Guinea Bissau and the Joint Marine Area of Senegal and Guinea Bissau


#  Instructions : 

Use AIS_fishing_activity.R to launch and customise scripts. 

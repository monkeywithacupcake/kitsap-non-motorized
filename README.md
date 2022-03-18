## Kitsap Non-Motorized

This is a project related to the bicycle parking map but with a completely different purpose. 

At the moment, it is in exploratory stages

I am not uploading the `/data` folder in my directory. 
If you wish to use this project, download the most current data from [Kitsap County GIS](https://www.kitsapgov.com/dis/Pages/resources.aspx) 


### Data Dictionaries (back created in cleaning files)

**sidewalks**
|col        |def                             |vals ( many if > 10 unique )                                                |
|:----------|:-------------------------------|:---------------------------------------------------------------------------|
|SURFTYPE   |Surface Type                    |NA, Thickened Edge                                                          |
|ADACOMPLY  |ADA Compliant Sidewalk          |Yes, NA                                                                     |
|CONDITION  |Condition of Sidewalk           |NA                                                                          |
|WIDTH      |Width of Sidewalk Surface       |5, 3, 0, 4, 8                                                               |
|INSTALLDAT |Not Used                        |NA                                                                          |
|OWNEDBY    |Not Used                        |0                                                                           |
|MAINTBY    |Not Used                        |0                                                                           |
|SIDEWALKID |Unique Id for Sidewalk          |many                                                                        |
|MODIFIED_B |who modified data               |ABRADFOR, kitsap\s-ags-pwroads                                              |
|MODIFIED_D |date of modification of data    |18596, 18584, 18734, 18971, 18935                                           |
|CREATED_BY |who created data                |ABRADFOR, NA, kitsap\s-ags-pwroads                                          |
|CREATED_DT |date of creation of data        |18183, 18311, NA, 18099, 18241, 18734, 18970                                |
|ROADNAME   |Name used for road              |many                                                                        |
|ROADLOGID  |Road Log Id - from Public Works |many                                                                        |
|LENG_FT    |Distance of segment, in Feet    |many                                                                        |
|EDGETYPE   |Edge of sidewalk format         |NA, Rolled, Thickened Edge, Curb & Gutter, GRV, Unknown, Vertical Curb, HMA |
|DISTRICT   |Commissioner District           |South, Central, North                                                       |
|DISTRICT_S |Not Used                        |NA                                                                          |
|SEG_ID     |Unique ID for Road Segment      |0, 1, NA                                                                    |
|BMP        |Beginning Mile Point            |many                                                                        |
|EMP        |Ending Mile Point               |many                                                                        |
|DEFICIENCY |Not Used                        |NA                                                                          |
|SIDE_OF_RO |Side of road sidewalk desc for  |RIGHT, LEFT, NA                                                             |
|CURBWIDTH  |Width of Curb (units?)          |0, 1, 2, 4                                                                  |
|SHAPE_LEN  |?                               |many                                                                        |
|geometry   |MULTILINESTRING geometry        |many                                                                        |

**shoulders**

|col        |def                                      |vals ( many if > 10 unique )       |
|:----------|:----------------------------------------|:----------------------------------|
|ROAD_LOG_I |Road Log Id - from Public Works          |many                               |
|BMP        |Beginning Mile Point                     |many                               |
|EMP        |Ending Mile Point                        |many                               |
|ROAD_NAME  |Name used for road                       |many                               |
|DISTANCE_M |Distance of segment, in Miles            |many                               |
|DISTANCE_F |Distance of segment, in Feet             |many                               |
|SIDE_OF_RO |Side of road shoulder desc for           |RIGHT, LEFT, NA, Left              |
|SEG_ID     |Unique ID for Road Segment               |many                               |
|COMPOSITE_ |combination of Road Log Id, BMP, and EMP |many                               |
|SHLDR_SURF |Surface material of shoulder             |GRV, ACP, NA, PAP, UNI, BST        |
|SHLDR_WIDT |Width of shoulder surface                |many                               |
|MODIFIED_B |who modified data                        |ABRADFOR, kitsap\s-ags-pwroads     |
|MODIFIED_D |date of modification of data             |many                               |
|CREATED_BY |who created data                         |NA, kitsap\s-ags-pwroads, ABRADFOR |
|CREATED_DT |date of creation of data                 |many                               |
|DISTRICT   |Commissioner District                    |South, Central, North              |
|DIST_SECTI |?                                        |NA, 4, False                       |
|CRIT_AREA  |?                                        |True, False, NA                    |
|SHAPE_LEN  |                                         |many                               |
|geometry   |MULTILINESTRING geometry                 |many                               |


# Import NLCD and create clip to mask 
# Landcover types to mask out
## Open water: 11
## Developed, open space: 21
# Developed, low intentiy: 22
## Developed, medium intensity: 23
## Developed, high intensity: 24

# Project landcovers of interest:
## Deciduous forest: 41
## Evergreen forest: 42
## Mixed forest: 43
## Shrub/scrub: 52
## Grasslands/herbaceous: 71
## Woody wetlands: 90
## Emergent herbaceous wetlands: 95

import ee
import geemap

# Import the NLCD collection
nlcd = ee.ImageCollection('USGS/NLCD_RELEASES/2021_REL/NLCD')

# Filter the collection to the 2021 product
nlcd2021 = nlcd.filter(ee.Filter.eq('system:index', '2021')).first()

# Select the land cover band
landcover = nlcd2021.select('landcover')

# Create a mask for these land cover types
landcover_toinclude = [41, 42, 43, 52, 71, 90, 95]
landcover_toexclude = [11, 41, 42, 43, 52, 71, 90, 95]
landcover_mask = landcover.eq(landcover_types[0])

for landcover_type in landcover_types[1:]:
    landcover_mask = landcover_mask.Or(landcover.eq(landcover_type))

# Define a function to mask and clip each image in the collection
def clip_to_mask(image):
    return image.updateMask(landcover_mask)
    
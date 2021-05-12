import pandas as pd
import geopandas as gpd
import folium
import matplotlib.pyplot as plt

x_ids = pd.read_csv("notebooks/INPUT/REALSAT_TIMESERIES/X_ids_v2.csv")
x_ids = x_ids.loc[0:3199] # notebook trains on first 3200 rows
x_ids = pd.DataFrame(x_ids["id"])

y_pred = pd.read_csv("notebooks/OUTPUT/Joe/REALSAT_TIMESERIES/RESULT/y_pred.csv")
y_pred = pd.DataFrame(y_pred["cluster_id"])

y_pred = pd.concat([x_ids.reset_index(drop=True), y_pred.reset_index(drop=True)], axis=1)

dt = gpd.read_file("data/dt_us_pnt.gpkg")
dt["ID"] = [int(x) for x in dt["ID"]]
dt = dt.loc[dt["ID"].isin(y_pred['id'])]

dt = dt.set_index("ID").join(y_pred.set_index("id"))
dt['cluster_id'] = [str(x) for x in dt['cluster_id']]

# matplotlib ----
dt.plot(column="cluster_id", legend = True)
plt.show()

# folium ---
# https://geopandas.org/gallery/plotting_with_folium.html

m = folium.Map(tiles="cartodbpositron")
dt_list = [[point.xy[1][0], point.xy[0][0]] for point in dt.geometry ]

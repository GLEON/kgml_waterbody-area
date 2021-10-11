import pandas as pd
import folium
import matplotlib.pyplot as plt

# import geopandas as gpd
# dt = gpd.read_file("data/dt_us_pnt.gpkg")
# dt["ID"] = [int(x) for x in dt["ID"]]
# dt_coordinates = [[point.xy[1][0], point.xy[0][0]] for point in dt.geometry]
# dt = pd.concat(
#     [
#         pd.DataFrame(dt.drop(columns="geometry")).reset_index(drop=True),
#         pd.DataFrame(dt_coordinates, columns=["lat", "lon"]).reset_index(drop=True),
#     ],
#     axis=1,
# )
# dt.to_csv("data/dt_us_pnt.csv", index=False)


def color_map(x):
    """Map integers to a list of colors

    See https://geopandas.org/gallery/plotting_with_folium.html

    Args:
        x (int): list position

    Returns:
        str: color
    """
    marker_colors = [
        "red",
        "blue",
        "green",
        "purple",
        "orange",
        "darkred",
        "lightred",
        "beige",
        "darkblue",
        "darkgreen",
        "cadetblue",
        "darkpurple",
        "white",
        "pink",
        "lightblue",
        "lightgreen",
        "gray",
        "black",
        "lightgray",
    ]
    return marker_colors[x]


x_ids = pd.read_csv("notebooks/INPUT/REALSAT_TIMESERIES/X_ids_v2.csv")
x_ids = x_ids.loc[0:3199]  # notebook trains on first 3200 rows
x_ids = pd.DataFrame(x_ids["id"])

y_pred = pd.read_csv("notebooks/OUTPUT/Jem/REALSAT_TIMESERIES/RESULT/y_pred.csv")
y_pred = pd.DataFrame(y_pred["cluster_id"])

y_pred = pd.concat(
    [x_ids.reset_index(drop=True), y_pred.reset_index(drop=True)], axis=1
)

dt = pd.read_csv("data/dt_us_pnt.csv", dtype={"ID": "int"})
dt = dt.loc[dt["ID"].isin(y_pred["id"])]
dt = dt.set_index("ID").join(y_pred.set_index("id"))
dt["cluster_id"] = [str(x) for x in dt["cluster_id"]]

# seaborn ----
sns.scatterplot(data=dt, x="lon", y="lat", hue="cluster_id")
plt.show()

# folium ----
# https://geopandas.org/gallery/plotting_with_folium.html

m = folium.Map(tiles="cartodbpositron")

for i in range(dt.shape[0]):
    # i = 0
    coordinates = (
        dt.reset_index(drop=True)["lat"].loc[[i]].to_list()
        + dt.reset_index(drop=True)["lon"].loc[[i]].to_list()
    )
    cluster_color = color_map(int(list(dt.reset_index().loc[[i]]["cluster_id"])[0]))
    m.add_child(folium.Circle(location=coordinates, color=cluster_color))

folium.LayerControl().add_to(m)
m.fit_bounds(m.get_bounds())
m.save(outfile="test.html")
m

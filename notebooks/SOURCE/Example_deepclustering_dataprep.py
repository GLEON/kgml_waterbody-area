import numpy as np
import pandas as pd
import os

# csv not committed to repo because it is very large
dt_raw = pd.read_csv("data/area_timeseries.csv")
dt_raw = dt_raw.fillna({"area_rm_missing": -1})

ids = dt_raw["id"].unique()
## choose a random assortment of lakes
n_lakes = 4000
np.random.seed(42)
ids = np.random.choice(ids, size=n_lakes, replace=False)

# filter by ids
dt = dt_raw.loc[dt_raw["id"].isin(ids)]
ids_save_path = "notebooks/INPUT/REALSAT_TIMESERIES/X_ids.csv"
ids_save = pd.DataFrame(dt["id"].unique(), columns=["id"])
ids_save.to_csv(ids_save_path)

# pivot wider
dt = dt.pivot(index=["id"], values="area_rm_missing", columns=["date"])
dt = dt.reset_index()
dt = dt.drop("id", axis=1)

dt = dt.to_numpy()

np.save("notebooks/INPUT/REALSAT_TIMESERIES/X_subset.npy", dt)

# X = np.load(
#     os.path.join("notebooks/INPUT/REALSAT_TIMESERIES", "X_subset.npy"),
#     allow_pickle=True,
# ).astype(np.float32)

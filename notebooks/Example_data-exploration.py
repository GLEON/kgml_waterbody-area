import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

DATASET = "EMOTION_DATASET"
INPUT_DIR = os.path.join("INPUT", DATASET)

print("#######################################################################")
print("LOAD DATA")
X = np.load(
    os.path.join(INPUT_DIR, "X_3_emotion_all_augmented_1.npy"), allow_pickle=True
).astype(np.float32)
y = np.load(
    os.path.join(INPUT_DIR, "y_3_emotion_all_augmented_1.npy"), allow_pickle=True
)
print(X.shape, y.shape)

print("#######################################################################")
print("RAW DATA EXPLORATION")
np.unique(y)
dt = pd.DataFrame(np.c_[X, y])
dt = dt.rename(columns={259: "emotion", 258: "test"})
dt["test"] = pd.to_numeric(dt["test"])

dt.groupby("emotion").describe()
sns.histplot(x="emotion", data=dt)
plt.show()

ax = sns.violinplot(x="emotion", y="test", data=dt)
plt.show()

dt_median = dt.groupby("emotion").apply(lambda x: x.median())
dt_median.apply(np.median, axis=1)

g = sns.FacetGrid(data=dt)
g.map_dataframe(sns.histplot, x="emotion")
plt.show()

# test = dt.melt(id_vars="emotion", var_name="feature")
# test["feature"] = test["feature"].astype(str)
# test2 = test.loc[test["feature"].isin(["0", "3"])]
# g = sns.FacetGrid(col="emotion", row="feature", data=test2)
# g.map(sns.histplot)
# plt.show()
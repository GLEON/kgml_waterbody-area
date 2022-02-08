# ---
# jupyter:
#   jupytext:
#     formats: ipynb,py:light
#     text_representation:
#       extension: .py
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.5.1
#   kernelspec:
#     display_name: realsat
#     language: python
#     name: realsat
# ---

# + [markdown] tags=["papermill-error-cell-tag"]
# <span style="color:red; font-family:Helvetica Neue, Helvetica, Arial, sans-serif; font-size:2em;">An Exception was encountered at '<a href="#papermill-error-cell">In [6]</a>'.</span>

# + [markdown] colab_type="text" id="view-in-github" papermill={"duration": 0.018578, "end_time": "2021-03-17T19:02:15.829654", "exception": false, "start_time": "2021-03-17T19:02:15.811076", "status": "completed"} tags=[]
# <a href="https://colab.research.google.com/github/GLEON/realsat/blob/master/Example_classification.ipynb" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a>

# + [markdown] id="VqnxVk9MDTFZ" papermill={"duration": 0.01754, "end_time": "2021-03-17T19:02:15.866475", "exception": false, "start_time": "2021-03-17T19:02:15.848935", "status": "completed"} tags=[]
# # IMPORT LIBRARIES

# + execution={"iopub.execute_input": "2021-03-17T19:02:15.896804Z", "iopub.status.busy": "2021-03-17T19:02:15.896338Z", "iopub.status.idle": "2021-03-17T19:02:16.829772Z", "shell.execute_reply": "2021-03-17T19:02:16.830393Z"} id="zhDfqf127zYt" papermill={"duration": 0.949374, "end_time": "2021-03-17T19:02:16.830547", "exception": false, "start_time": "2021-03-17T19:02:15.881173", "status": "completed"} tags=[]
import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from collections import Counter
import torch
from torch.utils.data.dataset import Dataset
from sklearn.model_selection import train_test_split
from sklearn.metrics import f1_score, confusion_matrix, classification_report
from sklearn.preprocessing import StandardScaler, LabelEncoder

# + [markdown] id="0v5ZdGjR7lLh" papermill={"duration": 0.007418, "end_time": "2021-03-17T19:02:16.845455", "exception": false, "start_time": "2021-03-17T19:02:16.838037", "status": "completed"} tags=[]
# # CONNECT GOOGLE DRIVE (DATA DIRECTORY) TO GOOGLE COLAB

# + execution={"iopub.execute_input": "2021-03-17T19:02:16.874274Z", "iopub.status.busy": "2021-03-17T19:02:16.873465Z", "iopub.status.idle": "2021-03-17T19:02:16.875812Z", "shell.execute_reply": "2021-03-17T19:02:16.876674Z"} id="1ssr6pMN-xNL" papermill={"duration": 0.02253, "end_time": "2021-03-17T19:02:16.876885", "exception": false, "start_time": "2021-03-17T19:02:16.854355", "status": "completed"} tags=[]
# from google.colab import drive
# drive.mount('/content/drive')

# + execution={"iopub.execute_input": "2021-03-17T19:02:16.909382Z", "iopub.status.busy": "2021-03-17T19:02:16.908881Z", "iopub.status.idle": "2021-03-17T19:02:16.910990Z", "shell.execute_reply": "2021-03-17T19:02:16.910587Z"} id="9lH32j6G-9y0" papermill={"duration": 0.018612, "end_time": "2021-03-17T19:02:16.911096", "exception": false, "start_time": "2021-03-17T19:02:16.892484", "status": "completed"} tags=[]
DATASET = "EMOTION_DATASET"
INPUT_DIR = os.path.join("INPUT", DATASET)

YOUR_DIR = os.path.join("jsta", DATASET)
MODEL_DIR = os.path.join(YOUR_DIR, "MODEL")
RESULT_DIR = os.path.join(YOUR_DIR, "RESULT")

if not os.path.exists(MODEL_DIR):
    os.makedirs(MODEL_DIR)
if not os.path.exists(RESULT_DIR):
    os.makedirs(RESULT_DIR)

# + [markdown] id="B1tO8fX68j0Q" papermill={"duration": 0.007804, "end_time": "2021-03-17T19:02:16.928827", "exception": false, "start_time": "2021-03-17T19:02:16.921023", "status": "completed"} tags=[]
# This notebook solves the time-series classification problem using Multi-layer Perceptron model. We solve the emotion classification problem where the input is a time series of length 259 and the classes are positive, neutral and negative emotion.

# + [markdown] id="3m5ocnVaEfdf" papermill={"duration": 0.007513, "end_time": "2021-03-17T19:02:16.943897", "exception": false, "start_time": "2021-03-17T19:02:16.936384", "status": "completed"} tags=[]
# ## HYPERPARAMETERS

# + execution={"iopub.execute_input": "2021-03-17T19:02:16.962822Z", "iopub.status.busy": "2021-03-17T19:02:16.962412Z", "iopub.status.idle": "2021-03-17T19:02:16.964087Z", "shell.execute_reply": "2021-03-17T19:02:16.963798Z"} id="gYLHcjgW7xKD" papermill={"duration": 0.012049, "end_time": "2021-03-17T19:02:16.964174", "exception": false, "start_time": "2021-03-17T19:02:16.952125", "status": "completed"} tags=[]
batch_size = 50
time_steps = 259
channels = 1
classes = 3
learning_rate = 0.005
epochs = 200

# + [markdown] id="YJWyBH2_B8Xa" papermill={"duration": 0.007668, "end_time": "2021-03-17T19:02:16.979602", "exception": false, "start_time": "2021-03-17T19:02:16.971934", "status": "completed"} tags=[]
# # LOADING DATA

# + colab={"base_uri": "https://localhost:8080/"} execution={"iopub.execute_input": "2021-03-17T19:02:17.003742Z", "iopub.status.busy": "2021-03-17T19:02:17.003255Z", "iopub.status.idle": "2021-03-17T19:02:17.049913Z", "shell.execute_reply": "2021-03-17T19:02:17.049365Z"} id="pRjT9SkWncKz" outputId="9f7ed44e-4e58-438a-ff01-95189cd4bc7d" papermill={"duration": 0.062567, "end_time": "2021-03-17T19:02:17.050058", "exception": false, "start_time": "2021-03-17T19:02:16.987491", "status": "completed"} tags=[]
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
print("TRAIN VAL TEST SPLIT")
train_X, test_X, train_y, test_y = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)
train_X, val_X, train_y, val_y = train_test_split(
    train_X, train_y, test_size=0.2, random_state=42, stratify=train_y
)
print(train_X.shape, train_y.shape, Counter(train_y))
print(val_X.shape, val_y.shape, Counter(val_y))
print(test_X.shape, test_y.shape, Counter(test_y))

print("#######################################################################")
print("PREPROCESS LABELS")
le = LabelEncoder()
le.fit(train_y)
print("CLASSES:{}".format(le.classes_))
train_y = le.transform(train_y)
val_y = le.transform(val_y)
test_y = le.transform(test_y)

print("#######################################################################")
print("PREPROCESS INPUTS")
mean = np.mean(train_X)
std = np.std(train_X)
train_X = (train_X - mean) / std
val_X = (val_X - mean) / std
test_X = (test_X - mean) / std

print("#######################################################################")
print("DATA LOADER")


class loader(Dataset):
    def __init__(self, X, y):
        self.data = X
        self.labels = y

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, index):
        return self.data[index], self.labels[index]


train_data = loader(X=train_X, y=train_y)
train_loader = torch.utils.data.DataLoader(
    dataset=train_data, batch_size=batch_size, shuffle=True, num_workers=0
)
val_data = loader(X=val_X, y=val_y)
val_loader = torch.utils.data.DataLoader(
    dataset=val_data, batch_size=batch_size, shuffle=True, num_workers=0
)
test_data = loader(X=test_X, y=test_y)
test_loader = torch.utils.data.DataLoader(
    dataset=test_data, batch_size=batch_size, shuffle=True, num_workers=0
)
print(
    "Train batch:{}\tVal batch:{}\tTest batch:{}".format(
        len(train_loader), len(val_loader), len(test_loader)
    )
)

# + [markdown] id="Pbm8lMLpB6By" papermill={"duration": 0.016951, "end_time": "2021-03-17T19:02:17.085201", "exception": false, "start_time": "2021-03-17T19:02:17.068250", "status": "completed"} tags=[]
#

# + [markdown] id="dkxa1aQgCC2A" papermill={"duration": 0.008771, "end_time": "2021-03-17T19:02:17.106619", "exception": false, "start_time": "2021-03-17T19:02:17.097848", "status": "completed"} tags=[]
# # DEFINE MODEL

# + [markdown] tags=["papermill-error-cell-tag"]
# <span id="papermill-error-cell" style="color:red; font-family:Helvetica Neue, Helvetica, Arial, sans-serif; font-size:2em;">Execution using papermill encountered an exception here and stopped:</span>

# + colab={"base_uri": "https://localhost:8080/"} execution={"iopub.execute_input": "2021-03-17T19:02:17.128187Z", "iopub.status.busy": "2021-03-17T19:02:17.127701Z", "iopub.status.idle": "2021-03-17T19:02:17.249342Z", "shell.execute_reply": "2021-03-17T19:02:17.248779Z"} id="4uAs0LVjzSrp" outputId="f5a79077-3ea6-4202-932b-df0f5f84cf3a" papermill={"duration": 0.135084, "end_time": "2021-03-17T19:02:17.249503", "exception": true, "start_time": "2021-03-17T19:02:17.114419", "status": "failed"} tags=[]
print("#######################################################################")
print("DEFINE MODEL")


class ANN(torch.nn.Module):
    def __init__(self, in_channels, out_channels):
        super(ANN, self).__init__()
        self.fc_1 = torch.nn.Linear(in_channels, 32)
        self.fc_2 = torch.nn.Linear(32, out_channels)

        self.relu = torch.nn.ReLU()

        for m in self.modules():
            if isinstance(m, torch.nn.Conv2d) or isinstance(m, torch.nn.Linear):
                torch.nn.init.xavier_uniform_(m.weight)

    def forward(self, x):
        x_fc_1 = self.relu(self.fc_1(x))
        out = self.fc_2(x_fc_1)
        return out


print("#######################################################################")
print("BUILD MODEL")
model = ANN(in_channels=time_steps, out_channels=classes)
optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
criterion = torch.nn.CrossEntropyLoss()

print(model)

# + [markdown] id="lNzh-fmTCIPQ" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
# # TRAINING MODEL

# + colab={"base_uri": "https://localhost:8080/", "height": 1000} id="7qC-j9nX2zqz" outputId="945a5ed9-57e9-4c60-fcc5-04da6919398a" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
print("#######################################################################")
print("TRAIN MODEL")
train_loss = []
val_loss = []
train_score = []
val_score = []
max_score = 0
for epoch in range(1, epochs):

    model.train()

    total_loss = 0
    for batch, [data, labels] in enumerate(train_loader):
        optimizer.zero_grad()
        # breakpoint()
        out = model(data)
        loss = criterion(out, labels)
        loss.backward()
        optimizer.step()
        total_loss += loss.item()
    print("Epoch:{}\tTrain Loss:{}".format(epoch, total_loss / (batch + 1)), end="\t")
    train_loss.append(total_loss / (batch + 1))

    total_loss = 0
    for batch, [data, labels] in enumerate(val_loader):
        out = model(data)
        loss = criterion(out, labels)
        total_loss += loss.item()
    print("Val Loss:{}".format(total_loss / (batch + 1)), end="\t")
    val_loss.append(total_loss / (batch + 1))

    model.eval()

    pred_labels = np.zeros(len(train_loader) * batch_size).astype(np.int)
    true_labels = np.zeros(len(train_loader) * batch_size).astype(np.int)
    total = 0
    for batch, [data, labels] in enumerate(train_loader):
        out = model(data)
        out = torch.argmax(torch.nn.functional.softmax(out, dim=1), dim=1)
        pred_labels[total : total + len(labels)] = out.detach().cpu().numpy()
        true_labels[total : total + len(labels)] = labels.cpu().numpy()
        total += len(labels)
    pred_labels = pred_labels[:total]
    true_labels = true_labels[:total]
    train_score.append(
        f1_score(y_true=true_labels, y_pred=pred_labels, average="macro")
    )
    print("Train Score:{:.4f}".format(train_score[-1]), end="\t")

    pred_labels = np.zeros(len(val_loader) * batch_size).astype(np.int)
    true_labels = np.zeros(len(val_loader) * batch_size).astype(np.int)
    total = 0
    for batch, [data, labels] in enumerate(val_loader):
        out = model(data)
        out = torch.argmax(torch.nn.functional.softmax(out, dim=1), dim=1)
        pred_labels[total : total + len(labels)] = out.detach().cpu().numpy()
        true_labels[total : total + len(labels)] = labels.cpu().numpy()
        total += len(labels)
    pred_labels = pred_labels[:total]
    true_labels = true_labels[:total]
    val_score.append(f1_score(y_true=true_labels, y_pred=pred_labels, average="macro"))
    print("Val Score:{:.4f}\tMax Score:{:.4f}".format(val_score[-1], max_score))
    if max_score < val_score[-1]:
        max_score = val_score[-1]
        torch.save(model.state_dict(), os.path.join(MODEL_DIR, "ANN.pt"))

fig, axs = plt.subplots(nrows=1, ncols=2, constrained_layout=True)
ax = axs[0]
ax.plot(train_loss, label="train")
ax.plot(val_loss, label="val")
ax.legend()
ax.set_ylabel("loss")
ax.set_xlabel("epochs")
ax = axs[1]
ax.plot(train_score, label="train")
ax.plot(val_score, label="val")
ax.legend()
ax.set_ylabel("F1")
ax.set_xlabel("epochs")
plt.savefig(os.path.join(RESULT_DIR, "ANN.pdf"), format="pdf")
plt.show()

# + [markdown] id="PJQ-fAlaCRob" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
# # TEST MODEL

# + colab={"base_uri": "https://localhost:8080/"} id="ePC-7R7CQq9z" outputId="a3dbbd9a-19f3-4920-e70c-72a2a95e2561" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
print("#######################################################################")
print("LOAD MODEL")
model.load_state_dict(torch.load(os.path.join(MODEL_DIR, "ANN.pt")))
model.eval()

# + colab={"base_uri": "https://localhost:8080/", "height": 755} id="IeW1RTw2QuqA" outputId="945adb98-03c6-4690-970b-cb70acebb8c8" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
print("#######################################################################")
print("TEST ON TRAIN SET")
pred_labels = np.zeros(len(train_loader) * batch_size).astype(np.int)
true_labels = np.zeros(len(train_loader) * batch_size).astype(np.int)
total = 0
for batch, [data, labels] in enumerate(train_loader):
    out = model(data)
    out = torch.argmax(torch.nn.functional.softmax(out, dim=1), dim=1)
    pred_labels[total : total + len(labels)] = out.detach().cpu().numpy()
    true_labels[total : total + len(labels)] = labels.cpu().numpy()
    total += len(labels)
pred_labels = pred_labels[:total]
true_labels = true_labels[:total]
print("Scores:")
print(
    classification_report(true_labels, pred_labels, target_names=le.classes_, digits=3)
)
print("\nConfusion_matrix:")
cm = confusion_matrix(true_labels, pred_labels)
cm_df = pd.DataFrame(cm, le.classes_, le.classes_)
plt.figure(figsize=(12, 8))
sns.heatmap(cm_df, annot=True)

# + colab={"base_uri": "https://localhost:8080/", "height": 755} id="DY9sIeLiQ8n3" outputId="a5765df7-ec2d-44b5-8eee-054af99e93b8" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
print("#######################################################################")
print("TEST ON VAL SET")
pred_labels = np.zeros(len(val_loader) * batch_size).astype(np.int)
true_labels = np.zeros(len(val_loader) * batch_size).astype(np.int)
total = 0
for batch, [data, labels] in enumerate(val_loader):
    out = model(data)
    out = torch.argmax(torch.nn.functional.softmax(out, dim=1), dim=1)
    pred_labels[total : total + len(labels)] = out.detach().cpu().numpy()
    true_labels[total : total + len(labels)] = labels.cpu().numpy()
    total += len(labels)
pred_labels = pred_labels[:total]
true_labels = true_labels[:total]
print("Scores:")
print(
    classification_report(true_labels, pred_labels, target_names=le.classes_, digits=3)
)
print("\nConfusion_matrix:")
cm = confusion_matrix(true_labels, pred_labels)
cm_df = pd.DataFrame(cm, le.classes_, le.classes_)
plt.figure(figsize=(12, 8))
sns.heatmap(cm_df, annot=True)

# + colab={"base_uri": "https://localhost:8080/", "height": 755} id="yhdN88IGGobE" outputId="65a80c03-3c7c-46b7-f7ee-7170f62cd3ec" papermill={"duration": null, "end_time": null, "exception": null, "start_time": null, "status": "pending"} tags=[]
print("#######################################################################")
print("TEST ON TEST SET")
pred_labels = np.zeros(len(test_loader) * batch_size).astype(np.int)
true_labels = np.zeros(len(test_loader) * batch_size).astype(np.int)
total = 0
for batch, [data, labels] in enumerate(test_loader):
    out = model(data)
    out = torch.argmax(torch.nn.functional.softmax(out, dim=1), dim=1)
    pred_labels[total : total + len(labels)] = out.detach().cpu().numpy()
    true_labels[total : total + len(labels)] = labels.cpu().numpy()
    total += len(labels)
pred_labels = pred_labels[:total]
true_labels = true_labels[:total]
print("Scores:")
print(
    classification_report(true_labels, pred_labels, target_names=le.classes_, digits=3)
)
print("\nConfusion_matrix:")
cm = confusion_matrix(true_labels, pred_labels)
cm_df = pd.DataFrame(cm, le.classes_, le.classes_)
plt.figure(figsize=(12, 8))
sns.heatmap(cm_df, annot=True)

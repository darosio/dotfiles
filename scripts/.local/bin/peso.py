#!/usr/bin/env python
#
"""Track weight."""
from __future__ import annotations

from datetime import timedelta

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns


def linearfit(days, val):  # noqa: ANN001
    """Fit line."""
    slope, intercept = np.polyfit(days, val, 1)
    fit_label = f" grams/week ({slope * 7000:.1f}) "
    return slope, fit_label


# data input
df = pd.read_csv("~/Sync/4home/Dan/npeso.tsv", sep="\t")  # noqa: PD901
df.data = pd.to_datetime(df.data, format="%d/%m/%Y")
df["days"] = [d.days for d in df.data - df.data.loc[0] + timedelta(days=1)]

slope, fit_label = linearfit(df.days, df.peso)
print(fit_label)

# plotting
sns.set_style("whitegrid")

sns.regplot(
    "days",
    "peso",
    data=df,
    ci=0,
    scatter_kws={"marker": "s", "s": 60, "color": "slategrey"},
    label="weight",
)
sns.regplot(
    "days",
    "peso",
    data=df,
    ci=95,
    color="indianred",
    label=fit_label,
    line_kws={"alpha": 0.71, "linewidth": 3},
    scatter=False,
)
# sns.regplot("days", "peso", data=df, ci=95, color="yellow", robust=True) # noqa: ERA001, E501
sns.regplot(
    "days",
    "peso",
    data=df,
    lowess=True,
    color="seagreen",
    label="lowess",
    scatter=False,
    line_kws={"alpha": 0.5, "linewidth": 7},
)

# sns.regplot(df.days[1:-1], np.convolve(df.peso, np.ones((3,))/3, mode='valid'),
#             ci=0, fit_reg=False, marker='_', line_kws={'linewidth': 5})
sns.lineplot(df.days[1:-1], np.convolve(df.peso, np.ones((3,)) / 3, mode="valid"))
sns.scatterplot(
    df.days[1:-1], np.convolve(df.peso, np.ones((3,)) / 3, mode="valid"), color="orange"
)

plt.legend()
plt.title("Tracking my weight")
plt.xlim([0, df.days.iloc[-1] + 1])
# plt.ylim([73.5, 77.5]) # noqa: ERA001
plt.grid(b=True, which="minor", color="r", linestyle="--")
ymin = int(round(min(df.peso) - 0.5))
ymax = int(round(max(df.peso) + 0.5))
plt.yticks([i / 2 for i in range(ymin * 2, ymax * 2 + 1)])
plt.tight_layout()
plt.show()

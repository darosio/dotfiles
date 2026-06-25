#!/usr/bin/env python
#
"""Track weight."""

import re
from pathlib import Path

import matplotlib.dates as mdates
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

SMOOTHING_WINDOW_DAYS = 3
WEIGHT_TABLE = Path("~/Sync/box/org/gtd.org").expanduser()
WEIGHT_ROW_RE = re.compile(
    r"\|\s*<(?P<date>\d{4}-\d{2}-\d{2})\s+\w+>\s*\|"
    r"\s*(?P<weight>\d+(?:\.\d+)?)\s*\|",
)
WEIGHT_HEADING_RE = re.compile(r"^\*\s+weight\s*$", re.IGNORECASE)


def linearfit(days: object, val: object) -> str:
    """Fit line."""
    slope, _intercept = np.polyfit(days, val, 1)
    return f" grams/week ({slope * 7000:.1f}) "


def read_weight_table(path: Path = WEIGHT_TABLE) -> pd.DataFrame:
    """Read weight entries from the Org table in gtd.org."""
    rows = []
    in_weight_heading = False
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("* "):
            in_weight_heading = bool(WEIGHT_HEADING_RE.match(line))
            continue
        if not in_weight_heading:
            continue
        match = WEIGHT_ROW_RE.match(line)
        if match:
            rows.append({
                "date": match.group("date"),
                "weight": float(match.group("weight")),
            })
    return pd.DataFrame(rows)


def main() -> None:
    """Run main."""
    # data input
    df = read_weight_table()
    df.date = pd.to_datetime(df.date, format="%Y-%m-%d")
    df["days"] = (df.date - df.date.iloc[0]).dt.days + 1
    df["date_num"] = mdates.date2num(df.date)
    grams_per_week = linearfit(df.days, df.weight)
    print(grams_per_week)

    # plotting
    sns.set_style("whitegrid")
    sns.regplot(x="date_num", y="weight", data=df, ci=0, scatter_kws={"s": 100})
    sns.regplot(
        x="date_num",
        y="weight",
        data=df,
        ci=95,
        color="indianred",
        label=grams_per_week,
        line_kws={"alpha": 0.75, "linewidth": 3},
        scatter=False,
    )
    sns.regplot(
        x="date_num",
        y="weight",
        data=df,
        lowess=True,
        color="seagreen",
        label="lowess",
        scatter=False,
        line_kws={"alpha": 0.5, "linewidth": 7},
    )

    if len(df) >= SMOOTHING_WINDOW_DAYS:
        smoothed = np.convolve(
            df.weight,
            np.ones(SMOOTHING_WINDOW_DAYS) / SMOOTHING_WINDOW_DAYS,
            mode="valid",
        )
        sns.scatterplot(
            x=df.date_num[1:-1],
            y=smoothed,
            marker="*",
            color="darkred",
            s=500,
            label="3-day moving average",
        )
    plt.legend()
    plt.title("Tracking my weight")
    plt.xlim([df.date_num.iloc[0] - 1, df.date_num.iloc[-1] + 1])
    ax = plt.gca()
    locator = mdates.AutoDateLocator()
    ax.xaxis.set_major_locator(locator)
    ax.xaxis.set_major_formatter(mdates.ConciseDateFormatter(locator))
    # plt.ylim([73.5, 77.5]) # noqa: ERA001
    plt.grid(which="minor", color="r", linestyle="--")
    ymin = round(min(df.weight) - 0.5)
    ymax = round(max(df.weight) + 0.5)
    plt.yticks([i / 2 for i in range(ymin * 2, ymax * 2 + 1)])
    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    main()

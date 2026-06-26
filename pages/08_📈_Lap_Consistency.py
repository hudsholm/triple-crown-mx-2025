import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Lap Consistency",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

RIDER_COLORS = ["#E8641A", "#1A7FE8", "#2ECC71", "#9B59B6", "#E74C3C"]

def hex_to_rgba(hex_color, opacity=0.08):
    h = hex_color.lstrip("#")
    r, g, b = int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16)
    return f"rgba({r},{g},{b},{opacity})"

st.title("8. Lap Consistency")
st.markdown("KDE lap time distributions for the top 5 finishers, plus coefficient of variation rankings.")

all_years   = sorted(df["year"].astype(str).unique())
all_classes = ["450", "250", "WMX"]
all_rounds  = sorted(df["round"].astype(str).unique(), key=lambda x: int(x))
all_motos   = sorted(df["moto"].astype(str).unique(), key=lambda x: int(x))

# ═══════════════════════════════════════════════════════════════════════════════
# 8a. KDE lap time distributions — top 5 per moto
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Lap time distributions — top 5 finishers")

c1, c2, c3, c4 = st.columns(4)
with c1:
    yr_kde  = st.selectbox("Year",  all_years,   index=len(all_years) - 1, key="kde_yr")
with c2:
    cls_kde = st.selectbox("Class", all_classes, key="kde_cls")
with c3:
    rnd_kde = st.selectbox("Round", all_rounds,  key="kde_rnd")
with c4:
    mto_kde = st.selectbox("Moto",  all_motos,   key="kde_mto")

if st.button("Update — KDE", type="primary", key="kde_btn"):
    subset = df[
        (df["year"].astype(str)  == yr_kde) &
        (df["class_label"]       == cls_kde) &
        (df["round"].astype(str) == rnd_kde) &
        (df["moto"].astype(str)  == mto_kde)
    ].copy()

    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        top5 = (
            subset.drop_duplicates(subset=["name"])
            .dropna(subset=["finish_position"])
            .sort_values("finish_position")
            .head(5)["name"].tolist()
        )
        track = subset["track"].iloc[0]
        laps  = subset[
            subset["name"].isin(top5) & (subset["lap"].astype(float) > 1)
        ].dropna(subset=["lap_time"])

        if laps.empty:
            st.warning("No lap time data for selected race.")
        else:
            x_min   = laps["lap_time"].min() * 0.98
            x_max   = laps["lap_time"].max() * 1.02
            x_range = np.linspace(x_min, x_max, 500)

            fig = go.Figure()
            for i, rider in enumerate(top5):
                rider_laps = laps[laps["name"] == rider]["lap_time"].values
                if len(rider_laps) < 2:
                    continue
                fp = int(
                    subset[subset["name"] == rider]["finish_position"]
                    .dropna().iloc[0]
                )
                color   = RIDER_COLORS[i]
                kde     = gaussian_kde(rider_laps, bw_method=0.3)
                density = kde(x_range)
                median  = np.median(rider_laps)

                fig.add_trace(go.Scatter(
                    x=x_range, y=density, mode="lines",
                    name=f"P{fp} — {rider} (med: {median:.2f}s)",
                    line=dict(color=color, width=2),
                    fill="tozeroy", fillcolor=hex_to_rgba(color, 0.08),
                    hovertemplate=f"<b>P{fp} — {rider}</b><br>Lap: %{{x:.2f}}s<br>Density: %{{y:.4f}}<extra></extra>",
                ))

            fig.update_layout(
                title=f"Lap Time Distribution — {cls_kde} | {track} | Round {rnd_kde} Moto {mto_kde}",
                xaxis=dict(title="Lap Time (s)"),
                yaxis=dict(title="Density", showticklabels=False),
                height=500, margin=dict(l=60, r=60, t=60, b=60),
                legend=dict(title="Rider"), hovermode="x unified",
            )
            st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 8b. CV consistency table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Coefficient of variation — consistency rankings")

@st.cache_data
def build_cv_table(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"]  = base["lap"].astype(float)
    base["year"] = base["year"].astype(str)

    max_lap = (
        base.groupby("race_id", observed=True)["lap"]
        .max().rename("max_lap").reset_index()
    )
    base = base.merge(max_lap, on="race_id", how="left")
    base["threshold"] = (base["max_lap"] * 0.75).apply(np.floor)
    laps_comp = (
        base.groupby(["race_id", "name"], observed=True)["lap"]
        .max().rename("laps_completed").reset_index()
    )
    base = base.merge(laps_comp, on=["race_id", "name"], how="left")
    base = base[base["laps_completed"] >= base["threshold"]].copy()
    base = base[base["lap"] > 1].copy()

    cv_per_race = (
        base.groupby(["race_id", "name", "class_label", "year"], observed=True)["lap_time"]
        .agg(mean_lap="mean", std_lap="std").reset_index()
    )
    cv_per_race["cv"] = (cv_per_race["std_lap"] / cv_per_race["mean_lap"] * 100)
    cv_per_race = cv_per_race.dropna(subset=["cv"])
    cv_per_race = cv_per_race.merge(
        _df.drop_duplicates(subset=["race_id", "name"])[["race_id", "name", "finish_position"]],
        on=["race_id", "name"], how="left"
    )

    rider_cv = (
        cv_per_race.groupby(["name", "class_label", "year"], observed=True)
        .agg(avg_cv=("cv", "mean"), avg_finish=("finish_position", "mean"),
             eligible_motos=("race_id", "count"))
        .reset_index()
    )
    rider_cv["avg_cv"]     = rider_cv["avg_cv"].round(2)
    rider_cv["avg_finish"] = rider_cv["avg_finish"].round(1)

    total_motos = (
        _df.drop_duplicates(subset=["race_id", "name"])
        .assign(year=lambda x: x["year"].astype(str))
        .groupby(["name", "class_label", "year"], observed=True)
        .size().reset_index(name="total_motos")
    )
    return rider_cv.merge(total_motos, on=["name", "class_label", "year"], how="left")

cv_table = build_cv_table(df)
cv_years = sorted(cv_table["year"].unique())

SORT_MAP = {
    "Avg CV — most consistent first":   ("avg_cv",          True),
    "Avg CV — least consistent first":  ("avg_cv",          False),
    "Avg Finish":                        ("avg_finish",      True),
    "Total Motos":                       ("total_motos",     False),
    "Eligible Motos":                    ("eligible_motos",  False),
}

c1, c2, c3, c4 = st.columns(4)
with c1:
    yr_cv   = st.selectbox("Year",  cv_years, index=len(cv_years) - 1, key="cv_yr")
with c2:
    cls_cv  = st.selectbox("Class", all_classes, key="cv_cls")
with c3:
    top_cv  = st.slider("Top N", 5, 50, 20, 5, key="cv_n")
with c4:
    sort_cv = st.selectbox("Sort by", list(SORT_MAP.keys()), key="cv_sort")

if st.button("Update — CV table", type="primary", key="cv_btn"):
    subset = cv_table[
        (cv_table["year"] == yr_cv) &
        (cv_table["class_label"] == cls_cv)
    ]
    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        col, asc = SORT_MAP[sort_cv]
        result = (
            subset.sort_values(col, ascending=asc, na_position="last")
            .head(top_cv).reset_index(drop=True)
            .rename(columns={
                "name": "Rider", "avg_cv": "Avg CV (%)",
                "avg_finish": "Avg Finish", "total_motos": "Total Motos",
                "eligible_motos": "Eligible Motos",
            })
        )
        result.index += 1
        st.dataframe(
            result[["Rider", "Total Motos", "Eligible Motos", "Avg CV (%)", "Avg Finish"]],
            use_container_width=False,
        )

st.divider()
st.markdown("""
**Notes**
- Lap 1 excluded from both charts — it's shorter and noisier than full laps.
- KDE curves use bandwidth 0.3; they track actual lap times closely rather than heavily smoothing. Y-axis density values integrate to 1 — curve shape and position are what matter, not the raw numbers.
- CV = std dev of lap times ÷ mean lap time, expressed as %. Normalises consistency across different pace levels — a 2% CV means the same whether a rider's average lap is 60s or 100s.
- CV is computed per moto then averaged across motos, so it measures within-moto consistency rather than track-to-track variation.
- Riders must complete ≥75% of a moto's laps to be included. "Eligible Motos" vs "Total Motos" gap indicates unreliable finishing.
""")

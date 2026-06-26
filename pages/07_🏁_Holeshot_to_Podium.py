import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy import stats
from statsmodels.nonparametric.smoothers_lowess import lowess
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Holeshot to Podium",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

st.title("7. From the Holeshot to the Podium")
st.markdown("Average starts, finishes, and positions gained/lost per moto")

CLASS_COLORS = {"450": "#E8641A", "250": "#1A7FE8", "WMX": "#2ECC71"}

@st.cache_data
def build_rider_agg(_df):
    pos = _df.dropna(subset=["lap", "place"]).copy()
    pos["lap"]   = pos["lap"].astype(float)
    pos["place"] = pos["place"].astype(float)
    pos["year"]  = pos["year"].astype(str)

    pos = pos.sort_values(["race_id", "name", "lap"])
    pos["prev_place"] = pos.groupby(["race_id", "name"], observed=True)["place"].shift(1)
    pos["next_lap"]   = pos.groupby(["race_id", "name"], observed=True)["lap"].shift(1)
    pos = pos[pos["prev_place"].notna() & (pos["lap"] == pos["next_lap"] + 1)].copy()
    pos["pos_gained"] = (pos["prev_place"] - pos["place"]).clip(lower=0)
    pos["pos_lost"]   = (pos["place"] - pos["prev_place"]).clip(lower=0)

    lap1 = (
        _df[_df["lap"] == 1].dropna(subset=["place"])
        .groupby(["race_id", "name"], observed=True)["place"]
        .first().reset_index(name="lap1_place")
    )
    race_agg = (
        pos.groupby(["race_id", "name", "class_label", "year"], observed=True)
        .agg(total_pos_gained=("pos_gained", "sum"), total_pos_lost=("pos_lost", "sum"))
        .reset_index()
    )
    race_agg = race_agg.merge(lap1, on=["race_id", "name"], how="left")
    race_agg = race_agg.merge(
        _df.drop_duplicates(subset=["race_id", "name"])[["race_id", "name", "finish_position"]],
        on=["race_id", "name"], how="left"
    )
    rider_agg = (
        race_agg.groupby(["name", "class_label", "year"], observed=True)
        .agg(
            total_motos=("race_id", "count"),
            avg_start=("lap1_place", "mean"),
            total_pos_gained=("total_pos_gained", "sum"),
            total_pos_lost=("total_pos_lost", "sum"),
            avg_finish=("finish_position", "mean"),
        ).reset_index()
    )
    rider_agg["pos_gained_per_moto"] = (rider_agg["total_pos_gained"] / rider_agg["total_motos"]).round(2)
    rider_agg["pos_lost_per_moto"]   = (rider_agg["total_pos_lost"]   / rider_agg["total_motos"]).round(2)
    rider_agg["gain_loss_ratio"]     = (
        rider_agg["pos_gained_per_moto"] / rider_agg["pos_lost_per_moto"].replace(0, np.nan)
    ).round(2)
    rider_agg["avg_start"]        = rider_agg["avg_start"].round(1)
    rider_agg["avg_finish"]       = rider_agg["avg_finish"].round(1)
    rider_agg["total_pos_gained"] = rider_agg["total_pos_gained"].astype(int)
    rider_agg["total_pos_lost"]   = rider_agg["total_pos_lost"].astype(int)
    return rider_agg

rider_agg = build_rider_agg(df)
all_years = sorted(rider_agg["year"].astype(str).unique())

SORT_OPTIONS = [
    ("Avg Finish",        "avg_finish",          True),
    ("Avg Start",         "avg_start",           True),
    ("Total Motos",       "total_motos",         False),
    ("Total Pos Gained",  "total_pos_gained",    False),
    ("Total Pos Lost",    "total_pos_lost",      False),
    ("Pos Gained / Moto", "pos_gained_per_moto", False),
    ("Pos Lost / Moto",   "pos_lost_per_moto",   False),
    ("Gain/Loss Ratio",   "gain_loss_ratio",     False),
]

# ═══════════════════════════════════════════════════════════════════════════════
# 7a. Positions gained / lost table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Positions gained & lost")

c1, c2, c3, c4 = st.columns(4)
with c1:
    yr_sel  = st.selectbox("Year",    all_years, index=len(all_years) - 1, key="pg_yr")
with c2:
    cls_sel = st.selectbox("Class",   ["450", "250", "WMX"], key="pg_cls")
with c3:
    top_n   = st.slider("Top N", 5, 50, 20, 5, key="pg_n")
with c4:
    sort_opt = st.selectbox(
        "Sort by", SORT_OPTIONS,
        format_func=lambda x: x[0], key="pg_sort"
    )

if st.button("Update — table", type="primary", key="pg_btn"):
    subset = rider_agg[
        (rider_agg["year"] == yr_sel) &
        (rider_agg["class_label"] == cls_sel)
    ]
    if subset.empty:
        st.warning("No data for selected filters.")
    else:
        col, asc = sort_opt[1], sort_opt[2]
        result = (
            subset.sort_values(col, ascending=asc, na_position="last")
            .head(top_n).reset_index(drop=True)
            .rename(columns={
                "name": "Rider", "total_motos": "Total Motos",
                "avg_start": "Avg Start", "total_pos_gained": "Total Pos Gained",
                "total_pos_lost": "Total Pos Lost", "avg_finish": "Avg Finish",
                "pos_gained_per_moto": "Pos Gained/Moto",
                "pos_lost_per_moto": "Pos Lost/Moto",
                "gain_loss_ratio": "Gain/Loss Ratio",
            })
        )
        result.index += 1
        st.dataframe(
            result[["Rider", "Total Motos", "Avg Start", "Total Pos Gained",
                    "Total Pos Lost", "Avg Finish", "Pos Gained/Moto",
                    "Pos Lost/Moto", "Gain/Loss Ratio"]],
            use_container_width=False,
        )

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 7b. Avg finish vs pos gained / moto scatter (LOWESS)
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Avg finish vs positions gained per moto")

yr_pg = st.selectbox("Year", all_years, index=len(all_years) - 1, key="pg_scatter_yr")

if st.button("Update — scatter", type="primary", key="pg_scatter_btn"):
    fig = go.Figure()
    for cls in ["450", "250", "WMX"]:
        sub = rider_agg[
            (rider_agg["year"] == yr_pg) &
            (rider_agg["class_label"] == cls)
        ].dropna(subset=["avg_finish", "pos_gained_per_moto"])
        if sub.empty:
            continue
        color = CLASS_COLORS[cls]
        fig.add_trace(go.Scatter(
            x=sub["avg_finish"], y=sub["pos_gained_per_moto"],
            mode="markers", name=cls,
            marker=dict(color=color, size=7, opacity=0.6),
            legendgroup=cls,
            customdata=sub["name"],
            hovertemplate="<b>%{customdata}</b><br>Avg Finish: %{x}<br>Pos Gained/Moto: %{y}<extra></extra>",
        ))
        ss = sub.sort_values("avg_finish")
        sm = lowess(ss["pos_gained_per_moto"], ss["avg_finish"], frac=0.5, return_sorted=True)
        fig.add_trace(go.Scatter(
            x=sm[:, 0], y=sm[:, 1], mode="lines",
            name=f"{cls} trend", line=dict(color=color, width=2.5),
            legendgroup=cls, hoverinfo="skip",
        ))
    fig.update_layout(
        title=f"Avg Finish vs Pos Gained/Moto — {yr_pg}",
        xaxis=dict(title="Avg Finish"),
        yaxis=dict(title="Pos Gained / Moto"),
        height=550, margin=dict(l=60, r=60, t=60, b=60),
        legend=dict(title="Class"), hovermode="closest",
    )
    st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 7c. Avg start vs avg finish scatter (with y=x reference line)
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Avg start vs avg finish")

yr_sf = st.selectbox("Year", all_years, index=len(all_years) - 1, key="sf_yr")

if st.button("Update — start/finish scatter", type="primary", key="sf_btn"):
    fig = go.Figure()
    all_vals = []
    for cls in ["450", "250", "WMX"]:
        sub = rider_agg[
            (rider_agg["year"] == yr_sf) &
            (rider_agg["class_label"] == cls)
        ].dropna(subset=["avg_start", "avg_finish"])
        if sub.empty:
            continue
        color = CLASS_COLORS[cls]
        all_vals += list(sub["avg_start"]) + list(sub["avg_finish"])
        fig.add_trace(go.Scatter(
            x=sub["avg_start"], y=sub["avg_finish"],
            mode="markers", name=cls,
            marker=dict(color=color, size=7, opacity=0.6),
            legendgroup=cls,
            customdata=sub["name"],
            hovertemplate="<b>%{customdata}</b><br>Avg Start: %{x}<br>Avg Finish: %{y}<extra></extra>",
        ))
        slope, intercept, r, _, _ = stats.linregress(sub["avg_start"], sub["avg_finish"])
        x_rng = np.linspace(sub["avg_start"].min(), sub["avg_start"].max(), 100)
        fig.add_trace(go.Scatter(
            x=x_rng, y=slope * x_rng + intercept,
            mode="lines", name=f"{cls} trend (R²={r**2:.2f})",
            line=dict(color=color, width=2, dash="dash"),
            legendgroup=cls, hoverinfo="skip",
        ))
    if all_vals:
        ref = [min(all_vals), max(all_vals)]
        fig.add_trace(go.Scatter(
            x=ref, y=ref, mode="lines", name="Start = Finish",
            line=dict(color="grey", width=1.5, dash="dot"), hoverinfo="skip",
        ))
    fig.update_layout(
        title=f"Avg Start vs Avg Finish — {yr_sf}",
        xaxis=dict(title="Avg Start"),
        yaxis=dict(title="Avg Finish"),
        height=550, margin=dict(l=60, r=60, t=60, b=60),
        legend=dict(title="Class"), hovermode="closest",
    )
    st.plotly_chart(fig, use_container_width=True)

st.divider()
st.markdown("""
**Notes**
- "Avg Start" is position at the end of lap 1, not off the gate. A rider who got the holeshot but was passed before lap 1 ended will show a worse start than their gate position.
- Positions gained/lost are summed lap-to-lap across the moto, not start-to-finish. A rider who dropped 10 spots then recovered 8 registers as 10 lost AND 8 gained.
- Gain/Loss Ratio: >1 = net gainer, <1 = net loser. Riders with zero positions lost return NaN.
- Front-runners often have low gain totals — they start near the front with less room to move forward.
- The LOWESS curve in the scatter shows the geometric ceiling effect: both the very front and very back have limited room to gain positions.
""")

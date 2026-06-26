import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Traffic Effects",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

CONDITIONS = [
    ("Sandwiched",   "cond_sandwiched",   "#9B27AE"),
    ("Chasing",      "cond_chasing",      "#E8641A"),
    ("Being Chased", "cond_being_chased", "#E8C11A"),
    ("Clear Air",    "cond_clear_air",    "#1A7FE8"),
]
COND_META = {k: (label, color) for label, k, color in CONDITIONS}

def hex_to_rgba(hex_color, opacity=0.08):
    h = hex_color.lstrip("#")
    r, g, b = int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16)
    return f"rgba({r},{g},{b},{opacity})"

st.title("12. Traffic Effects")
st.markdown("Pace by traffic condition and lap-by-lap place journey, coloured by traffic state")

@st.cache_data
def build_traffic_base(_df):
    base = _df.dropna(subset=["lap", "lap_time", "place"]).copy()
    base["lap"]   = base["lap"].astype(float)
    base["place"] = base["place"].astype(float)
    base["year"]  = base["year"].astype(str)
    base["round"] = base["round"].astype(str)
    base["moto"]  = base["moto"].astype(str)
    base["traffic_state"] = base["traffic_state"].astype(str)
    base["cond_sandwiched"]   = base["traffic_state"] == "Sandwiched"
    base["cond_chasing"]      = base["traffic_state"] == "Chasing"
    base["cond_being_chased"] = base["traffic_state"] == "Being Chased"
    base["cond_clear_air"]    = base["traffic_state"] == "Clear Air"
    return base

traffic_base = build_traffic_base(df)

all_years   = sorted(traffic_base["year"].unique())
all_classes = ["450", "250", "WMX"]
all_rounds  = ["All"] + sorted(traffic_base["round"].unique(), key=lambda x: int(x))
all_motos   = ["All"] + sorted(traffic_base["moto"].unique(), key=lambda x: int(x))
specific_rounds = sorted(traffic_base["round"].unique(), key=lambda x: int(x))
specific_motos  = sorted(traffic_base["moto"].unique(), key=lambda x: int(x))

# ═══════════════════════════════════════════════════════════════════════════════
# 12a. Pace (z-score) by traffic condition — KDE
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Pace by traffic condition — z-score distribution")
st.caption("Only riders with ≥3 laps in 1st place in the selected window appear. This restricts to front-runners who experience all four conditions.")

c1, c2, c3, c4 = st.columns(4)
with c1: yr_tr  = st.selectbox("Year",  all_years,   index=len(all_years) - 1, key="tr_yr")
with c2: cls_tr = st.selectbox("Class", all_classes, key="tr_cls")
with c3: rnd_tr = st.selectbox("Round", all_rounds,  key="tr_rnd")
with c4: mto_tr = st.selectbox("Moto",  all_motos,   key="tr_mto")

def eligible_traffic(yr, cls, rnd, mto):
    m = (traffic_base["year"] == yr) & (traffic_base["class_label"] == cls) & (traffic_base["place"] == 1)
    if rnd != "All": m &= traffic_base["round"] == rnd
    if mto != "All": m &= traffic_base["moto"]  == mto
    laps_in_first = traffic_base[m].groupby("name", observed=True)["lap"].count()
    return sorted(laps_in_first[laps_in_first > 2].index.tolist())

elig_tr = eligible_traffic(yr_tr, cls_tr, rnd_tr, mto_tr)
rider_tr = st.selectbox(
    "Rider", elig_tr if elig_tr else ["— no eligible riders —"], key="tr_rdr"
)

if st.button("Update — traffic KDE", type="primary", key="tr_btn") and elig_tr:
    m = (
        (traffic_base["year"] == yr_tr) &
        (traffic_base["class_label"] == cls_tr) &
        (traffic_base["name"] == rider_tr)
    )
    if rnd_tr != "All": m &= traffic_base["round"] == rnd_tr
    if mto_tr != "All": m &= traffic_base["moto"]  == mto_tr
    subset = traffic_base[m]

    fig = go.Figure()
    all_vals = []

    for label, cond_col, color in CONDITIONS:
        vals = subset[subset[cond_col]]["z_score"].dropna().values
        if len(vals) < 2:
            continue
        all_vals.extend(vals)
        kde    = gaussian_kde(vals, bw_method=0.3)
        x1     = np.percentile(vals, 1)
        x99    = np.percentile(vals, 99)
        x_lo   = x1 * 1.1 if x1 < 0 else x1 * 0.9
        x_rng  = np.linspace(x_lo, x99 * 1.1, 500)
        density = kde(x_rng)
        median  = np.median(vals)
        med_d   = kde(np.array([median]))[0]

        fig.add_trace(go.Scatter(
            x=x_rng, y=density, mode="lines",
            name=f"{label} (n={len(vals)}, median={median:.2f})",
            line=dict(color=color, width=2),
            fill="tozeroy", fillcolor=hex_to_rgba(color, 0.08),
            hovertemplate=f"<b>{label}</b><br>Z-Score: %{{x:.2f}}<br>Density: %{{y:.4f}}<extra></extra>",
        ))
        fig.add_trace(go.Scatter(
            x=[median, median], y=[0, med_d], mode="lines",
            line=dict(color=color, width=1.5, dash="dot"),
            showlegend=False, hoverinfo="skip",
        ))

    if all_vals:
        g1, g99 = np.percentile(all_vals, 1), np.percentile(all_vals, 99)
        x_lo = g1 * 1.15 if g1 < 0 else g1 * 0.85
        rnd_s = f"Round {rnd_tr}" if rnd_tr != "All" else "All Rounds"
        mto_s = f"Moto {mto_tr}"  if mto_tr != "All" else "All Motos"
        fig.update_layout(
            title=f"Traffic Conditions — {rider_tr} | {cls_tr} | {yr_tr} | {rnd_s} | {mto_s}",
            xaxis=dict(title="Z-Score (lap time vs. moto average for that lap)", range=[x_lo, g99 * 1.15]),
            yaxis=dict(title="Density", showticklabels=False),
            height=500, margin=dict(l=60, r=60, t=60, b=60),
            legend=dict(title="Condition"), hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)
    else:
        st.warning(f"No data for {rider_tr} with selected filters.")

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 12b. Place journey — coloured by traffic state
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Place journey — coloured by traffic condition")

c1, c2, c3, c4 = st.columns(4)
with c1: yr_jn  = st.selectbox("Year",  all_years,       index=len(all_years) - 1, key="jn_yr")
with c2: cls_jn = st.selectbox("Class", all_classes,     key="jn_cls")
with c3: rnd_jn = st.selectbox("Round", specific_rounds, key="jn_rnd")
with c4: mto_jn = st.selectbox("Moto",  specific_motos,  key="jn_mto")

def eligible_journey(yr, cls, rnd, mto):
    m = (
        (traffic_base["year"]  == yr)  &
        (traffic_base["class_label"] == cls) &
        (traffic_base["round"] == rnd) &
        (traffic_base["moto"]  == mto)
    )
    return sorted(traffic_base[m]["name"].dropna().unique().tolist())

elig_jn  = eligible_journey(yr_jn, cls_jn, rnd_jn, mto_jn)
rider_jn = st.selectbox("Rider", elig_jn if elig_jn else ["— no data —"], key="jn_rdr")

if st.button("Update — place journey", type="primary", key="jn_btn") and elig_jn:
    m = (
        (traffic_base["year"]  == yr_jn)  &
        (traffic_base["class_label"] == cls_jn) &
        (traffic_base["round"] == rnd_jn) &
        (traffic_base["moto"]  == mto_jn) &
        (traffic_base["name"]  == rider_jn)
    )
    subset = traffic_base[m].sort_values("lap").copy()

    if subset.empty:
        st.warning(f"No data for {rider_jn}.")
    else:
        laps       = subset["lap"].tolist()
        places     = subset["place"].tolist()
        gap_ahead  = subset["gap_ahead"].tolist()
        gap_behind = subset["gap_behind"].tolist()
        conds      = subset["traffic_state"].tolist()
        max_place  = int(subset["place"].max())

        fig = go.Figure()
        legend_shown = set()

        for i in range(len(laps) - 1):
            state = conds[i]
            label, color = COND_META.get(
                "cond_" + state.lower().replace(" ", "_"),
                (state, "#aaaaaa")
            )
            ga = gap_ahead[i]
            gb = gap_behind[i]
            ahead_str  = f"{ga:.2f}s" if pd.notna(ga) else "—"
            behind_str = f"{gb:.2f}s" if pd.notna(gb) else "—"

            fig.add_trace(go.Scatter(
                x=[laps[i], laps[i + 1]],
                y=[places[i], places[i + 1]],
                mode="lines",
                line=dict(color=color, width=3),
                name=label,
                legendgroup=label,
                showlegend=(label not in legend_shown),
                customdata=[[ahead_str, behind_str], [ahead_str, behind_str]],
                hovertemplate=(
                    f"<b>{rider_jn}</b><br>"
                    "Lap: %{x}<br>Place: %{y}<br>"
                    f"Condition: {label}<br>"
                    "Gap ahead: %{customdata[0]}<br>"
                    "Gap behind: %{customdata[1]}<extra></extra>"
                ),
            ))
            legend_shown.add(label)

        fig.update_layout(
            title=f"Place Journey — {rider_jn} | {cls_jn} | {yr_jn} | Round {rnd_jn} | Moto {mto_jn}",
            xaxis=dict(title="Lap", tickmode="linear", dtick=1),
            yaxis=dict(
                title="Place", autorange="reversed",
                tickmode="linear", dtick=1,
                range=[max_place + 0.5, 0.5],
            ),
            height=500, margin=dict(l=60, r=60, t=60, b=60),
            legend=dict(title="Condition"), hovermode="closest",
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()
st.markdown("""
**Traffic state definitions** (2.0-second threshold, mutually exclusive and exhaustive)
- **Sandwiched**: someone within 2.0s ahead AND within 2.0s behind — simultaneous attack and defence.
- **Chasing**: someone within 2.0s ahead, no one within 2.0s behind — pursuing a pass with no defensive pressure.
- **Being Chased**: no one within 2.0s ahead (or leading), someone within 2.0s behind — defending with clear track ahead.
- **Clear Air**: no one within 2.0s in either direction — racing the clock in a gap.

**Other notes**
- The KDE chart only shows riders who led ≥3 laps in the filter window, ensuring all four conditions appear in meaningful quantities.
- Gap ahead/behind in the journey hover show the raw time gap to the rider immediately ahead/behind. "—" = leading or last.
""")

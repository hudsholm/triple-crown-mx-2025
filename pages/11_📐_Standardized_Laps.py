import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde, kurtosis
from scipy import stats
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(
    page_title="Triple Crown MX · Standardized Lap Times",
    page_icon="🏍️",
    layout="wide",
    initial_sidebar_state="expanded",
)

df = load_data()

RIDER_COLORS = ["#E8641A", "#1A7FE8"]

def hex_to_rgba(hex_color, opacity=0.08):
    h = hex_color.lstrip("#")
    r, g, b = int(h[0:2], 16), int(h[2:4], 16), int(h[4:6], 16)
    return f"rgba({r},{g},{b},{opacity})"

st.title("11. Standardizing Lap Times")
st.markdown("""
- % off best
- Z-score
""")

@st.cache_data
def build_pob_base(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"]   = base["lap"].astype(float)
    base["year"]  = base["year"].astype(str)
    base["round"] = base["round"].astype(str)
    base["moto"]  = base["moto"].astype(str)

    lap_best = (
        base.groupby(["race_id", "lap"], observed=True)["lap_time"]
        .min().rename("best_lap_time").reset_index()
    )
    base = base.merge(lap_best, on=["race_id", "lap"], how="left")
    base["pct_off_best"] = (
        (base["lap_time"] - base["best_lap_time"]) / base["best_lap_time"] * 100
    )

    lap_stats = (
        base.groupby(["race_id", "lap"], observed=True)["lap_time"]
        .agg(mean_lt="mean", std_lt="std").reset_index()
    )
    base = base.merge(lap_stats, on=["race_id", "lap"], how="left")
    base["z_score"] = np.where(
        base["std_lt"].notna() & (base["std_lt"] > 0),
        (base["lap_time"] - base["mean_lt"]) / base["std_lt"],
        np.nan,
    )
    return base[base["pct_off_best"] >= 0].copy()

@st.cache_data
def build_regression_table(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"]  = base["lap"].astype(float)
    base["year"] = base["year"].astype(str)

    lb = (
        base.groupby(["race_id", "lap"], observed=True)["lap_time"]
        .min().rename("best_lap_time").reset_index()
    )
    base = base.merge(lb, on=["race_id", "lap"], how="left")
    base["pct_off_best"] = (
        (base["lap_time"] - base["best_lap_time"]) / base["best_lap_time"] * 100
    )
    base = base[base["pct_off_best"] >= 0].copy()

    def calc_reg(grp):
        if len(grp) < 3:
            return pd.Series({"slope": np.nan, "intercept": np.nan})
        s, i, _, _, _ = stats.linregress(grp["lap"], grp["pct_off_best"])
        return pd.Series({"slope": s, "intercept": i})

    per_moto = (
        base.groupby(["race_id", "name", "class_label", "year"], observed=True)
        .apply(calc_reg, include_groups=False).reset_index()
    )
    season = (
        per_moto.groupby(["name", "class_label", "year"], observed=True)
        .agg(mean_slope=("slope", "mean"), std_slope=("slope", "std"),
             mean_intercept=("intercept", "mean"))
        .reset_index()
    )
    season["mean_slope"]     = season["mean_slope"].round(3)
    season["std_slope"]      = season["std_slope"].round(3)
    season["mean_intercept"] = season["mean_intercept"].round(2)
    season["year"]           = season["year"].astype(str)

    total_motos = (
        _df.drop_duplicates(subset=["race_id", "name"])
        .assign(year=lambda x: x["year"].astype(str))
        .groupby(["name", "class_label", "year"], observed=True)
        .size().reset_index(name="total_motos")
    )
    return season.merge(total_motos, on=["name", "class_label", "year"], how="left")

@st.cache_data
def build_zscore_table(_df):
    base = _df.dropna(subset=["lap", "lap_time"]).copy()
    base["lap"]  = base["lap"].astype(float)
    base["year"] = base["year"].astype(str)

    ls = (
        base.groupby(["race_id", "lap"], observed=True)["lap_time"]
        .agg(mean_lt="mean", std_lt="std").reset_index()
    )
    base = base.merge(ls, on=["race_id", "lap"], how="left")
    base = base[base["std_lt"].notna() & (base["std_lt"] > 0)].copy()
    base["z_score"] = (base["lap_time"] - base["mean_lt"]) / base["std_lt"]

    lap1_z = (
        base[base["lap"] == 1]
        .groupby(["name", "class_label", "year"], observed=True)["z_score"]
        .mean().rename("mean_z_lap1").reset_index()
    )
    overall_z = (
        base.groupby(["name", "class_label", "year"], observed=True)["z_score"]
        .mean().rename("mean_z_overall").reset_index()
    )
    total_motos = (
        _df.drop_duplicates(subset=["race_id", "name"])
        .assign(year=lambda x: x["year"].astype(str))
        .groupby(["name", "class_label", "year"], observed=True)
        .size().reset_index(name="total_motos")
    )
    t = total_motos.merge(lap1_z, on=["name", "class_label", "year"], how="left")
    t = t.merge(overall_z, on=["name", "class_label", "year"], how="left")
    t["mean_z_lap1"]    = t["mean_z_lap1"].round(3)
    t["mean_z_overall"] = t["mean_z_overall"].round(3)
    return t

pob_base   = build_pob_base(df)
reg_table  = build_regression_table(df)
z_table    = build_zscore_table(df)

all_years   = sorted(pob_base["year"].unique())
all_classes = ["450", "250", "WMX"]
all_rounds  = ["All"] + sorted(pob_base["round"].unique(), key=lambda x: int(x))
all_motos   = ["All"] + sorted(pob_base["moto"].unique(), key=lambda x: int(x))

def kde_plot(vals, color, name, metric="pct_off_best"):
    if len(vals) < 2:
        return []
    kde     = gaussian_kde(vals, bw_method=0.3)
    kurt    = kurtosis(vals, fisher=False)
    p95     = np.percentile(vals, 95)
    median  = np.median(vals)

    if metric == "pct_off_best":
        x_lo, x_hi = 0, np.percentile(vals, 99) * 1.1
    else:
        x1, x99 = np.percentile(vals, 1), np.percentile(vals, 99)
        x_lo = x1 * 1.1 if x1 < 0 else x1 * 0.9
        x_hi = x99 * 1.1

    x_range = np.linspace(x_lo, x_hi, 500)
    density = kde(x_range)
    med_d   = kde(np.array([median]))[0]

    hover_extra = (
        f"Kurtosis: {kurt:.2f}<br>P95: {p95:.2f}<br>Median: {median:.2f}"
    )
    x_label = "% off best: %{x:.2f}%" if metric == "pct_off_best" else "Z-Score: %{x:.2f}"

    return [
        go.Scatter(
            x=x_range, y=density, mode="lines", name=name,
            line=dict(color=color, width=2),
            fill="tozeroy", fillcolor=hex_to_rgba(color, 0.08),
            hovertemplate=f"<b>{name}</b><br>{x_label}<br>Density: %{{y:.4f}}<br>{hover_extra}<extra></extra>",
        ),
        go.Scatter(
            x=[median, median], y=[0, med_d], mode="lines",
            line=dict(color=color, width=1.5, dash="dot"),
            showlegend=False, hoverinfo="skip",
        ),
    ]

# ═══════════════════════════════════════════════════════════════════════════════
# 11a. % off best — KDE comparison
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("% off best lap — rider comparison (KDE)")

c1, c2, c3, c4 = st.columns(4)
with c1: yr_pob  = st.selectbox("Year",  all_years,   index=len(all_years) - 1, key="pob_yr")
with c2: cls_pob = st.selectbox("Class", all_classes, key="pob_cls")
with c3: rnd_pob = st.selectbox("Round", all_rounds,  key="pob_rnd")
with c4: mto_pob = st.selectbox("Moto",  all_motos,   key="pob_mto")

def eligible_pob(yr, cls, rnd, mto):
    m = (pob_base["year"] == yr) & (pob_base["class_label"] == cls)
    if rnd != "All": m &= pob_base["round"] == rnd
    if mto != "All": m &= pob_base["moto"]  == mto
    return sorted(pob_base[m]["name"].dropna().unique())

elig = eligible_pob(yr_pob, cls_pob, rnd_pob, mto_pob)
c1, c2 = st.columns(2)
with c1: rdr_a = st.selectbox("Rider A", elig, index=0 if elig else 0, key="pob_rdr_a")
with c2: rdr_b = st.selectbox("Rider B", elig, index=min(1, len(elig)-1), key="pob_rdr_b")

if st.button("Update — % off best", type="primary", key="pob_btn"):
    fig = go.Figure()
    all_p95 = []
    for rider, color in [(rdr_a, RIDER_COLORS[0]), (rdr_b, RIDER_COLORS[1])]:
        m = (pob_base["year"] == yr_pob) & (pob_base["class_label"] == cls_pob) & (pob_base["name"] == rider)
        if rnd_pob != "All": m &= pob_base["round"] == rnd_pob
        if mto_pob != "All": m &= pob_base["moto"]  == mto_pob
        vals = pob_base[m]["pct_off_best"].dropna().values
        if len(vals) >= 2:
            all_p95.append(np.percentile(vals, 95))
            for t in kde_plot(vals, color, rider, "pct_off_best"):
                fig.add_trace(t)
    rnd_s = f"Round {rnd_pob}" if rnd_pob != "All" else "All Rounds"
    mto_s = f"Moto {mto_pob}"  if mto_pob != "All" else "All Motos"
    fig.update_layout(
        title=f"% Off Best — {rdr_a} vs {rdr_b} | {cls_pob} | {yr_pob} | {rnd_s} | {mto_s}",
        xaxis=dict(title="% Off Best Lap", range=[0, max(all_p95) * 1.1] if all_p95 else None),
        yaxis=dict(title="Density", showticklabels=False),
        height=500, margin=dict(l=60, r=60, t=60, b=60),
        legend=dict(title="Rider"), hovermode="x unified",
    )
    st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 11b. % off best regression table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("% off best — regression slope ranking")
st.caption("Mean slope = avg change in % off best per lap. Negative = improving vs. field, positive = fading.")

REG_SORTS = {
    "Mean Slope — most improvement first":   ("mean_slope",     True),
    "Mean Slope — least improvement first":  ("mean_slope",     False),
    "Std Dev of Slopes — most consistent":   ("std_slope",      True),
    "Std Dev of Slopes — least consistent":  ("std_slope",      False),
    "Mean Intercept — lowest first":          ("mean_intercept", True),
    "Mean Intercept — highest first":         ("mean_intercept", False),
    "Total Motos":                            ("total_motos",    False),
}

c1, c2, c3, c4 = st.columns(4)
with c1: yr_reg   = st.selectbox("Year",    all_years,   index=len(all_years) - 1, key="reg_yr")
with c2: cls_reg  = st.selectbox("Class",   all_classes, key="reg_cls")
with c3: top_reg  = st.slider("Top N", 5, 50, 20, 5, key="reg_n")
with c4: sort_reg = st.selectbox("Sort by", list(REG_SORTS.keys()), key="reg_sort")

if st.button("Update — regression table", type="primary", key="reg_btn"):
    sub = reg_table[(reg_table["year"] == yr_reg) & (reg_table["class_label"] == cls_reg)]
    if sub.empty:
        st.warning("No data.")
    else:
        col, asc = REG_SORTS[sort_reg]
        result = (
            sub.sort_values(col, ascending=asc, na_position="last")
            .head(top_reg).reset_index(drop=True)
            .rename(columns={
                "name": "Rider", "total_motos": "Total Motos",
                "mean_slope": "Mean Slope", "std_slope": "Std Dev Slope",
                "mean_intercept": "Mean Intercept (%)",
            })
        )
        result.index += 1
        st.dataframe(result[["Rider", "Total Motos", "Mean Slope", "Std Dev Slope", "Mean Intercept (%)"]], use_container_width=False)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 11c. Z-score — KDE comparison
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Lap time z-score — rider comparison (KDE)")

c1, c2, c3, c4 = st.columns(4)
with c1: yr_z  = st.selectbox("Year",  all_years,   index=len(all_years) - 1, key="z_yr")
with c2: cls_z = st.selectbox("Class", all_classes, key="z_cls")
with c3: rnd_z = st.selectbox("Round", all_rounds,  key="z_rnd")
with c4: mto_z = st.selectbox("Moto",  all_motos,   key="z_mto")

def eligible_z(yr, cls, rnd, mto):
    m = (pob_base["year"] == yr) & (pob_base["class_label"] == cls)
    if rnd != "All": m &= pob_base["round"] == rnd
    if mto != "All": m &= pob_base["moto"]  == mto
    return sorted(pob_base[m]["name"].dropna().unique())

elig_z = eligible_z(yr_z, cls_z, rnd_z, mto_z)
c1, c2 = st.columns(2)
with c1: rdr_za = st.selectbox("Rider A", elig_z, index=0, key="z_rdr_a")
with c2: rdr_zb = st.selectbox("Rider B", elig_z, index=min(1, len(elig_z)-1), key="z_rdr_b")

if st.button("Update — z-score KDE", type="primary", key="z_btn"):
    fig = go.Figure()
    all_vals = []
    for rider, color in [(rdr_za, RIDER_COLORS[0]), (rdr_zb, RIDER_COLORS[1])]:
        m = (pob_base["year"] == yr_z) & (pob_base["class_label"] == cls_z) & (pob_base["name"] == rider)
        if rnd_z != "All": m &= pob_base["round"] == rnd_z
        if mto_z != "All": m &= pob_base["moto"]  == mto_z
        vals = pob_base[m]["z_score"].dropna().values
        if len(vals) >= 2:
            all_vals.extend(vals)
            for t in kde_plot(vals, color, rider, "z_score"):
                fig.add_trace(t)
    rnd_s = f"Round {rnd_z}" if rnd_z != "All" else "All Rounds"
    mto_s = f"Moto {mto_z}"  if mto_z != "All" else "All Motos"
    if all_vals:
        g1, g99 = np.percentile(all_vals, 1), np.percentile(all_vals, 99)
        x_lo = g1 * 1.15 if g1 < 0 else g1 * 0.85
        fig.update_layout(
            title=f"Z-Score — {rdr_za} vs {rdr_zb} | {cls_z} | {yr_z} | {rnd_s} | {mto_s}",
            xaxis=dict(title="Z-Score (lap time vs. moto average for that lap)", range=[x_lo, g99 * 1.15]),
            yaxis=dict(title="Density", showticklabels=False),
            height=500, margin=dict(l=60, r=60, t=60, b=60),
            legend=dict(title="Rider"), hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 11d. Z-score season ranking table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Z-score — season ranking table")
st.caption("Negative = faster than the field average. Mean Z — Lap 1 isolates starting performance.")

Z_SORTS = {
    "Mean Z — Lap 1 (best starters first)":   ("mean_z_lap1",    True),
    "Mean Z — Lap 1 (worst starters first)":  ("mean_z_lap1",    False),
    "Mean Z — Overall (best overall first)":   ("mean_z_overall", True),
    "Mean Z — Overall (worst overall first)":  ("mean_z_overall", False),
    "Total Motos":                              ("total_motos",    False),
}

c1, c2, c3, c4 = st.columns(4)
with c1: yr_zt   = st.selectbox("Year",    all_years,   index=len(all_years) - 1, key="zt_yr")
with c2: cls_zt  = st.selectbox("Class",   all_classes, key="zt_cls")
with c3: top_zt  = st.slider("Top N", 5, 50, 20, 5, key="zt_n")
with c4: sort_zt = st.selectbox("Sort by", list(Z_SORTS.keys()), key="zt_sort")

if st.button("Update — z-score table", type="primary", key="zt_btn"):
    sub = z_table[(z_table["year"] == yr_zt) & (z_table["class_label"] == cls_zt)]
    if sub.empty:
        st.warning("No data.")
    else:
        col, asc = Z_SORTS[sort_zt]
        result = (
            sub.sort_values(col, ascending=asc, na_position="last")
            .head(top_zt).reset_index(drop=True)
            .rename(columns={
                "name": "Rider", "total_motos": "Total Motos",
                "mean_z_lap1": "Mean Z — Lap 1", "mean_z_overall": "Mean Z — Overall",
            })
        )
        result.index += 1
        st.dataframe(result[["Rider", "Total Motos", "Mean Z — Lap 1", "Mean Z — Overall"]], use_container_width=False)

st.divider()
st.markdown("""
**Notes — % off best**
- Each lap is compared to the fastest lap at that exact lap number in the same moto — controls for track evolution (ruts, line development).
- Kurtosis above 3 = heavier tails than normal (more outlier laps); below 3 = more uniform pace.
- Regression slope: avg change in % off best per lap. Low/negative = rider stays closer to best as moto progresses.

**Notes — z-score**
- Z-score = (rider lap time − mean of all riders' lap N) ÷ std dev of lap N. Negative = faster than field average.
- Computed per lap per moto; laps where only one rider was present (std = 0) are dropped.
- Unlike % off best, z-score is not bounded at 0 and gives a continuous faster/slower scale relative to the field.
""")

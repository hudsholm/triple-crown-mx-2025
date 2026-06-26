import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data, load_weather

st.set_page_config(page_title="Triple Crown MX · Weather", page_icon="🏍️", layout="wide")

df      = load_data()
weather = load_weather()

TCMX_POINTS = {1:25,2:22,3:20,4:18,5:16,6:15,7:14,8:13,9:12,10:11,
               11:10,12:9,13:8,14:7,15:6,16:5,17:4,18:3,19:2,20:1}
RAIN_THRESHOLD = 1.0
YEAR_COLORS    = {"2024":"#1A7FE8","2025":"#E8641A"}

def hex_to_rgba(h,op=0.08):
    h=h.lstrip("#"); r,g,b=int(h[0:2],16),int(h[2:4],16),int(h[4:6],16)
    return f"rgba({r},{g},{b},{op})"

def fmt_time(s):
    if pd.isna(s): return "—"
    m=int(s//60); return f"{m}:{s%60:05.2f}"

st.title("15. Weather Effects")
st.markdown("Year-over-year lap time comparisons at repeat venues, and wet vs. dry pace for top riders.")

@st.cache_data
def build_wx_base(_df, _w):
    w = _w.copy()
    w["date"] = pd.to_datetime(w["date"]).dt.strftime("%Y-%m-%d")

    base = _df.copy()
    base["date"] = pd.to_datetime(base["date"]).dt.strftime("%Y-%m-%d")
    base["lap"]  = base["lap"].astype(float)
    base["year"] = base["year"].astype(str)
    base = base.merge(w[["track","date","max_temp_c","precipitation_mm"]], on=["date","track"], how="left")
    base = base[base["lap"] > 1].copy()

    # Top 5 per track per year per class by combined moto points
    moto_pts = (base.drop_duplicates(subset=["race_id","name"]).dropna(subset=["finish_position"]).copy())
    moto_pts["finish_position"] = moto_pts["finish_position"].astype(int)
    moto_pts["moto_points"] = moto_pts["finish_position"].map(TCMX_POINTS).fillna(0)
    round_pts = (moto_pts.groupby(["name","class_label","year","track"],observed=True)
                 ["moto_points"].sum().reset_index(name="total_points"))
    top5 = (round_pts.sort_values(["class_label","year","track","total_points"],ascending=[True,True,True,False])
            .groupby(["class_label","year","track"],observed=True).head(5)[["name","class_label","year","track"]])
    base_top5 = base.merge(top5, on=["name","class_label","year","track"], how="inner")

    # pct_off_best within top5
    lb = (base_top5.groupby(["race_id","lap"],observed=True)["lap_time"]
          .min().rename("best_lt").reset_index())
    base_top5 = base_top5.merge(lb, on=["race_id","lap"], how="left")
    base_top5["pct_off_best"] = (base_top5["lap_time"]-base_top5["best_lt"])/base_top5["best_lt"]*100
    base_top5 = base_top5[base_top5["pct_off_best"]>=0].copy()

    # z-score vs full field (for wet/dry rider chart)
    ls = (base.groupby(["race_id","lap"],observed=True)["lap_time"]
          .agg(mean_lt="mean",std_lt="std").reset_index())
    base_z = base.merge(ls, on=["race_id","lap"], how="left")
    base_z = base_z[base_z["std_lt"].notna()&(base_z["std_lt"]>0)].copy()
    base_z["z_score"] = (base_z["lap_time"]-base_z["mean_lt"])/base_z["std_lt"]
    base_z["is_wet"]  = base_z["precipitation_mm"]>=RAIN_THRESHOLD

    base_top5["is_wet"] = base_top5["precipitation_mm"]>=RAIN_THRESHOLD
    venue_years = (base_top5.drop_duplicates(subset=["track","year"])
                   .groupby("track",observed=True)["year"].nunique())
    repeated = venue_years[venue_years>1].index.tolist()

    return base_top5, base_z, repeated

wx_top5, wx_z, repeated_venues = build_wx_base(df, weather)

all_classes = ["450","250","WMX"]
all_years   = ["2024","2025"]

# ═══════════════════════════════════════════════════════════════════════════════
# 15a. Year-over-year KDE at repeat venues
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Year-over-year lap times at repeat venues")
st.caption("Top 5 finishers per track per year per class. Lap 1 excluded.")

c1,c2 = st.columns(2)
with c1: cls_yoy = st.selectbox("Class", all_classes, key="yoy_cls")
with c2: trk_yoy = st.selectbox("Track", sorted(repeated_venues) if repeated_venues else ["— no repeat venues —"], key="yoy_trk")

if st.button("Update — year-over-year", type="primary", key="yoy_btn") and repeated_venues:
    sub = wx_top5[(wx_top5["class_label"]==cls_yoy)&(wx_top5["track"]==trk_yoy)].dropna(subset=["lap_time"])
    fig = go.Figure(); all_vals=[]
    for yr, color in YEAR_COLORS.items():
        vals = sub[sub["year"]==yr]["lap_time"].dropna().values
        if len(vals)<5: continue
        all_vals.extend(vals)
        kde=gaussian_kde(vals,bw_method=0.3)
        x=np.linspace(np.percentile(vals,1),np.percentile(vals,99),400)
        d=kde(x); med=np.median(vals)
        temp  = sub[sub["year"]==yr]["max_temp_c"].iloc[0]
        prec  = sub[sub["year"]==yr]["precipitation_mm"].iloc[0]
        fig.add_trace(go.Scatter(x=x,y=d,mode="lines",
            name=f"{trk_yoy} {yr} (max {temp:.1f}°C, {prec:.1f}mm)",
            line=dict(color=color,width=2,dash="solid" if yr=="2024" else "dash"),
            fill="tozeroy",fillcolor=hex_to_rgba(color,0.05),
            hovertemplate=f"<b>{trk_yoy} {yr}</b><br>Lap: %{{x:.2f}}s<extra></extra>"))
        fig.add_trace(go.Scatter(x=[med,med],y=[0,kde(np.array([med]))[0]],mode="lines",
            line=dict(color=color,width=1,dash="dot"),showlegend=False,hoverinfo="skip"))
    if all_vals:
        p1,p99=np.percentile(all_vals,1),np.percentile(all_vals,99)
        tv=np.linspace(p1,p99,7)
        fig.update_layout(
            title=f"Year-over-Year — {cls_yoy} | {trk_yoy} | Solid=2024, Dashed=2025",
            xaxis=dict(title="Lap Time",range=[p1*0.998,p99*1.002],
                       tickvals=tv,ticktext=[fmt_time(t) for t in tv]),
            yaxis=dict(title="Density",showticklabels=False),
            height=550,margin=dict(l=60,r=60,t=60,b=60),
            legend=dict(title="Year"),hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 15b. Wet vs dry % off best — KDE
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Wet vs dry — % off best (top 5 field)")

c1,c2 = st.columns(2)
with c1: cls_wd = st.selectbox("Class", all_classes, key="wd_cls")
with c2: yr_wd  = st.selectbox("Year",  all_years,   index=1, key="wd_yr")

if st.button("Update — wet/dry KDE", type="primary", key="wd_btn"):
    sub = wx_top5[(wx_top5["class_label"]==cls_wd)&(wx_top5["year"]==yr_wd)].dropna(subset=["pct_off_best","is_wet"])
    fig=go.Figure()
    for label,mask_val,color in [(f"Dry (<{RAIN_THRESHOLD}mm)",False,"#1A7FE8"),(f"Wet (≥{RAIN_THRESHOLD}mm)",True,"#E8641A")]:
        vals=sub[sub["is_wet"]==mask_val]["pct_off_best"].values
        if len(vals)<5: continue
        kde=gaussian_kde(vals,bw_method=0.3)
        x=np.linspace(0,np.percentile(vals,99),400); d=kde(x); med=np.median(vals)
        fig.add_trace(go.Scatter(x=x,y=d,mode="lines",name=f"{label} (n={len(vals)}, med={med:.1f}%)",
            line=dict(color=color,width=2),fill="tozeroy",fillcolor=hex_to_rgba(color,0.08),
            hovertemplate=f"<b>{label}</b><br>% off best: %{{x:.2f}}%<extra></extra>"))
        fig.add_trace(go.Scatter(x=[med,med],y=[0,kde(np.array([med]))[0]],mode="lines",
            line=dict(color=color,width=1.5,dash="dot"),showlegend=False,hoverinfo="skip"))
    wet_rounds = wx_top5[(wx_top5["is_wet"])&(wx_top5["year"]==yr_wd)][["track","precipitation_mm"]].drop_duplicates()
    wet_str = ", ".join(f"{r['track']} ({r['precipitation_mm']}mm)" for _,r in wet_rounds.iterrows())
    fig.update_layout(title=f"Wet vs Dry % Off Best — {cls_wd} | {yr_wd}",
        xaxis=dict(title="% Off Best (lap-by-lap)"),
        yaxis=dict(title="Density",showticklabels=False),
        height=500,margin=dict(l=60,r=60,t=60,b=60),
        legend=dict(title="Condition"),hovermode="x unified")
    st.plotly_chart(fig, use_container_width=True)
    if wet_str: st.caption(f"Wet rounds: {wet_str}")

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 15c. Top 10 wet vs dry median z-score bar chart
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Top 10 riders — wet vs dry median z-score")
st.caption("Z-scores computed against the full field per lap. Negative = faster than field average.")

@st.cache_data
def build_season_pts(_df):
    base = _df.drop_duplicates(subset=["race_id","name"]).dropna(subset=["finish_position"]).copy()
    base["year"]            = base["year"].astype(str)
    base["round"]           = base["round"].astype(int)
    base["moto"]            = base["moto"].astype(int)
    base["finish_position"] = base["finish_position"].astype(int)
    base["moto_points"]     = base["finish_position"].map(TCMX_POINTS).fillna(0)
    rp = (base.groupby(["name","class_label","year","round"],observed=True)
          ["moto_points"].sum().reset_index(name="combined_points"))
    return (rp.groupby(["name","class_label","year"],observed=True)["combined_points"]
            .sum().reset_index(name="season_points"))

season_pts = build_season_pts(df)

c1,c2 = st.columns(2)
with c1: cls_wz = st.selectbox("Class", all_classes, key="wz_cls")
with c2: yr_wz  = st.selectbox("Year",  all_years,   index=1, key="wz_yr")

if st.button("Update — top 10 wet/dry", type="primary", key="wz_btn"):
    top10 = (season_pts[(season_pts["class_label"]==cls_wz)&(season_pts["year"]==yr_wz)]
             .sort_values("season_points",ascending=False).head(10)["name"].tolist())
    sub = wx_z[(wx_z["class_label"]==cls_wz)&(wx_z["year"]==yr_wz)&(wx_z["name"].isin(top10))].dropna(subset=["z_score","is_wet"])
    rows=[]
    for rider in top10:
        r=sub[sub["name"]==rider]
        dry_z=r[~r["is_wet"]]["z_score"].dropna(); wet_z=r[r["is_wet"]]["z_score"].dropna()
        rows.append({"rider":rider,"dry_median_z":dry_z.median() if len(dry_z)>=2 else np.nan,
                     "wet_median_z":wet_z.median() if len(wet_z)>=2 else np.nan,
                     "dry_n":len(dry_z),"wet_n":len(wet_z)})
    result=pd.DataFrame(rows).dropna(subset=["dry_median_z"]).sort_values("dry_median_z")
    fig=go.Figure()
    fig.add_trace(go.Bar(y=result["rider"],x=result["dry_median_z"],name="Dry",orientation="h",
        marker_color="#1A7FE8",customdata=result["dry_n"],
        hovertemplate="<b>%{y}</b><br>Dry median z: %{x:.3f}<br>n laps: %{customdata}<extra></extra>"))
    fig.add_trace(go.Bar(y=result["rider"],x=result["wet_median_z"],name="Wet",orientation="h",
        marker_color="#E8641A",customdata=result["wet_n"],
        hovertemplate="<b>%{y}</b><br>Wet median z: %{x:.3f}<br>n laps: %{customdata}<extra></extra>"))
    fig.add_vline(x=0,line_width=1,line_dash="dash",line_color="grey")
    fig.update_layout(title=f"Wet vs Dry Median Z-Score — {cls_wz} | {yr_wz}",
        xaxis=dict(title="Median Z-Score (negative = faster than field avg)"),
        barmode="group",height=520,margin=dict(l=180,r=60,t=60,b=60),
        legend=dict(title="Condition"),hovermode="closest")
    st.plotly_chart(fig, use_container_width=True)
    wet_rounds=wx_z[(wx_z["is_wet"])&(wx_z["year"]==yr_wz)][["track","precipitation_mm"]].drop_duplicates()
    wet_str=", ".join(f"{r['track']} ({r['precipitation_mm']}mm)" for _,r in wet_rounds.iterrows())
    no_wet=[r["rider"] for r in rows if r["wet_n"]<2]
    if wet_str: st.caption(f"Wet rounds: {wet_str} | Threshold: ≥{RAIN_THRESHOLD}mm")
    if no_wet:  st.caption(f"Insufficient wet laps (< 2) for: {', '.join(no_wet)} — wet bar not shown.")

st.divider()
st.markdown("""
**Notes**
- Year-over-year restricted to top 5 finishers per track per year. Lap 1 excluded.
- Wet/dry threshold: ≥1.0mm precipitation on race day (from Environment Canada climate stations).
- The field z-score chart uses the **full field** (not just top 10) as the benchmark for each lap, then filters to the top 10 by season points for display.
- Riders with fewer than 2 wet laps are excluded from the wet bar.
""")

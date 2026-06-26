import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(page_title="Triple Crown MX · Gender Gap", page_icon="🏍️", layout="wide")

df = load_data()

CLASS_COLORS = {"250": "#1A7FE8", "WMX": "#E8641A"}

def hex_to_rgba(h, op=0.08):
    h = h.lstrip("#"); r,g,b = int(h[0:2],16),int(h[2:4],16),int(h[4:6],16)
    return f"rgba({r},{g},{b},{op})"

def fmt_time(s):
    if pd.isna(s): return "—"
    m = int(s//60); return f"{m}:{s%60:06.3f}"

st.title("13. Gender Gap")
st.markdown("Lap-time comparison between the 250 and WMX classes (same bike), plus a Jamie Astudillo cross-class case study.")

@st.cache_data
def build_gg_base(_df):
    base = _df.dropna(subset=["lap","lap_time"]).copy()
    base["lap"]   = base["lap"].astype(float)
    base["year"]  = base["year"].astype(str)
    base["round"] = base["round"].astype(str)
    base["moto"]  = base["moto"].astype(str)
    base = base[base["class_label"].isin(["250","WMX"]) & (base["lap"] > 1)].copy()
    # Drop WMX R2 moto 1 (separate race day)
    base = base[~((base["round"]=="2")&(base["class_label"]=="WMX")&(base["moto"]=="1"))].copy()
    base.loc[(base["round"]=="2")&(base["class_label"]=="WMX")&(base["moto"]=="2"),"moto"] = "1"
    base.loc[(base["round"]=="2")&(base["class_label"]=="WMX")&(base["moto"]=="3"),"moto"] = "2"
    top10 = (
        base.dropna(subset=["finish_position"])
        .groupby(["race_id","name"],observed=True)["finish_position"].first().reset_index()
    )
    top10 = top10[top10["finish_position"]<=10][["race_id","name"]]
    return base.merge(top10, on=["race_id","name"], how="inner")

gg_base = build_gg_base(df)
gg_years  = sorted(gg_base["year"].unique())
gg_rounds = ["All"] + sorted(gg_base["round"].unique(), key=int)
gg_motos  = ["All"] + sorted(gg_base["moto"].unique(),  key=int)

# ── 13a. KDE distribution ──────────────────────────────────────────────────────
st.subheader("250 vs WMX lap time distributions — top 10 finishers")

c1,c2,c3 = st.columns(3)
with c1: yr_gg  = st.selectbox("Year",  gg_years,  index=len(gg_years)-1)
with c2: rnd_gg = st.selectbox("Round", gg_rounds)
with c3: mto_gg = st.selectbox("Moto",  gg_motos)

if st.button("Update — KDE", type="primary", key="gg_kde"):
    mask = gg_base["year"] == yr_gg
    if rnd_gg != "All": mask &= gg_base["round"] == rnd_gg
    if mto_gg != "All": mask &= gg_base["moto"]  == mto_gg
    subset = gg_base[mask]
    fig = go.Figure(); all_vals = []
    for cls, color in CLASS_COLORS.items():
        vals = subset[subset["class_label"]==cls]["lap_time"].dropna().values
        if len(vals)<2: continue
        all_vals.extend(vals)
        kde = gaussian_kde(vals, bw_method=0.3)
        x = np.linspace(np.percentile(vals,1)*0.995, np.percentile(vals,99)*1.005, 500)
        d = kde(x); med = np.median(vals)
        fig.add_trace(go.Scatter(x=x, y=d, mode="lines", name=f"{cls} (n={len(vals)}, med={fmt_time(med)})",
            line=dict(color=color,width=2), fill="tozeroy", fillcolor=hex_to_rgba(color,0.08),
            hovertemplate=f"<b>{cls}</b><br>Lap: %{{x:.3f}}s<br>Density: %{{y:.4f}}<extra></extra>"))
        fig.add_trace(go.Scatter(x=[med,med], y=[0,kde(np.array([med]))[0]], mode="lines",
            line=dict(color=color,width=1.5,dash="dot"), showlegend=False, hoverinfo="skip"))
    if all_vals:
        glo1,glo99 = np.percentile(all_vals,1), np.percentile(all_vals,99)
        tv = np.linspace(glo1,glo99,8)
        fig.update_layout(
            title=f"Gender Gap — 250 vs WMX | {yr_gg} | {'Round '+rnd_gg if rnd_gg!='All' else 'All Rounds'} | {'Moto '+mto_gg if mto_gg!='All' else 'All Motos'}",
            xaxis=dict(title="Lap Time", range=[glo1*0.995,glo99*1.005],
                       tickvals=tv, ticktext=[fmt_time(t) for t in tv]),
            yaxis=dict(title="Density", showticklabels=False),
            height=500, margin=dict(l=60,r=60,t=60,b=60),
            legend=dict(title="Class"), hovermode="x unified",
        )
        st.plotly_chart(fig, use_container_width=True)

st.divider()

# ── 13b. Jamie Astudillo case study ───────────────────────────────────────────
st.subheader("Jamie Astudillo — 2025 cross-class case study")
st.caption("Jamie raced both WMX and 250 in 2025, making her the only rider with direct cross-class data.")

@st.cache_data
def build_jamie_table(_df):
    JAMIE = "Jamie Astudillo"
    base = _df[_df["year"].astype(str)=="2025"].copy()
    base["lap"]   = base["lap"].astype(float)
    base["round"] = base["round"].astype(str)
    base["moto"]  = base["moto"].astype(str)
    base["year"]  = base["year"].astype(str)

    for cls in ["WMX","250"]:
        ls = (base[base["class_label"]==cls]
              .groupby(["race_id","lap"],observed=True)["lap_time"]
              .agg(mean_lt="mean", std_lt="std").reset_index()
              .rename(columns={"mean_lt":f"mean_{cls}","std_lt":f"std_{cls}"}))
        base = base.merge(ls, on=["race_id","lap"], how="left")

    base["z_WMX"] = np.where(base["class_label"]=="WMX",
        (base["lap_time"]-base["mean_WMX"])/base["std_WMX"].replace(0,np.nan), np.nan)
    base["z_250"] = np.where(base["class_label"]=="250",
        (base["lap_time"]-base["mean_250"])/base["std_250"].replace(0,np.nan), np.nan)

    base["mapped_moto"] = base["moto"]
    base.loc[(base["round"]=="2")&(base["class_label"]=="WMX")&(base["moto"]=="2"),"mapped_moto"]="1"
    base.loc[(base["round"]=="2")&(base["class_label"]=="WMX")&(base["moto"]=="3"),"mapped_moto"]="2"

    jamie_wmx = base[(base["name"]==JAMIE)&(base["class_label"]=="WMX")]
    wmx_motos = (jamie_wmx.drop_duplicates(subset=["race_id"])
                 [["race_id","round","moto","mapped_moto","track","finish_position"]]
                 .sort_values(["round","moto"]))

    rows=[]
    for _, mr in wmx_motos.iterrows():
        race_id, rnd, mto, mmap, track = mr["race_id"],mr["round"],mr["moto"],mr["mapped_moto"],mr["track"]
        wmx_laps = base[(base["name"]==JAMIE)&(base["class_label"]=="WMX")&(base["race_id"]==race_id)].dropna(subset=["lap_time"])
        wmx_z = wmx_laps["z_WMX"].dropna().mean(); wmx_avg = wmx_laps["lap_time"].mean()
        jamie_max = wmx_laps["lap"].max()
        is_r2m1 = (rnd=="2" and mto=="1")
        if not is_r2m1:
            j250 = base[(base["name"]==JAMIE)&(base["class_label"]=="250")&(base["round"]==rnd)&(base["moto"]==mmap)].dropna(subset=["lap_time"])
            p250_place = j250["finish_position"].dropna().iloc[0] if not j250.empty and j250["finish_position"].notna().any() else np.nan
            p250_avg   = j250["lap_time"].mean() if not j250.empty else np.nan
            p250_z     = j250["z_250"].dropna().mean() if not j250.empty else np.nan
            r250 = base[(base["class_label"]=="250")&(base["round"]==rnd)&(base["moto"]==mmap)]
            r250_id = r250["race_id"].iloc[0] if not r250.empty else None
            if r250_id and not pd.isna(jamie_max):
                r250_sub = base[(base["class_label"]=="250")&(base["race_id"]==r250_id)&(base["lap"]<=jamie_max)].dropna(subset=["lap_time"])
                r250_tot = base[(base["class_label"]=="250")&(base["race_id"]==r250_id)]["lap"].max()
                cumul = r250_sub.groupby("name",observed=True)["lap_time"].sum().reset_index(name="ct")
                jamie_ct = wmx_laps[wmx_laps["lap"]<=jamie_max]["lap_time"].sum()
                hyp_pl = int((cumul["ct"]<jamie_ct).sum())+1
                hyp_note = f"Lap {int(jamie_max)} of {int(r250_tot)}"
            else: hyp_pl,hyp_note = np.nan,"—"
        else:
            p250_place=p250_avg=p250_z=hyp_pl=np.nan; hyp_note="—"
        rows.append({"Round":rnd,"Moto":mto,"Track":track,
            "WMX Place":int(mr["finish_position"]) if pd.notna(mr["finish_position"]) else "—",
            "WMX Avg Lap":fmt_time(wmx_avg),"WMX Avg Z":f"{wmx_z:.3f}" if pd.notna(wmx_z) else "—",
            "250 Place":int(p250_place) if pd.notna(p250_place) else "—",
            "250 Avg Lap":fmt_time(p250_avg),"250 Avg Z":f"{p250_z:.3f}" if pd.notna(p250_z) else "—",
            "Hyp. 250 Place":int(hyp_pl) if pd.notna(hyp_pl) else "—","Hyp. Laps":hyp_note})
    return pd.DataFrame(rows)

jamie_df = build_jamie_table(df)
st.dataframe(jamie_df, hide_index=True, use_container_width=False)
st.caption("Hyp. 250 Place = where Jamie's WMX cumulative lap times would have placed her in the simultaneous 250 moto.")

st.divider()
st.markdown("""
**Notes**
- Both classes ride 250-displacement bikes on the same tracks on the same weekends — the cleanest gender comparison the data permits.
- Restricted to top 10 finishers per moto. Removes back-markers so the comparison reflects competitive pace.
- WMX Round 2 runs three motos; moto 1 is on the men's day off. It is dropped, and motos 2/3 are remapped to 1/2 to align with the 250 same-day structure.
- Hypothetical placement uses cumulative time (not average lap), so traffic, mistakes, and fade are all captured.
- Z-scores are independent per class — a z of −1 in WMX and 0 in 250 tell different stories about the same rider on the same day.
""")

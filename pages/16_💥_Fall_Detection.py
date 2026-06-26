import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
from scipy.stats import gaussian_kde
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(page_title="Triple Crown MX · Fall Detection", page_icon="🏍️", layout="wide")

df = load_data()

TCMX_POINTS = {1:25,2:22,3:20,4:18,5:16,6:15,7:14,8:13,9:12,10:11,
               11:10,12:9,13:8,14:7,15:6,16:5,17:4,18:3,19:2,20:1}

def fmt_time(s):
    if pd.isna(s): return "—"
    m=int(s//60); return f"{m}:{s%60:06.3f}"

def hex_to_rgba(h,op=0.08):
    h=h.lstrip("#"); r,g,b=int(h[0:2],16),int(h[2:4],16),int(h[4:6],16)
    return f"rgba({r},{g},{b},{op})"

st.title("16. Fall Detection")
st.markdown("""
- Distinguishing recoverable and unrecoverable anomalies
- Simulated no-DNF standings
""")

@st.cache_data
def build_anom_base(_df):
    base = _df.dropna(subset=["lap","lap_time","place","finish_position"]).copy()
    base["lap"]             = base["lap"].astype(float)
    base["place"]           = base["place"].astype(float)
    base["finish_position"] = base["finish_position"].astype(float)
    base["year"]            = base["year"].astype(str)
    base["round"]           = base["round"].astype(str)
    base["moto"]            = base["moto"].astype(str)

    rs = (base.groupby(["race_id","name"],observed=True)["lap_time"]
          .agg(rider_median="median",rider_std="std",rider_laps="count").reset_index())
    base = base.merge(rs, on=["race_id","name"], how="left")
    base = base[(base["rider_laps"]>=3)&base["rider_std"].notna()&(base["rider_std"]>0)].copy()
    base["rider_z"] = (base["lap_time"]-base["rider_median"])/base["rider_std"]

    base = base.sort_values(["race_id","name","lap"])
    base["prev_place"] = base.groupby(["race_id","name"],observed=True)["place"].shift(1)
    base["place_drop"] = base["place"]-base["prev_place"]

    llp = (base.sort_values(["race_id","name","lap"])
           .groupby(["race_id","name"],observed=True)
           .agg(last_lap_num=("lap","max"),last_lap_place=("place","last")).reset_index())
    mml = (base.groupby("race_id",observed=True)["lap"].max().rename("moto_max_lap").reset_index())
    base = base.merge(llp, on=["race_id","name"], how="left")
    base = base.merge(mml, on="race_id", how="left")
    return base

anom_base = build_anom_base(df)

def detect_anomalies(base, threshold):
    rec = base[
        (base["rider_z"]>=threshold) & (base["place_drop"]>=2) & (base["lap"]>1) &
        (base["last_lap_num"]==base["moto_max_lap"]) &
        (abs(base["finish_position"]-base["last_lap_place"])<2)
    ].copy()
    rec["anomaly_type"] = "Recoverable"

    unrec = base.drop_duplicates(subset=["race_id","name"]).copy()
    unrec = unrec[abs(unrec["finish_position"]-unrec["last_lap_place"])>=2].copy()
    unrec["anomaly_type"] = "Unrecoverable"
    unrec["lap"]        = unrec["last_lap_num"]
    unrec["rider_z"]    = (unrec["lap_time"]-unrec["rider_median"])/unrec["rider_std"]
    unrec["place_drop"] = unrec["finish_position"]-unrec["last_lap_place"]
    return rec, unrec

all_years   = sorted(anom_base["year"].unique())
all_classes = ["450","250","WMX"]
all_rounds  = ["All"]+sorted(anom_base["round"].unique(),key=int)
all_motos   = ["All"]+sorted(anom_base["moto"].unique(),key=int)

# ═══════════════════════════════════════════════════════════════════════════════
# 16a. Anomaly table
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Anomaly log")

c1,c2,c3,c4 = st.columns(4)
with c1: yr_an   = st.selectbox("Year",  all_years,   index=len(all_years)-1,  key="an_yr")
with c2: cls_an  = st.selectbox("Class", all_classes, key="an_cls")
with c3: rnd_an  = st.selectbox("Round", all_rounds,  key="an_rnd")
with c4: mto_an  = st.selectbox("Moto",  all_motos,   key="an_mto")

c5,c6 = st.columns(2)
with c5: thr_an  = st.slider("SD threshold", 1.5, 4.0, 2.5, 0.25, key="an_thr")
with c6: typ_an  = st.selectbox("Show", ["Both","Recoverable","Unrecoverable"], key="an_typ")

if st.button("Update — anomaly log", type="primary", key="an_btn"):
    rec, unrec = detect_anomalies(anom_base, thr_an)

    def filt(sub):
        m = (sub["year"]==yr_an)&(sub["class_label"]==cls_an)
        if rnd_an!="All": m &= sub["round"]==rnd_an
        if mto_an!="All": m &= sub["moto"]==mto_an
        return sub[m]

    rec_f   = filt(rec)
    unrec_f = filt(unrec)

    if typ_an in ["Both","Recoverable"]:
        st.markdown(f"**Recoverable anomalies (n={len(rec_f)})**")
        if rec_f.empty:
            st.info("None found.")
        else:
            rd = rec_f[["name","round","moto","track","lap","lap_time","rider_median","rider_z","prev_place","place","place_drop"]].copy()
            rd["lap_time"]    = rd["lap_time"].apply(fmt_time)
            rd["rider_median"]= rd["rider_median"].apply(fmt_time)
            rd["rider_z"]     = rd["rider_z"].round(2)
            rd["place_drop"]  = rd["place_drop"].astype(int)
            rd["lap"]         = rd["lap"].astype(int)
            rd["prev_place"]  = rd["prev_place"].astype(int)
            rd["place"]       = rd["place"].astype(int)
            rd = rd.rename(columns={"name":"Rider","round":"Rd","moto":"Moto","track":"Track",
                "lap":"Lap","lap_time":"Anomaly Lap","rider_median":"Median Lap",
                "rider_z":"Rider Z","prev_place":"Place Before","place":"Place After","place_drop":"Places Lost"})
            st.dataframe(rd.reset_index(drop=True), hide_index=True, use_container_width=False)

    if typ_an in ["Both","Unrecoverable"]:
        st.markdown(f"**Unrecoverable anomalies / DNFs (n={len(unrec_f)})**")
        if unrec_f.empty:
            st.info("None found.")
        else:
            ud = unrec_f[["name","round","moto","track","last_lap_num","moto_max_lap","last_lap_place","finish_position","place_drop"]].copy()
            for c in ["place_drop","last_lap_num","moto_max_lap","last_lap_place","finish_position"]:
                ud[c] = ud[c].astype(int)
            ud = ud.rename(columns={"name":"Rider","round":"Rd","moto":"Moto","track":"Track",
                "last_lap_num":"Last Lap","moto_max_lap":"Moto Laps",
                "last_lap_place":"Place at Last Lap","finish_position":"Finish","place_drop":"Places Lost"})
            st.dataframe(ud.reset_index(drop=True), hide_index=True, use_container_width=False)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 16b. Anomaly visualisations
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("Anomaly characteristics")

ANOM_COLORS = {"Recoverable":"#1A7FE8","Unrecoverable":"#E8641A"}

c1,c2,c3 = st.columns(3)
with c1: yr_a2  = st.selectbox("Year",  all_years,   index=len(all_years)-1, key="a2_yr")
with c2: cls_a2 = st.selectbox("Class", all_classes, key="a2_cls")
with c3: thr_a2 = st.slider("SD threshold", 1.5, 4.0, 2.5, 0.25, key="a2_thr")
view_a2 = st.selectbox("View", ["Rider frequency","When in moto (KDE)"], key="a2_view")

if st.button("Update — characteristics", type="primary", key="a2_btn"):
    rec2, unrec2 = detect_anomalies(anom_base, thr_a2)
    def filt2(sub): return sub[(sub["year"]==yr_a2)&(sub["class_label"]==cls_a2)]
    rec2,unrec2 = filt2(rec2),filt2(unrec2)
    total = len(rec2)+len(unrec2)
    st.caption(f"Recoverable: {len(rec2)}  |  Unrecoverable: {len(unrec2)}  |  SD threshold: {thr_a2}")

    if total==0:
        st.warning("No anomalies for selected filters.")
    elif view_a2=="Rider frequency":
        all_names = pd.concat([rec2[["name"]],unrec2[["name"]]]).drop_duplicates()["name"].tolist()
        motos_e = (anom_base[(anom_base["year"]==yr_a2)&(anom_base["class_label"]==cls_a2)&(anom_base["name"].isin(all_names))]
                   .drop_duplicates(subset=["race_id","name"]).groupby("name",observed=True).size().reset_index(name="motos"))
        fig=go.Figure()
        for label,sub,color in [("Recoverable",rec2,ANOM_COLORS["Recoverable"]),("Unrecoverable",unrec2,ANOM_COLORS["Unrecoverable"])]:
            if sub.empty: continue
            cnt=(sub.groupby("name",observed=True).size().reset_index(name="count")
                 .merge(motos_e,on="name",how="left").sort_values("count",ascending=True))
            fig.add_trace(go.Bar(y=cnt["name"],x=cnt["count"],name=label,orientation="h",
                marker_color=color,customdata=cnt["motos"].fillna(0).astype(int),
                hovertemplate="<b>%{y}</b><br>"+label+": %{x}<br>Motos entered: %{customdata}<extra></extra>"))
        fig.update_layout(title="Anomaly Frequency by Rider",xaxis=dict(title="Count",dtick=1),
            barmode="group",height=max(400,len(all_names)*20),
            margin=dict(l=180,r=60,t=60,b=60),legend=dict(title="Type"),hovermode="closest")
        st.plotly_chart(fig,use_container_width=True)
    else:
        fig=go.Figure(); all_vals=[]
        for label,sub,color in [("Recoverable",rec2,ANOM_COLORS["Recoverable"]),("Unrecoverable",unrec2,ANOM_COLORS["Unrecoverable"])]:
            if sub.empty: continue
            lap_col = "lap" if label=="Recoverable" else "last_lap_num"
            pct=(sub[lap_col]/sub["moto_max_lap"]*100).dropna().values
            if len(pct)<2: continue
            all_vals.extend(pct)
            kde=gaussian_kde(pct,bw_method=0.4); x=np.linspace(0,100,400); d=kde(x)
            med=np.median(pct); med_d=kde(np.array([med]))[0]
            fig.add_trace(go.Scatter(x=x,y=d,mode="lines",
                name=f"{label} (n={len(pct)}, med={med:.0f}% through)",
                line=dict(color=color,width=2),fill="tozeroy",fillcolor=hex_to_rgba(color,0.08),
                hovertemplate=f"<b>{label}</b><br>% through: %{{x:.1f}}%<extra></extra>"))
            fig.add_trace(go.Scatter(x=[med,med],y=[0,med_d],mode="lines",
                line=dict(color=color,width=1.5,dash="dot"),showlegend=False,hoverinfo="skip"))
        if all_vals:
            fig.update_layout(title="When in the Moto Do Anomalies Occur?",
                xaxis=dict(title="% Through Moto",range=[0,100],tickvals=[0,25,50,75,100],
                           ticktext=["Start","25%","50%","75%","End"]),
                yaxis=dict(title="Density",showticklabels=False),
                height=500,margin=dict(l=60,r=60,t=60,b=60),
                legend=dict(title="Type"),hovermode="x unified")
            st.plotly_chart(fig,use_container_width=True)

st.divider()

# ═══════════════════════════════════════════════════════════════════════════════
# 16c. What-if: no DNFs standings simulation
# ═══════════════════════════════════════════════════════════════════════════════
st.subheader("What if DNFs didn't happen? — standings simulation")
st.caption("Restores DNF riders to their last recorded place, cascades downstream riders, recomputes season standings.")

c1,c2 = st.columns(2)
with c1: yr_a3  = st.selectbox("Year",  all_years,   index=len(all_years)-1, key="a3_yr")
with c2: cls_a3 = st.selectbox("Class", all_classes, key="a3_cls")

if st.button("Update — simulation", type="primary", key="a3_btn"):
    _, unrec3 = detect_anomalies(anom_base, 2.5)
    unrec3 = unrec3[(unrec3["year"]==yr_a3)&(unrec3["class_label"]==cls_a3)].copy()

    if unrec3.empty:
        st.info("No unrecoverable DNFs detected for selected filters.")
    else:
        prize_base = (df.drop_duplicates(subset=["race_id","name"]).dropna(subset=["finish_position"]).copy())
        prize_base["year"]            = prize_base["year"].astype(str)
        prize_base["round"]           = prize_base["round"].astype(int)
        prize_base["moto"]            = prize_base["moto"].astype(int)
        prize_base["finish_position"] = prize_base["finish_position"].astype(int)
        prize_base["moto_points"]     = prize_base["finish_position"].map(TCMX_POINTS).fillna(0)

        actual = prize_base[(prize_base["year"]==yr_a3)&(prize_base["class_label"]==cls_a3)][
            ["race_id","name","round","moto","finish_position","moto_points"]].copy()
        sim = actual.copy(); sim["sim_finish"]=sim["finish_position"]; sim["sim_pts"]=sim["moto_points"]

        for race_id, grp in unrec3.groupby("race_id",observed=True):
            rs = sim[sim["race_id"]==race_id].copy()
            for _, dnf in grp.iterrows():
                rider, restored = dnf["name"], int(dnf["last_lap_place"])
                cur = rs.loc[rs["name"]==rider,"sim_finish"].values
                if not len(cur): continue
                cur=int(cur[0])
                rs.loc[(rs["name"]!=rider)&(rs["sim_finish"]>=restored)&(rs["sim_finish"]<cur),"sim_finish"]+=1
                rs.loc[rs["name"]==rider,"sim_finish"]=restored
            rs["sim_pts"] = rs["sim_finish"].map(TCMX_POINTS).fillna(0)
            sim.loc[sim["race_id"]==race_id,"sim_finish"]=rs["sim_finish"].values
            sim.loc[sim["race_id"]==race_id,"sim_pts"]=rs["sim_pts"].values

        def season_rank(pts_col,label):
            return (actual[["name"]].drop_duplicates()
                    .merge(sim.groupby("name",observed=True)[pts_col].sum().reset_index(name=label),on="name",how="left")
                    .sort_values(label,ascending=False).reset_index(drop=True))

        act_s = (actual.groupby("name",observed=True)["moto_points"].sum()
                 .reset_index(name="actual_pts").sort_values("actual_pts",ascending=False).reset_index(drop=True))
        act_s.index+=1; act_s["actual_rank"]=act_s.index
        sim_s = (sim.groupby("name",observed=True)["sim_pts"].sum()
                 .reset_index(name="sim_pts").sort_values("sim_pts",ascending=False).reset_index(drop=True))
        sim_s.index+=1; sim_s["sim_rank"]=sim_s.index

        cmp = act_s.merge(sim_s,on="name",how="outer")
        cmp["pts_delta"]  = cmp["sim_pts"]-cmp["actual_pts"]
        cmp["rank_delta"] = cmp["actual_rank"]-cmp["sim_rank"]

        top5=cmp.head(5)["name"].tolist()
        affected=cmp[cmp["pts_delta"]!=0]["name"].tolist()
        show = cmp[cmp["name"].isin(set(top5+affected))].copy()

        # DNF summary
        dnf_disp = unrec3[["name","round","moto","track","last_lap_num","moto_max_lap","last_lap_place","finish_position"]].copy()
        for c in ["last_lap_num","moto_max_lap","last_lap_place","finish_position"]:
            dnf_disp[c]=dnf_disp[c].astype(int)
        dnf_disp=dnf_disp.rename(columns={"name":"Rider","round":"Rd","moto":"Moto","track":"Track",
            "last_lap_num":"Last Lap","moto_max_lap":"Moto Laps","last_lap_place":"Place at DNF","finish_position":"Actual Finish"})
        st.markdown("**DNFs being restored:**")
        st.dataframe(dnf_disp.reset_index(drop=True), hide_index=True, use_container_width=False)

        show["Pts Δ"]  = show["pts_delta"].apply(lambda x: f"+{int(x)}" if x>0 else (f"{int(x)}" if x<0 else "—"))
        show["Rank Δ"] = show["rank_delta"].apply(lambda x: f"+{int(x)}" if x>0 else (f"{int(x)}" if x<0 else "—"))
        show["actual_pts"] = show["actual_pts"].astype(int); show["sim_pts"] = show["sim_pts"].astype(int)
        show=show.rename(columns={"name":"Rider","actual_rank":"Actual Rank","actual_pts":"Actual Pts",
                                   "sim_rank":"Sim Rank","sim_pts":"Sim Pts"}).reset_index(drop=True)
        show.index+=1
        st.markdown("**Season standings: actual vs simulated**")
        st.dataframe(show[["Rider","Actual Rank","Actual Pts","Sim Rank","Sim Pts","Pts Δ","Rank Δ"]], use_container_width=False)
        st.caption("Cascade effect applied — riders behind the restored position shift down one place.")

st.divider()
st.markdown("""
**Anomaly definitions**
- **Recoverable**: lap time ≥ N standard deviations above the rider's own median in that moto, AND place drops ≥ 2 on the same lap, AND the rider finishes the moto cleanly (last lap = moto's final lap, finish position matches last-lap place).
- **Unrecoverable**: |finish position − place on last recorded lap| ≥ 2. Catches DNFs and riders who fell/crashed near the end and couldn't recover before the flag.
- The SD threshold slider adjusts sensitivity — lower values catch more marginal events; higher values only flag dramatic spikes.
""")

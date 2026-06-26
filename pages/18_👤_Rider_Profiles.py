import streamlit as st
import pandas as pd
import numpy as np
import plotly.graph_objects as go
import sys, os
sys.path.insert(0, os.path.dirname(os.path.dirname(__file__)))
from utils.data import load_data

st.set_page_config(page_title="Triple Crown MX · Rider Profiles", page_icon="🏍️", layout="wide")

df = load_data()

CLASS_COLORS = {"450": "#1A7FE8", "250": "#1AE87F", "WMX": "#E8641A"}
CLASS_BG     = {"450": "#E6F1FB", "250": "#EAF3DE", "WMX": "#FBEAF0"}
CLASS_TEXT   = {"450": "#0C447C", "250": "#27500A", "WMX": "#72243E"}
TRAFFIC_STATE_COLORS = {
    "Sandwiched": "#9B27AE", "Chasing": "#E8641A",
    "Being Chased": "#E8C11A", "Clear Air": "#1A7FE8",
}
BIN_LABELS = ["0–10%","10–20%","20–30%","30–40%","40–50%",
              "50–60%","60–70%","70–80%","80–90%","90–100%"]

def hex_to_rgba(h, op=0.12):
    h=h.lstrip("#"); r,g,b=int(h[0:2],16),int(h[2:4],16),int(h[4:6],16)
    return f"rgba({r},{g},{b},{op})"

st.title("18. Rider Profiles")
st.markdown("Per-rider season summary card with pace arc, results timeline, and championship standing trajectory")

# ── Cached data builds ────────────────────────────────────────────────────────
@st.cache_data
def build_profile_base(_df):
    base = _df.copy()
    base["frac_bin"] = ((1-base["frac_remaining"])*10).clip(0,9.9999).fillna(0).astype(int)
    return base

@st.cache_data
def build_moto_results(_df):
    mr = (
        _df.groupby(["name","class_label","year","round","moto"],observed=True)
        [["finish_position","points"]].first().reset_index()
    )
    mr = mr.sort_values(["round","moto"])
    mr["cum_points"] = (
        mr.groupby(["name","class_label","year"],observed=True)["points"].cumsum()
    )
    rows=[]
    for (cl,yr), grp in mr.groupby(["class_label","year"],observed=True):
        grp = grp.sort_values(["round","moto"]).copy()
        grp["round_int"] = grp["round"].astype(int)
        grp["moto_int"]  = grp["moto"].astype(int)
        for (rnd,mto),_ in grp.groupby(["round_int","moto_int"],observed=True):
            snap = grp[(grp["round_int"]<rnd)|((grp["round_int"]==rnd)&(grp["moto_int"]<=mto))]
            latest = snap.groupby("name",observed=True)["cum_points"].max().reset_index()
            latest["standing"] = latest["cum_points"].rank(ascending=False,method="min").astype(int)
            latest["class_label"]=cl; latest["year"]=yr
            latest["round"]=rnd; latest["moto"]=mto
            rows.append(latest[["name","class_label","year","round","moto","standing"]])
    standings = pd.concat(rows,ignore_index=True)
    mr = mr.merge(standings,on=["name","class_label","year","round","moto"],how="left")
    return mr

df_prof  = build_profile_base(df)
moto_res = build_moto_results(df)

eligible_riders = sorted(moto_res["name"].dropna().unique().tolist())

# ── Filter cascade ─────────────────────────────────────────────────────────────
c1,c2,c3 = st.columns(3)
with c1:
    rider_sel = st.selectbox("Rider", eligible_riders)

avail_years = sorted(df_prof[df_prof["name"]==rider_sel]["year"].unique().tolist())
with c2:
    year_sel = st.selectbox("Year", avail_years, index=len(avail_years)-1 if avail_years else 0)

avail_classes = sorted(df_prof[(df_prof["name"]==rider_sel)&(df_prof["year"]==year_sel)]["class_label"].unique().tolist())
with c3:
    class_sel = st.selectbox("Class", avail_classes if avail_classes else ["—"])

if st.button("Show Profile", type="primary") and avail_classes:
    col = CLASS_COLORS.get(class_sel, "#888")
    bg  = CLASS_BG.get(class_sel, "#eee")
    txt = CLASS_TEXT.get(class_sel, "#333")

    mask = (
        (df_prof["name"]==rider_sel) &
        (df_prof["class_label"]==class_sel) &
        (df_prof["year"]==year_sel)
    )
    sub = df_prof[mask]
    yr_moto = moto_res[
        (moto_res["name"]==rider_sel) &
        (moto_res["class_label"]==class_sel) &
        (moto_res["year"]==year_sel)
    ].sort_values(["round","moto"]).copy()

    if len(sub)==0 or len(yr_moto)==0:
        st.warning("No data for this selection.")
    else:
        # ── Summary metrics ────────────────────────────────────────────────────
        avg_finish  = sub.groupby(["round","moto"],observed=True)["finish_position"].first().mean()
        avg_z       = sub["z_score"].mean()
        avg_pct_off = sub["pct_off_best"].mean()
        podiums     = int((yr_moto["finish_position"]<=3).sum())
        n_motos     = len(yr_moto)
        mfr         = sub["manufacturer"].iloc[0]

        traffic_counts = sub["traffic_state"].astype(str).value_counts()
        traffic_pcts   = {s: traffic_counts.get(s,0)/len(sub)*100 for s in TRAFFIC_STATE_COLORS}

        # ── Header: name + class badge ─────────────────────────────────────────
        st.markdown(
            f"### {rider_sel} &nbsp;"
            f'<span style="background:{bg};color:{txt};border-radius:4px;padding:3px 10px;'
            f'font-size:14px;font-weight:600">{class_sel}</span>'
            f' &nbsp;<span style="background:#f0f0ee;color:#666;border-radius:4px;'
            f'padding:3px 10px;font-size:13px">{mfr}</span>',
            unsafe_allow_html=True,
        )

        # ── 4 metric bubbles — native st.metric ────────────────────────────────
        m1, m2, m3, m4 = st.columns(4)
        m1.metric("Avg Finish",   f"{avg_finish:.1f}")
        m2.metric("Avg Z-Score",  f"{avg_z:+.2f}")
        m3.metric("Avg % Off Best",   f"{avg_pct_off:.1f}%")
        m4.metric("Podiums", str(podiums), delta=f" of {n_motos} motos", delta_color="off")

        st.divider()

        # ── Traffic profile strip ──────────────────────────────────────────────
        traf_tot = sum(traffic_pcts.values()) or 1
        traf_w   = {k: v/traf_tot*100 for k,v in traffic_pcts.items()}
        strip_segs = "".join(
            f'<div style="flex:{traf_w[s]:.1f};background:{c};"></div>'
            for s,c in TRAFFIC_STATE_COLORS.items()
        )
        legend_items = " &nbsp;".join(
            f'<span style="font-size:14px;color:#333">'
            f'<span style="color:{c};font-size:16px">■</span> '
            f'<strong>{traffic_pcts[s]:.0f}%</strong> {s}</span>'
            for s,c in TRAFFIC_STATE_COLORS.items()
        )
        st.markdown(
            f'<div style="margin-bottom:6px;font-size:14px;font-weight:600;color:#444">Traffic profile</div>'
            f'<div style="display:flex;height:10px;border-radius:5px;overflow:hidden;background:#eee;margin-bottom:10px">'
            f'{strip_segs}</div>'
            f'<div style="display:flex;gap:24px;margin-bottom:8px">{legend_items}</div>',
            unsafe_allow_html=True,
        )

        st.divider()

        # ── Pace arc ────────────────────────────────────────────────────────────
        arc = (
            sub.groupby("frac_bin",observed=True)["z_score"]
            .mean().reindex(range(10))
        )
        AXIS_STYLE = dict(gridcolor="rgba(0,0,0,0.06)", linecolor="rgba(0,0,0,0.15)")
        fig_arc = go.Figure()
        fig_arc.add_trace(go.Scatter(
            x=BIN_LABELS, y=arc.values, mode="lines+markers",
            line=dict(color=col,width=2.5), marker=dict(size=7,color=col),
            fill="tozeroy", fillcolor=hex_to_rgba(col,0.12),
            showlegend=False,
            hovertemplate="%{x}<br>Z-score: %{y:.2f}<extra></extra>",
        ))
        fig_arc.update_layout(
            title=dict(text="Pace Arc — Avg z-score by race %",font=dict(size=16,color="#1a1a1a"),x=0,xanchor="left"),
            xaxis=dict(**AXIS_STYLE,title=dict(text="Fraction of race completed",font=dict(size=15,color="#1a1a1a")),
                       tickfont=dict(size=13,color="#1a1a1a"),tickangle=30,showgrid=True),
            yaxis=dict(**AXIS_STYLE,title=dict(text="Avg z-score",font=dict(size=15,color="#1a1a1a")),
                       tickfont=dict(size=13,color="#1a1a1a"),showgrid=True),
            height=420,margin=dict(t=50,b=60,l=80,r=30),
            plot_bgcolor="white",paper_bgcolor="white",
            font=dict(family="Arial, sans-serif",color="#1a1a1a"),
        )
        st.plotly_chart(fig_arc, use_container_width=True)

        # ── Results & standing chart ───────────────────────────────────────────
        all_motos = (moto_res[(moto_res["class_label"]==class_sel)&(moto_res["year"]==year_sel)]
                     [["round","moto"]].drop_duplicates().copy())
        all_motos["round_int"]=all_motos["round"].astype(int)
        all_motos["moto_int"]=all_motos["moto"].astype(int)
        all_motos=all_motos.sort_values(["round_int","moto_int"])
        all_motos["x_label"]=all_motos.apply(lambda r: f"R{int(r['round_int'])}M{int(r['moto_int'])}",axis=1)

        yr_moto["round_int"]=yr_moto["round"].astype(int)
        yr_moto["moto_int"]=yr_moto["moto"].astype(int)
        yr_moto["x_label"]=yr_moto.apply(lambda r: f"R{int(r['round_int'])}M{int(r['moto_int'])}",axis=1)

        full_idx=(all_motos[["x_label"]]
                  .merge(yr_moto[["x_label","finish_position","standing"]],on="x_label",how="left"))
        full_idx["standing_filled"]=full_idx["standing"].ffill()

        x_labels    = full_idx["x_label"].tolist()
        finish_vals = full_idx["finish_position"].tolist()
        stand_vals  = full_idx["standing_filled"].tolist()

        all_pos    = [v for v in finish_vals+stand_vals if pd.notna(v)]
        actual_max = max(all_pos) if all_pos else 10
        dt         = max(1, round(actual_max/5))

        fig_res=go.Figure()
        fig_res.add_trace(go.Scatter(x=x_labels,y=finish_vals,mode="lines+markers",
            line=dict(color=col,width=2.5),marker=dict(size=7,color=col),
            name="Finish Position",connectgaps=False,
            hovertemplate="%{x}<br>Finish: %{y}<extra></extra>"))
        fig_res.add_trace(go.Scatter(x=x_labels,y=stand_vals,mode="lines+markers",
            line=dict(color="#E8C01A",width=2.5,dash="dash"),marker=dict(size=5,color="#E8C01A"),
            name="Championship Standing",connectgaps=True,
            hovertemplate="%{x}<br>Standing: %{y}<extra></extra>"))
        fig_res.update_layout(
            title=dict(text="Results & Championship Standing",font=dict(size=16,color="#1a1a1a"),x=0,xanchor="left"),
            xaxis=dict(**AXIS_STYLE,tickangle=35,tickfont=dict(size=13,color="#1a1a1a"),showgrid=True),
            yaxis=dict(**AXIS_STYLE,title=dict(text="Position",font=dict(size=15,color="#1a1a1a")),
                       tickfont=dict(size=13,color="#1a1a1a"),
                       range=[actual_max+2,0.5],dtick=dt,tick0=1,showgrid=True),
            legend=dict(orientation="h",x=0.5,xanchor="center",y=-0.18,font=dict(size=13,color="#1a1a1a")),
            height=420,margin=dict(t=50,b=90,l=80,r=40),
            plot_bgcolor="white",paper_bgcolor="white",
            font=dict(family="Arial, sans-serif",color="#1a1a1a"),
        )
        st.plotly_chart(fig_res, use_container_width=True)

st.divider()
st.markdown("""
**Notes**
- Pace arc shows average z-score per 10% decile of the race. Negative = faster than field average for that lap; positive = slower.
- Results chart shows finish position per moto (solid) and championship standing after each moto (dashed gold), with standing forward-filled across motos where the rider didn't enter.
- Traffic profile strip shows the fraction of laps spent in each traffic state across all motos in the selected year/class.
- Manufacturer shown is the first value found in that year/class — if it changes mid-season the card won't reflect that.
""")

// Atlas referendum heatmaps (generated data: code/atlas/build_panels.R, make_heatmaps).
// Clicking a panel's small heatmap (.atlas-heat) opens, below the panel, the same heatmap grouped by voting bloc
// (.atlas-heatmap). The grouped heatmap is drawn here from <key>_heat.json so the cursor can be read exactly:
// hovering (or tapping) shows the question, the unit under the cursor, its bloc's average and the place's average;
// clicking pins the readout until the next click.
(() => {
  const LOW = [178, 24, 43], MID = [247, 247, 247], HIGH = [33, 102, 172], NA = [217, 217, 217];
  const GAP = 3, BAR = 30, BAR_GAP = 6;
  const ramp = v => {                                   // 0..100 -> rgb, as scale_fill_gradient2(midpoint = 0.5)
    if (v < 0) return NA;
    const t = v / 100, [a, b, u] = t < 0.5 ? [LOW, MID, t / 0.5] : [MID, HIGH, (t - 0.5) / 0.5];
    return a.map((x, i) => Math.round(x + (b[i] - x) * u));
  };
  const pct = x => x == null || x < 0 ? "no vote" : `${Math.round(x)}%`;
  const esc = s => String(s).replace(/[&<>"]/g, c => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" })[c]);
  const ink = hex => {                                  // dark or light text on a bloc colour
    const c = [1, 3, 5].map(i => parseInt(hex.slice(i, i + 2), 16) / 255);
    return c.reduce((s, x, i) => s + [0.2126, 0.7152, 0.0722][i] * Math.pow(x, 2.2), 0) > 0.32 ? "#111" : "#fff";
  };

  function build(host, d) {
    host.innerHTML = "";
    const wrap = document.createElement("div"); wrap.className = "atlas-heat-stage";
    const base = document.createElement("canvas"), over = document.createElement("canvas");
    over.className = "atlas-heat-over";
    const tip = document.createElement("div"); tip.className = "atlas-heat-tip"; tip.hidden = true;
    tip.setAttribute("role", "status"); tip.setAttribute("aria-live", "polite");
    wrap.append(base, over, tip); host.append(wrap);

    const nr = d.rn.length, nc = d.nc, blocIx = new Map(d.blocs.map((b, i) => [b.b, i]));
    // one pixel per cell, stretched into place
    const cells = document.createElement("canvas"); cells.width = nc; cells.height = nr;
    const cx = cells.getContext("2d"), im = cx.createImageData(nc, nr);
    for (let i = 0; i < nr * nc; i++) { const c = ramp(d.v[i]); im.data.set([c[0], c[1], c[2], 255], 4 * i); }
    cx.putImageData(im, 0, 0);
    // blocs in row order; tiny blocs get at least d.min of the height
    const groups = [];
    for (let r = 0; r < nr; r++) {
      const g = groups[groups.length - 1];
      if (g && g.b === d.rb[r]) g.n++; else groups.push({ b: d.rb[r], r0: r, n: 1 });
    }
    const weight = g => Math.max(g.n, d.min * nr), wsum = groups.reduce((s, g) => s + weight(g), 0);
    let L = null, pinned = null;

    function layout() {
      const W = host.clientWidth, H = Math.round(Math.max(340, Math.min(760, W * 0.58)));
      const dpr = window.devicePixelRatio || 1;
      for (const c of [base, over]) { c.width = Math.round(W * dpr); c.height = Math.round(H * dpr); c.style.width = W + "px"; c.style.height = H + "px"; }
      const hw = W - BAR - BAR_GAP, avail = H - GAP * (groups.length - 1);
      let y = 0;
      for (const g of groups) { g.y = y; g.h = avail * weight(g) / wsum; y += g.h + GAP; }
      L = { W, H, hw, dpr };
      const ctx = base.getContext("2d"); ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
      ctx.clearRect(0, 0, W, H); ctx.imageSmoothingEnabled = false;
      ctx.font = "600 12px system-ui, -apple-system, Segoe UI, sans-serif"; ctx.textAlign = "center"; ctx.textBaseline = "middle";
      for (const g of groups) {
        ctx.drawImage(cells, 0, g.r0, nc, g.n, 0, g.y, hw, g.h);
        const b = d.blocs[blocIx.get(g.b)];
        ctx.fillStyle = b.c; ctx.fillRect(hw + BAR_GAP, g.y, BAR, g.h);
        if (g.h >= 13) { ctx.fillStyle = ink(b.c); ctx.fillText(String(b.b), hw + BAR_GAP + BAR / 2, g.y + g.h / 2 + 0.5); }
      }
      draw(pinned);
    }

    function hit(x, y) {
      if (!L || x < 0 || x >= L.hw || y < 0 || y >= L.H) return null;
      const g = groups.find(g => y >= g.y && y < g.y + g.h);
      if (!g) return null;
      const r = g.r0 + Math.min(g.n - 1, Math.floor((y - g.y) / g.h * g.n)), c = Math.min(nc - 1, Math.floor(x / L.hw * nc));
      return { g, r, c };
    }

    function draw(h) {
      const ctx = over.getContext("2d"); ctx.setTransform(L.dpr, 0, 0, L.dpr, 0, 0); ctx.clearRect(0, 0, L.W, L.H);
      if (!h) return;
      const cw = L.hw / nc, rh = h.g.h / h.g.n, x0 = h.c * cw, y0 = h.g.y + (h.r - h.g.r0) * rh;
      ctx.fillStyle = "rgba(255,255,255,.28)";              // soften everything but the question's column
      ctx.fillRect(0, 0, x0, L.H); ctx.fillRect(x0 + cw, 0, L.hw - x0 - cw, L.H);
      ctx.strokeStyle = "#111"; ctx.lineWidth = 1;
      ctx.strokeRect(Math.round(x0) - 0.5, 0.5, Math.max(2, Math.round(cw)) + 1, L.H - 1);
      ctx.lineWidth = 1.5; ctx.strokeRect(Math.round(x0) - 1.5, y0 - 1.5, Math.max(2, Math.round(cw)) + 3, Math.max(2, rh) + 3);
      ctx.strokeStyle = "rgba(17,17,17,.55)"; ctx.lineWidth = 1;   // the row, faintly
      ctx.beginPath(); ctx.moveTo(0, y0 + rh / 2); ctx.lineTo(L.hw, y0 + rh / 2); ctx.stroke();
    }

    function show(h, x, y) {
      if (!h) { tip.hidden = true; return; }
      const q = d.q[h.c], b = d.blocs[blocIx.get(h.g.b)], word = q.k === "dem" ? "Democratic" : "yes";
      const unitV = d.v[h.r * nc + h.c];
      tip.innerHTML =
        `<p class="q">${esc(q.l)}</p>` + (q.d ? `<p class="qd">${esc(q.d)}</p>` : "") +
        `<table><tr><th>${esc(d.rn[h.r])}</th><td>${pct(unitV)}${unitV >= 0 ? " " + word : ""}</td></tr>` +
        `<tr><th><span class="sw" style="background:${b.c};color:${ink(b.c)}">${b.b}</span>${esc(b.nm)} average</th><td>${pct(b.a[h.c])}</td></tr>` +
        `<tr><th>${esc(d.place)} average</th><td>${pct(d.all[h.c])}</td></tr></table>` +
        (pinned ? `<p class="pin">Pinned. Click again to move or release it.</p>` : "");
      tip.hidden = false;
      // beside the cursor, kept inside the plot
      const tw = tip.offsetWidth, th = tip.offsetHeight, pad = 14;
      let tx = x + pad, ty = y + pad;
      if (tx + tw > L.W) tx = Math.max(0, x - tw - pad);
      if (ty + th > L.H) ty = Math.max(0, L.H - th);
      tip.style.left = tx + "px"; tip.style.top = ty + "px";
    }

    const pos = e => { const r = over.getBoundingClientRect(); return [e.clientX - r.left, e.clientY - r.top]; };
    over.addEventListener("pointermove", e => {
      if (pinned || e.pointerType === "touch") return;
      const [x, y] = pos(e), h = hit(x, y); draw(h); show(h, x, y);
    });
    over.addEventListener("pointerleave", () => { if (!pinned) { draw(null); show(null); } });
    over.addEventListener("click", e => {
      const [x, y] = pos(e), h = hit(x, y);
      const same = pinned && h && pinned.r === h.r && pinned.c === h.c;
      pinned = h && !same ? Object.assign(h, { x, y }) : null;
      draw(h); show(h, x, y);
      if (!pinned && e.pointerType === "touch") { draw(null); show(null); }
    });
    let t; new ResizeObserver(() => { clearTimeout(t); t = setTimeout(() => { layout(); if (pinned) show(pinned, pinned.x, pinned.y); }, 60); }).observe(host);
    layout();
  }

  function load(host) {
    if (host.dataset.loaded) return;
    host.dataset.loaded = "1";
    fetch(host.dataset.json).then(r => { if (!r.ok) throw r.status; return r.json(); }).then(d => build(host, d))
      .catch(() => {                                      // no data: fall back to the static picture
        host.innerHTML = `<img src="${host.dataset.img}" alt="${host.getAttribute("aria-label") || ""}" style="width:100%;height:auto">`;
      });
  }

  document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll(".atlas-panel").forEach(panel => {
      const fig = panel.querySelector(".atlas-heat"), heat = panel.querySelector(".atlas-heatmap");
      if (!fig || !heat) return;
      fig.tabIndex = 0;
      fig.setAttribute("role", "button");
      fig.setAttribute("aria-controls", heat.id);
      fig.setAttribute("aria-expanded", "false");
      const toggle = () => {
        const open = heat.hidden;
        heat.hidden = !open;
        fig.setAttribute("aria-expanded", String(open));
        if (open) {
          const img = heat.querySelector("img[data-src]"); if (img && !img.src) img.src = img.dataset.src;   // older panels
          const plot = heat.querySelector(".atlas-heat-plot"); if (plot) load(plot);
          heat.scrollIntoView({ behavior: "smooth", block: "start" });
        }
      };
      fig.addEventListener("click", toggle);
      fig.addEventListener("keydown", e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); toggle(); } });
    });
    // size the map's column so the map is as tall as the square PCA and heatmap beside it
    document.querySelectorAll(".atlas-panel-grid").forEach(grid => {
      const img = grid.querySelector(".atlas-heat") && grid.querySelector(".atlas-state-map .atlas-zoom img");
      if (!img) return;
      const fit = () => { if (img.naturalWidth && img.naturalHeight)
        grid.style.setProperty("--heat-cols", `1fr minmax(12rem, 1.5fr) ${(img.naturalWidth / img.naturalHeight).toFixed(3)}fr 1fr`); };
      if (img.complete) fit(); else img.addEventListener("load", fit, { once: true });
    });
    // one subpanel open at a time, on the whole page: opening the PCA's demographics, a bloc's profile
    // (bloc_profiles.js, which adds .atlas-bloc-profile) or the heatmap closes every other one
    const SUB = ".atlas-demog, .atlas-heatmap, .atlas-bloc-profile";
    const close = el => {
      if (el.hidden) return;
      const panel = el.closest(".atlas-panel");
      if (el.classList.contains("atlas-bloc-profile")) { panel.dispatchEvent(new CustomEvent("bloc-select", { detail: 0 })); return; }
      el.hidden = true;
      const fig = panel.querySelector(el.classList.contains("atlas-demog") ? ".atlas-pca" : ".atlas-heat");
      if (fig) fig.setAttribute("aria-expanded", "false");
    };
    const watch = new MutationObserver(recs => recs.forEach(r => {
      if (r.target.hidden) return;
      document.querySelectorAll(SUB).forEach(el => { if (el !== r.target) close(el); });
    }));
    document.querySelectorAll(SUB).forEach(el => watch.observe(el, { attributes: true, attributeFilter: ["hidden"] }));
  });
})();

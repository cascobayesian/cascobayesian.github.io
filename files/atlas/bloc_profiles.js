// Atlas bloc profiles. Click a bloc in a panel's list, or on its map, and the bloc stays bright on the map while the
// rest fades, and its demographic profile opens under the panel, beside the place's own. Hovering the map names the
// bloc under the pointer. Needs, on the panel's map <img>: data-index (bloc-index PNG laid out as the map: red
// channel = bloc number, transparent elsewhere) and data-profile (JSON), from code/atlas/bloc_profiles/01_profiles.R.
// Runs after the atlas's zoom script, which wraps the map in .atlas-zoom-stage.
// Clicking a town / precinct on the map also highlights its bloc, but the card under the panel is about that unit:
// its name, bloc, how certain the assignment is, and its demographics beside its bloc's and the place's. Read from
// <key>_units.png (unit-index image laid out as the bloc index; RGB = unit number) and <key>_units.json, beside the
// bloc index (code/atlas/unit_cards/01_units.R).
(() => {
  const RAMP = (n, lo = [227, 236, 245], hi = [29, 69, 128]) => Array.from({ length: n }, (_, i) => {
    const t = n === 1 ? 1 : i / (n - 1);
    return "#" + lo.map((a, j) => Math.round(a + (hi[j] - a) * t).toString(16).padStart(2, "0")).join("");
  });
  const withCols = (cats, cols) => cats.map((c, i) => [c[0], c[1], cols[i]]);
  const TOPICS = [
    { k: "race", t: "Race and ethnicity", cats: [["white", "White", "#E31A1C"], ["hispanic", "Hispanic", "#F2C200"],
      ["aian", "American Indian", "#00A9A5"], ["black", "Black", "#1F78B4"], ["asian", "Asian", "#33A02C"],
      ["other", "Other or multiracial", "#B5B1A7"]] },
    { k: "language", t: "Language spoken at home", cats: prof => [["english", "English only", "#CFCAC0"], ["spanish", "Spanish", "#F2C200"],
      ["indo_euro", "Other European", "#C86B98"], ["asian", "Asian &amp; Pacific Island", "#33A02C"],
      prof.native_share >= 0.5 ? ["other", "Native American &amp; other", "#00A9A5"] : ["other", "Arabic &amp; other", "#8E6BB8"]] },
    { k: "nativity", t: "Nativity and citizenship", cats: [["native", "Native-born", "#CFCAC0"], ["naturalized", "Naturalized citizen", "#6FA0CC"],
      ["noncitizen", "Not a citizen", "#1D4580"]] },
    { k: "education", t: "Education, adults 25+", cats: withCols([["lt_hs", "No diploma"], ["hs", "High school"],
      ["some_college", "Some college"], ["ba", "Bachelor&rsquo;s"], ["grad", "Graduate"]], RAMP(5)) },
    { k: "income", t: "Household income", cats: withCols([["lt25", "Under $25k"], ["i25_50", "$25&ndash;50k"], ["i50_75", "$50&ndash;75k"],
      ["i75_100", "$75&ndash;100k"], ["i100_150", "$100&ndash;150k"], ["i150", "$150k+"]], RAMP(6)) },
    { k: "poverty", t: "Income against the poverty line", cats: withCols([["below", "Below poverty"], ["near", "1&ndash;2&times; poverty"],
      ["above", "2&times; or more"]], RAMP(3).reverse()) },
  ];
  const pct = x => Math.round(100 * x) + "%";
  const money = x => "$" + (Math.round(x / 100) * 100).toLocaleString("en-US");
  const light = hex => { const c = [1, 3, 5].map(i => parseInt(hex.slice(i, i + 2), 16) / 255);
    return 0.2126 * c[0] ** 2.2 + 0.7152 * c[1] ** 2.2 + 0.0722 * c[2] ** 2.2 > 0.32; };

  const tip = document.createElement("div"); tip.className = "atlas-tip"; tip.hidden = true;
  document.addEventListener("DOMContentLoaded", () => document.body.appendChild(tip));
  const showTip = (html, x, y) => { tip.innerHTML = html; tip.hidden = false;
    const r = tip.getBoundingClientRect();
    tip.style.left = Math.min(x + 14, innerWidth - r.width - 8) + "px"; tip.style.top = Math.max(8, y - r.height - 10) + "px"; };
  const hideTip = () => { tip.hidden = true; };

  const bar = (vals, cats, label, who) => '<div class="bp-row"><span class="bp-who">' + who + '</span><div class="bp-bar">' +
    cats.filter(c => vals[c[0]] > 0.0005).map(c => {
      const v = vals[c[0]];
      return `<span class="bp-seg" style="flex:${v} 1 0;background:${c[2]};color:${light(c[2]) ? "#111" : "#fff"}" data-tip="${c[1]}: <b>${pct(v)}</b> ${label}">${v >= 0.09 ? pct(v) : ""}</span>`;
    }).join("") + "</div></div>";

  const render = (panel, prof, b, li) => {
    const box = panel.querySelector(".atlas-bloc-profile");
    box.classList.remove("uc");
    if (!b) { box.hidden = true; return; }
    const p = prof.blocs[b], all = prof.place, sw = li.querySelector(".bloc-swatch");
    const blocLab = "Bloc " + b, placeLab = prof.title;
    box.innerHTML =
      '<header class="bp-head">' + sw.outerHTML +
      `<div><h3>${li.querySelector(".bloc-name").innerHTML}</h3><p>${li.querySelector(".bloc-meta").innerHTML} &middot; ${p.pop.toLocaleString("en-US")} residents (${pct(p.pop / all.pop)} of ${placeLab})</p></div>` +
      `<button type="button" class="bp-close" aria-label="Close bloc profile">&times;</button></header>` +
      '<div class="bp-grid">' + TOPICS.map(T0 => {
        const T = { ...T0, cats: typeof T0.cats === "function" ? T0.cats(prof) : T0.cats };
        const extra = T.k === "income" ? `<p class="bp-stat">Median <b>${money(p.median_income)}</b> <span>(${placeLab} ${money(all.median_income)})</span></p>` : "";
        return `<section><h4>${T.t}</h4>${extra}` +
          bar(p[T.k], T.cats, "in bloc " + b, blocLab) + bar(all[T.k], T.cats, "in " + placeLab, placeLab) +
          '<ul class="bp-key">' + T.cats.map(c => `<li><span style="background:${c[2]}"></span>${c[1]} <b>${pct(p[T.k][c[0]])}</b> <i>${pct(all[T.k][c[0]])}</i></li>`).join("") +
          "</ul></section>";
      }).join("") + "</div>" +
      `<p class="bp-note">Bold: the bloc; gray: ${placeLab} overall. Race from 2020 Census blocks; everything else from the ACS 2020&ndash;24 ` +
      `(block groups; language and nativity by tract), shared out to blocks by population and summed over the bloc&rsquo;s ${prof.unit}.` +
      (prof.native_share >= 0.5 ? ` &ldquo;Native American &amp; other&rdquo; is mostly the ACS&rsquo;s &ldquo;other languages&rdquo; group, ${pct(prof.native_share)} of whose speakers statewide speak Navajo or another Native language.` : "") + "</p>";
    box.hidden = false;
    box.querySelector(".bp-close").addEventListener("click", () => panel.dispatchEvent(new CustomEvent("bloc-select", { detail: 0 })));
  };

  const certainty = (U, i) => {
    const p = U.p[i];
    if (p == null) return "Assignment certainty not recorded for this fit";
    if (U.measure === "stability") return `In this bloc in <b>${pct(p)}</b> of the model&rsquo;s posterior samples`;
    return `<b>${pct(p)}</b> probability of belonging to this bloc` +
      (U.alt[i] != null && U.alt_p[i] >= 0.01 ? `; next most likely bloc ${U.alt[i]} (${pct(U.alt_p[i])})` : "");
  };
  const renderUnit = (panel, prof, U, i, li) => {
    const box = panel.querySelector(".atlas-bloc-profile"), b = U.bloc[i];
    box.classList.add("uc");
    const p = prof.blocs[b], all = prof.place, sw = li.querySelector(".bloc-swatch"), placeLab = prof.title;
    const who = U.name[i], unitWord = prof.unit.replace(/s$/, ""), pop = U.pop[i];
    const val = (T, c) => { const v = U.shares[T.k][c[0]][i]; return v == null ? null : v / 100; };
    const p1 = x => x == null ? "&ndash;" : pct(x);
    const hasPop = pop > 0 && U.shares.race.white[i] != null;
    const meter = U.p[i] == null ? "" : `<span class="uc-meter" title="Assignment certainty"><span style="width:${Math.round(100 * U.p[i])}%;background:${sw.style.background}"></span></span>`;
    box.innerHTML =
      '<header class="bp-head">' + sw.outerHTML +
      `<div><h3>${who}</h3><p>${li.querySelector(".bloc-name").innerHTML} (bloc ${b}) &middot; ${pop.toLocaleString("en-US")} residents</p>` +
      `<p class="uc-cert">${meter}${certainty(U, i)}</p></div>` +
      `<button type="button" class="bp-close" aria-label="Close ${unitWord} summary">&times;</button></header>` +
      (!hasPop ? `<p class="bp-note">No residents are recorded in this ${unitWord} (2020 Census), so it has no demographic summary.</p>` :
      '<div class="bp-grid">' + TOPICS.map(T0 => {
        const T = { ...T0, cats: typeof T0.cats === "function" ? T0.cats(prof) : T0.cats };
        const uv = Object.fromEntries(T.cats.map(c => [c[0], val(T, c) || 0]));
        const mi = U.median_income[i];
        const extra = T.k === "income" ? `<p class="bp-stat">Median <b>${mi == null ? "&ndash;" : money(mi)}</b> <span>(bloc ${money(p.median_income)}, ${placeLab} ${money(all.median_income)})</span></p>` : "";
        return `<section><h4>${T.t}</h4>${extra}` +
          bar(uv, T.cats, "in " + who, who) + bar(p[T.k], T.cats, "in bloc " + b, "Bloc " + b) + bar(all[T.k], T.cats, "in " + placeLab, placeLab) +
          '<ul class="bp-key">' + T.cats.map(c => `<li><span style="background:${c[2]}"></span>${c[1]} <b>${p1(val(T, c))}</b> <i>${pct(p[T.k][c[0]])} &middot; ${pct(all[T.k][c[0]])}</i></li>`).join("") +
          "</ul></section>";
      }).join("") + "</div>" +
      `<p class="bp-note">Bold: this ${unitWord}; gray: bloc ${b} &middot; ${placeLab} overall. Race from 2020 Census blocks; everything else from the ACS 2020&ndash;24 ` +
      `(block groups; language and nativity by tract), shared out to blocks by population and summed over the ${unitWord}. ` +
      `For a single ${unitWord} these survey estimates carry wide margins of error.</p>`);
    box.hidden = false;
    box.querySelector(".bp-close").addEventListener("click", () => panel.dispatchEvent(new CustomEvent("bloc-select", { detail: 0 })));
  };

  document.addEventListener("DOMContentLoaded", () => document.querySelectorAll(".atlas-panel").forEach(panel => {
    const img = panel.querySelector(".atlas-state-map img[data-index]");
    if (!img) return;
    const zoom = img.closest(".atlas-zoom"), lis = [...panel.querySelectorAll(".atlas-blocs li")];
    const liOf = new Map(lis.map(li => [+li.querySelector(".bloc-swatch").textContent, li]));   // cut-outs keep the fit's numbers
    const card = document.createElement("div"); card.className = "atlas-bloc-profile"; card.hidden = true;
    panel.querySelector(".atlas-panel-grid").after(card);
    card.addEventListener("pointerover", e => { const s = e.target.closest(".bp-seg"); if (s) showTip(s.dataset.tip, e.clientX, e.clientY); });
    card.addEventListener("pointermove", e => { const s = e.target.closest(".bp-seg"); s ? showTip(s.dataset.tip, e.clientX, e.clientY) : hideTip(); });
    card.addEventListener("pointerleave", hideTip);

    let prof = null, idx = null, W = 0, H = 0, mask = null, mw = 0, mh = 0, cur = 0, curUnit = -1;
    let readyP = null, unitsP = null, U = null, uctx = null;
    const loadUnits = () => unitsP || (unitsP = (async () => {   // unit-index image + per-unit data, on first use
      const base = img.dataset.index.replace(/_index\.png$/, "_units");
      const [u, im] = await Promise.all([fetch(base + ".json").then(r => { if (!r.ok) throw r.status; return r.json(); }),
        new Promise((ok, no) => { const i = new Image(); i.onload = () => ok(i); i.onerror = no; i.src = base + ".png"; })]);
      const c = document.createElement("canvas"); c.width = im.naturalWidth; c.height = im.naturalHeight;
      uctx = c.getContext("2d", { willReadFrequently: true }); uctx.drawImage(im, 0, 0); U = u;
    })().catch(() => { U = null; }));
    const unitAt = e => {                            // 0-based unit index under the pointer, or -1
      if (!U || !uctx) return -1;
      const r = img.getBoundingClientRect(), cw = uctx.canvas.width, ch = uctx.canvas.height;
      const x = Math.floor((e.clientX - r.left) / r.width * cw), y = Math.floor((e.clientY - r.top) / r.height * ch);
      if (x < 0 || y < 0 || x >= cw || y >= ch) return -1;
      const d = uctx.getImageData(x, y, 1, 1).data;
      return d[3] > 127 ? d[0] * 65536 + d[1] * 256 + d[2] - 1 : -1;
    };
    const load = () => readyP || (readyP = (async () => {   // fetched on first use: 38 panels share one page
      const [p, im] = await Promise.all([fetch(img.dataset.profile).then(r => r.json()),
        new Promise((ok, no) => { const i = new Image(); i.onload = () => ok(i); i.onerror = no; i.src = img.dataset.index; })]);
      prof = p; W = im.naturalWidth; H = im.naturalHeight;
      const c = document.createElement("canvas"); c.width = W; c.height = H;
      const ctx = c.getContext("2d", { willReadFrequently: true }); ctx.drawImage(im, 0, 0);
      const d = ctx.getImageData(0, 0, W, H).data; idx = new Uint8Array(W * H);
      for (let i = 0; i < idx.length; i++) idx[i] = d[4 * i + 3] > 127 ? d[4 * i] : 0;
      mw = W >> 1; mh = H >> 1;                       // the mask at half the hi-res map's resolution
      mask = document.createElement("canvas"); mask.className = "atlas-bloc-mask"; mask.width = mw; mask.height = mh;
      zoom.querySelector(".atlas-zoom-stage").appendChild(mask);
    })());
    const blocAt = e => {
      if (!idx) return 0;
      const r = img.getBoundingClientRect();
      const x = Math.floor((e.clientX - r.left) / r.width * W), y = Math.floor((e.clientY - r.top) / r.height * H);
      return x >= 0 && y >= 0 && x < W && y < H ? idx[y * W + x] : 0;
    };
    const paint = b => {
      const ctx = mask.getContext("2d");
      if (!b) { ctx.clearRect(0, 0, mw, mh); return; }
      const out = ctx.createImageData(mw, mh), o = out.data, R = 2;
      const at = (x, y) => (x < 0 || y < 0 || x >= mw || y >= mh) ? 0 : idx[(2 * y) * W + 2 * x];
      for (let y = 0; y < mh; y++) for (let x = 0; x < mw; x++) {
        const v = at(x, y), i = 4 * (y * mw + x);
        if (!v) continue;
        if (v !== b) { o[i] = o[i + 1] = o[i + 2] = 255; o[i + 3] = 205; continue; }
        if (at(x - R, y) !== b || at(x + R, y) !== b || at(x, y - R) !== b || at(x, y + R) !== b) { o[i] = o[i + 1] = o[i + 2] = 17; o[i + 3] = 235; }
      }
      ctx.putImageData(out, 0, 0);
    };
    // b: bloc to highlight; unit: a unit index for the unit card instead of the bloc card
    const select = async (b, unit = -1) => {
      if (!b && !cur) return;                    // nothing open: closing needs no data
      if (b) closeOthers(panel, "bloc");
      await load();
      if (b && !liOf.has(b)) b = 0;
      if (b !== cur || !b) paint(b);
      cur = b; curUnit = b ? unit : -1;
      panel.classList.toggle("bloc-selected", !!b);
      liOf.forEach((li, n) => { li.classList.toggle("active", n === b); li.setAttribute("aria-pressed", String(n === b)); });
      if (b && unit >= 0 && U) renderUnit(panel, prof, U, unit, liOf.get(b)); else render(panel, prof, b, liOf.get(b));
    };
    panel.addEventListener("bloc-select", e => select(e.detail));
    liOf.forEach((li, n) => {
      li.tabIndex = 0; li.setAttribute("role", "button"); li.setAttribute("aria-pressed", "false");
      const go = () => select(cur === n && curUnit < 0 ? 0 : n);
      li.addEventListener("click", go);
      li.addEventListener("keydown", e => { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); go(); } });
    });
    // the map: a click that did not drag picks the bloc under it (off the blocs clears); hover names it
    let down = null;
    zoom.addEventListener("pointerdown", e => { down = [e.clientX, e.clientY]; hideTip(); });
    zoom.addEventListener("click", e => {
      if (e.target.closest(".atlas-zoom-ctl") || !down || Math.hypot(e.clientX - down[0], e.clientY - down[1]) > 5) return;
      (async () => {
        await Promise.all([load(), loadUnits()]);
        const u = unitAt(e), b = u >= 0 && U ? U.bloc[u] : blocAt(e);
        if (u >= 0 && u === curUnit) { select(0); return; }   // the same unit again closes its card
        select(b, u);
      })();
    });
    zoom.addEventListener("pointermove", e => {
      if (e.buttons || e.target.closest(".atlas-zoom-ctl")) { hideTip(); return; }
      if (!idx) { load(); loadUnits(); return; }
      const b = blocAt(e);
      if (!b || !liOf.has(b)) { hideTip(); zoom.style.cursor = ""; return; }
      zoom.style.cursor = "pointer";
      const u = unitAt(e);
      showTip((u >= 0 ? `<b>${U.name[u]}</b><br>` : "") + `<b>${b}</b> ${liOf.get(b).querySelector(".bloc-name").innerHTML}`, e.clientX, e.clientY);
    });
    zoom.addEventListener("pointerleave", () => { hideTip(); zoom.style.cursor = ""; });
  }));

  // one demographic panel open at a time: opening a bloc's profile closes the PCA demographics (this place's and any
  // other place's) and other places' bloc profiles; opening the PCA demographics closes every bloc profile
  const closeDemog = p => {
    const d = p.querySelector(".atlas-demog"), fig = p.querySelector(".atlas-pca");
    if (d && !d.hidden) { d.hidden = true; if (fig) fig.setAttribute("aria-expanded", "false"); }
  };
  function closeOthers(panel, opened) {
    document.querySelectorAll(".atlas-panel").forEach(p => {
      if (p !== panel || opened === "bloc") closeDemog(p);
      if (p !== panel || opened === "pca") p.dispatchEvent(new CustomEvent("bloc-select", { detail: 0 }));
    });
  }
  document.addEventListener("DOMContentLoaded", () => document.querySelectorAll(".atlas-panel").forEach(panel => {
    const d = panel.querySelector(".atlas-demog");
    if (d) new MutationObserver(() => { if (!d.hidden) closeOthers(panel, "pca"); }).observe(d, { attributes: true, attributeFilter: ["hidden"] });
  }));
})();

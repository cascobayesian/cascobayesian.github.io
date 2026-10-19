# Post-render step: drop pages that are online but deliberately unlinked from docs/sitemap.xml,
# so search engines aren't pointed at them. Add a page's path to HIDDEN to hide it the same way.
import os, re

HIDDEN = ["todo.html", "research/atlas.html"]

path = os.path.join(os.environ.get("QUARTO_PROJECT_OUTPUT_DIR", "docs"), "sitemap.xml")
if os.path.exists(path):
    xml = open(path, encoding="utf-8").read()
    for page in HIDDEN:
        xml = re.sub(r"\s*<url>\s*<loc>[^<]*/" + re.escape(page) + r"</loc>.*?</url>", "", xml, flags=re.S)
    open(path, "w", encoding="utf-8").write(xml)

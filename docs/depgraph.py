#!/usr/bin/env python3
import os, re
from collections import defaultdict, deque

ROOT = "/Users/nicolas.grislain/Typednotes/hale"
SRC = os.path.join(ROOT, "Hale")
OUT_DIR = "/Users/nicolas.grislain/Typednotes/linen/docs"


def path_to_module(path):
    rel = os.path.relpath(path, ROOT)
    return rel[:-len(".lean")].replace(os.sep, ".")


# All .lean modules under Hale/, plus the Hale.lean root.
modules = {}
for dirpath, _, files in os.walk(SRC):
    for f in files:
        if f.endswith(".lean"):
            p = os.path.join(dirpath, f)
            modules[path_to_module(p)] = p
root_file = os.path.join(ROOT, "Hale.lean")
if os.path.exists(root_file):
    modules["Hale"] = root_file

def strip_block_comments(src):
    """Remove Lean /- ... -/ block comments (which nest), leaving line structure."""
    out = []
    depth = 0
    i = 0
    n = len(src)
    while i < n:
        two = src[i:i + 2]
        if two == "/-":
            depth += 1
            i += 2
        elif two == "-/" and depth > 0:
            depth -= 1
            i += 2
        else:
            if depth == 0:
                out.append(src[i])
            elif src[i] == "\n":
                out.append("\n")  # preserve line breaks inside comments
            i += 1
    return "".join(out)


# Real Lean imports sit at column 0 and precede any declaration.
import_re = re.compile(r'^import\s+(Hale(?:\.[A-Za-z0-9_]+)*)\s*$')

edges = defaultdict(set)
nodes = set(modules.keys())
for mod, path in modules.items():
    with open(path, encoding="utf-8") as fh:
        src = strip_block_comments(fh.read())
    for line in src.splitlines():
        # drop any trailing line comment
        line = line.split("--", 1)[0].rstrip()
        m = import_re.match(line)
        if m:
            dep = m.group(1)
            edges[mod].add(dep)
            nodes.add(dep)

# Kahn topo sort: a module appears AFTER everything it imports.
deps_of = {n: set(edges.get(n, set())) for n in nodes}
rdeps = defaultdict(set)
for n, ds in deps_of.items():
    for d in ds:
        rdeps[d].add(n)

remaining = {n: set(deps_of[n]) for n in nodes}
ready = deque(sorted(n for n in nodes if not remaining[n]))
order = []
while ready:
    n = ready.popleft()
    order.append(n)
    newly = []
    for dependent in sorted(rdeps[n]):
        remaining[dependent].discard(n)
        if not remaining[dependent]:
            newly.append(dependent)
    if newly:
        ready = deque(sorted(set(ready) | set(newly)))

cycle_nodes = sorted(n for n in nodes if remaining[n])

os.makedirs(OUT_DIR, exist_ok=True)
dot_path = os.path.join(OUT_DIR, "module-dependencies.dot")
with open(dot_path, "w", encoding="utf-8") as fh:
    fh.write("// Hale module dependency graph (intra-Hale imports only).\n")
    fh.write("// Generated from `import Hale.*` statements under Hale/.\n")
    fh.write("// A -> B means module A imports module B.\n")
    fh.write("digraph HaleModules {\n")
    fh.write("  rankdir=LR;\n")
    fh.write('  node [shape=box, style=rounded, fontsize=9, fontname="Helvetica"];\n')
    fh.write('  edge [color="#888888", arrowsize=0.6];\n')
    for n in sorted(nodes):
        if n in modules:
            fh.write(f'  "{n}";\n')
        else:
            fh.write(f'  "{n}" [color="#cc0000", style="rounded,dashed"];\n')
    for mod in sorted(edges):
        for dep in sorted(edges[mod]):
            fh.write(f'  "{mod}" -> "{dep}";\n')
    fh.write("}\n")

with open("/tmp/topo_order.txt", "w") as fh:
    fh.write("\n".join(order))
with open("/tmp/cycle.txt", "w") as fh:
    fh.write("\n".join(cycle_nodes))

# ---- Write Markdown ----
n_edges = sum(len(v) for v in edges.values())
missing = sorted(n for n in nodes if n not in modules)
md_path = os.path.join(OUT_DIR, "module-dependencies.md")
with open(md_path, "w", encoding="utf-8") as fh:
    w = fh.write
    w("# Hale module dependencies\n\n")
    w("Dependency graph and topological order of every module under "
      "[`Hale/`](../../hale/Hale), derived from the `import Hale.*` "
      "statements in each source file (imports inside comments/docstrings "
      "are ignored).\n\n")
    w("An edge **A → B** means *module A imports module B*, so **B must be "
      "built before A**.\n\n")
    w("## Summary\n\n")
    w(f"- **Modules (nodes):** {len(nodes)}\n")
    w(f"- **Source files scanned:** {len(modules)}\n")
    w(f"- **Dependency edges:** {n_edges}\n")
    w(f"- **Cycles (strongly-connected components > 1):** "
      f"{len(cycle_nodes)} → the graph is a DAG.\n")
    if missing:
        w(f"- **Imported but no source file found:** {len(missing)} "
          f"({', '.join('`'+m+'`' for m in missing)})\n")
    w("\n## Graph\n\n")
    w("The full Graphviz source is in "
      "[`module-dependencies.dot`](module-dependencies.dot); a rendered "
      "version is in [`module-dependencies.svg`](module-dependencies.svg). "
      "Regenerate either with:\n\n")
    w("```sh\n")
    w("python3 docs/depgraph.py            # rebuild .dot + .md\n")
    w("dot -Tsvg docs/module-dependencies.dot -o docs/module-dependencies.svg\n")
    w("```\n\n")
    w("## Topologically sorted modules\n\n")
    w("Each module is listed after all modules it imports. Within a "
      "dependency level, ordering is alphabetical.\n\n")
    for i, m in enumerate(order, 1):
        w(f"{i}. `{m}`\n")
    w("\n")
    if cycle_nodes:
        w("## Modules in cycles (not sortable)\n\n")
        for m in cycle_nodes:
            w(f"- `{m}`\n")
print("md:", md_path)

print({
    "n_files": len(modules),
    "n_nodes": len(nodes),
    "n_edges": sum(len(v) for v in edges.values()),
    "n_sorted": len(order),
    "n_cycle": len(cycle_nodes),
})
print("dot:", dot_path)

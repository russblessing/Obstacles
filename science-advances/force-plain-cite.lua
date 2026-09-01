-- force-plain-cite.lua
--
-- scicite.sty (Science/AAAS's citation package) only supports plain
-- \cite{key1,key2} — it redefines \cite itself and provides nothing
-- else. AAAS's own author instructions are explicit about this:
-- "cite your references... using the standard LATEX \cite command,
-- not another command driven by outside macros."
--
-- Pandoc's natbib citation_package mode instead emits \citep{},
-- \citet{}, and \citeyearpar{} depending on citation style — none of
-- which scicite.sty defines. This filter intercepts every citation
-- and converts it to plain \cite{}, bypassing that mismatch entirely.
--
-- Must run AFTER pandoc-crossref in the filter chain (pandoc-crossref
-- needs to resolve @fig:label / @tbl:label references into their own
-- AST nodes first; only genuine bibliography citations should still
-- be Cite nodes by the time this filter runs).

function Cite(el)
  local keys = {}
  for _, c in ipairs(el.citations) do
    table.insert(keys, c.id)
  end
  return pandoc.RawInline("latex", "\\cite{" .. table.concat(keys, ",") .. "}")
end

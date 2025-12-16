from fontTools.ttLib import TTFont
font = TTFont("/usr/share/fonts/noto/NotoColorEmoji.ttf")
cmap = font.getBestCmap(); assert cmap
revCmap = {}
for codepoint, name in cmap.items(): revCmap[name] = codepoint
def doLig(lig, gly1cp):
    codepointSeq = [gly1cp]
    for name in lig.Component:
        if name not in revCmap: return
        codepointSeq.append(revCmap[name])
    grapheme = "".join(map(chr, codepointSeq))
    u = "".join([f"\\u{c:04X}" for c in codepointSeq])
    print(f"{len(grapheme.encode('utf-8'))} {u} {grapheme}")
for lookup_idx, lookup in enumerate(font['GSUB'].table.LookupList.Lookup):
    if lookup.LookupType == 4: # Ligature Substitution
        for subtable_idx, subtable in enumerate(lookup.SubTable):
            if not hasattr(subtable, 'ligatures'): continue
            for name, ligList in subtable.ligatures.items():
                if name not in revCmap: continue
                gly1cp = revCmap[name]
                for lig in ligList: doLig(lig, gly1cp)

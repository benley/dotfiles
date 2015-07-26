" Vim syntax file
" Language: AutoCAD DXF
" Maintainer: Benjamin Staffin

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn keyword dxfSectionMark SECTION ENDSEC TABLE ENDTAB
syn keyword dxfSection HEADER CLASSES TABLES BLOCKS ENTITIES OBJECTS
syn keyword dxfTable APPID BLOCK_RECORD DIMSTYLE LAYER LTYPE STYLE UCS VIEW VPORT

syn keyword dxfEntity CLASS BLOCK ENDBLK DICTIONARY
syn keyword dxfEntity IMAGE IMAGEDEF IMAGEDEF_REACTOR
syn keyword dxfEntity LAYOUT MTEXT

" syn match dxfSection /SECTION.*ENDSEC/
"       \ contains=dxfSections

" syn region dxfSection start="SECTION" end="ENDSEC" fold transparent
"       \ matchgroup=dxfSection
"       \ contains=dxfSections

hi def link dxfSectionMark Keyword
hi def link dxfSection Structure
hi def link dxfTable Identifier
hi def link dxfEntity Function


let b:current_syntax = "dxf"

let &cpo = s:cpo_save
unlet s:cpo_save

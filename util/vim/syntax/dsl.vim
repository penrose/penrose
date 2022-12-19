" Vim syntax file
" Language: Penrose Domain
" Maintainer: Keenan Crane
" Latest Revision: 23 October 2020

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword dslKeywords type predicate constructor function notation symmetric

" Comments
syn keyword dslTodo TODO FIXME XXX NOTE
syn match dslComment "--.*$" contains=dslTodo,@Spell

" Types and values
syn match dslString "\"[^"]*\""

let b:current_syntax = "dsl"

hi def link dslTodo Todo
hi def link dslKeywords Statement
hi def link dslComment Comment
hi def link dslString Constant


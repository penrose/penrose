" Vim syntax file
" Language: Penrose Substance
" Maintainer: Keenan Crane
" Latest Revision: 23 October 2020

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword subKeywords Label AutoLabel NoLabel All None

" Comments
syn keyword subTodo TODO FIXME XXX NOTE
syn match subComment "--.*$" contains=subTodo,@Spell

" Types and values
syn match subString "\$[^$]*\$"
syn match subString "\"[^$]*\""

let b:current_syntax = "sub"

hi def link subTodo Todo
hi def link subKeywords Statement
hi def link subComment Comment
hi def link subString Constant


" Vim syntax file
" Language: Penrose Style
" Maintainer: Keenan Crane
" Latest Revision: 23 October 2020

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword styKeywords forall where
syn keyword styKeywords ensure encourage layer
syn keyword styKeywords override delete
syn keyword styKeywords LOCAL
syn keyword styKeywords import

" Comments
syn keyword styTodo TODO FIXME XXX NOTE
syn match styComment "--.*$" contains=styTodo,@Spell

" Types and values
syn keyword styTypes rgba vec2 vec3
syn match styString "\"[^"]*\""

" Numerical values
" Regular int like number with - + or nothing in front
syn match styNumber '\<\d\+\>'
syn match styNumber '[-+]\d\+'
" Floating point number with decimal no E or e (+,-)
syn match styNumber "\d\+\.\d*"
syn match styNumber '[-+]\d\+\.\d*'
" Floating point like number with E and no decimal point (+,-)
syn match styNumber '[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+'
syn match styNumber '\d[[:digit:]]*[eE][\-+]\=\d\+'
" Floating point like number with E and decimal point (+,-)
syn match styNumber '[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
syn match styNumber '\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+'
" Undetermined numerical value
syn match styUndetermined "?"

let b:current_syntax = "sty"

hi def link styTodo Todo
hi def link styKeywords Statement
hi def link styTypes Type
hi def link styComment Comment
hi def link styString Constant
hi def link styNumber Constant
hi def link styUndetermined Constant


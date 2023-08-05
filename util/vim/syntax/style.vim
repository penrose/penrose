" Vim syntax file
" Language: Penrose Style
" Maintainer: Keenan Crane
" Latest Revision: 4 August 2023

if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword styKeywords forall where with collect
syn keyword styKeywords ensure encourage
syn keyword styKeywords layer above below
syn keyword styKeywords override delete
syn keyword styKeywords LOCAL
syn keyword styKeywords import
syn keyword styKeywords has math text label
syn keyword styKeywords none

" Comments
syn keyword styTodo TODO FIXME XXX NOTE
syn match styComment "--.*$" contains=styTodo,@Spell

" Types and values
syn keyword styTypes bool color constraint file function int list mat2x2 mat3x3 mat4x4 objective path scalar shape string style vec2 vec3 vec4 then
syn match styString "\"[^"]*\""

" Highlight as a keyword only if preceded by
" a newline and some whitespace, and if followed by a
" character that can be used as the first character of a
" variable name.
syn match styStringKeyword '\(^\s*\)string\ze\(\s*[a-zA-Z]\)'
syn match styColorKeyword '\(^\s*\)color\ze\(\s*[a-zA-Z]\)'
syn match styShapeKeyword '\(^\s*\)shape\ze\(\s*[a-zA-Z]\)'

" Numerical values
" Regular int like number with - + or nothing in front
syn match styNumber '\<\d\+\>'
syn match styNumber '\<[-+]\d\+\>'
" Floating point number with decimal no E or e (+,-)
syn match styNumber "\<\d\+\.\d*\>"
syn match styNumber '\<[-+]\d\+\.\d*\>'
" Floating point like number with E and no decimal point (+,-)
syn match styNumber '\<[-+]\=\d[[:digit:]]*[eE][\-+]\=\d\+\>'
syn match styNumber '\<\d[[:digit:]]*[eE][\-+]\=\d\+\>'
" Floating point like number with E and decimal point (+,-)
syn match styNumber '\<[-+]\=\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+\>'
syn match styNumber '\<\d[[:digit:]]*\.\d*[eE][\-+]\=\d\+\>'
" Undetermined numerical value
syn match styUndetermined "?"

let b:current_syntax = "style"

hi def link styTodo Todo
hi def link styKeywords Statement
hi def link styLabelKeyword Statement
hi def link styTypes Type
hi def link styStringKeyword Type
hi def link styColorKeyword Type
hi def link styShapeKeyword Type
hi def link styComment Comment
hi def link styString Constant
hi def link styNumber Constant
hi def link styUndetermined Constant


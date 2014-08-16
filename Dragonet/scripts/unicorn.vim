syn case match
syn keyword unicornKeyword graph node port config and or nand or nor
syn keyword unicornKeyword cluster boolean port gconfig attr type semantics helpers spawn predicate
hi def link unicornKeyword Keyword

syn region uniComment start="/\*" end="\*/"
syn region uniComment start="//" end="$"
hi def link uniComment Comment

setlocal expandtab
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4

if exists("b:current_syntax")
  finish
endif



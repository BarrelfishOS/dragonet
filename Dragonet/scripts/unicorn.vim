syn case match
syn keyword unicornKeyword graph node port config and or nand or nor
syn keyword unicornKeyword cluster boolean port gconfig attr type semantics helpers spawn predicate
hi def link unicornKeyword Keyword

syn region uniComment start="/\*" end="\*/"
syn region uniComment start="//" end="$"
hi def link uniComment Comment

if exists("b:current_syntax")
  finish
endif

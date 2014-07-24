syn case match
syn keyword unicornKeyword graph node boolean port and or conf cluster
hi def link unicornKeyword Keyword

syn region uniComment start="/\*" end="\*/"
syn region uniComment start="//" end="$"
hi def link uniComment Comment

if exists("b:current_syntax")
  finish
endif

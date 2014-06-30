" Tracer vim syntax file
" v0.3
" Cyril Deguet <cdeguet_at_amadeus_dot_net>
" Matthieu Dalstein matthieu.dalstein@amadeus.com

if exists("b:current_syntax")
    finish
endif

syn case match

" Colors
" Type: Green
" Statement: Orange
" Special: 
"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" 1st Line
syn match trcDate /^[0-9]\{4}\/[0-9]\{2}\/[0-9]\{2} / nextgroup=trcTime
syn match trcTime /[0-9]\{2}:[0-9]\{2}:[0-9]\{2}\.[0-9]\{6} / contained nextgroup=trcHost,trcLog
syn match trcHost /\S\+ / contained nextgroup=trcProc,trcErr,trcWarn,trcNot,trcInfo
syn match trcProc /\S\+ / contained nextgroup=trcErr,trcWarn,trcNot,trcInfo
syn match trcErr /[A-Z]\{2,4} \(ERROR\|FATAL\|CRIT\) .*/ contained
syn match trcWarn /[A-Z]\{2,4} WARN .*/ contained
syn match trcInfo /[A-Z]\{2,4} INFO .*/ contained
syn match trcNot /[A-Z]\{2,4} NOT .*/ contained
syn match trcLog /\([^ ]* \(APP\|MDW\)\)\@!.*/ contained

hi def link trcDate Normal
hi def link trcTime Type
hi def link trcHost Statement
hi def link trcProc Special
hi def link trcErr Error
hi def link trcWarn LineNr
hi def link trcInfo String
hi def link trcNot ModeMsg
hi def link trcLog WarningMsg
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


"syn region trcLog start="/\([^ ]* \(APP\|MDW\)\)\@!" end="^[0-9]\{4}\/[0-9]\{2}\/[0-9]\{2} " contained

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MSG - 2nd Line with connector
"syn match trcHeader /^Message / nextgroup=trcSense
"syn match trcHeader /sent\|received/ contained nextgroup=trcCon
"syn keyword trcReceived received nextgroup=trcCon1 
syn match trcSent /^Message sent/ nextgroup=trcCon1 
syn match trcReceived /^Message received/ nextgroup=trcCon1 
syn match trcQuery /^Query/ nextgroup=trcCon1 
syn match trcReply /^Reply/ nextgroup=trcCon1 
syn match trcCon1 /[^(S]*(\?/ contained nextgroup=trcConName
syn match trcConName /[^, )]\+/ contained nextgroup=trcCon2
syn match trcCon2 /)\?.*CorrID=/ contained nextgroup=trcCorrID
syn match trcCorrID /[0-9A-Za-z]\+/ contained nextgroup=trcCon3
syn match trcCon3 /.*MsgID=/ contained nextgroup=trcMsgID
syn match trcMsgID /[^\]]*/ contained

hi def link trcReceived Identifier
hi def link trcQuery Identifier
hi def link trcReply Underlined
hi def link trcSent Underlined
hi def link trcCon Constant
hi def link trcSense Constant
hi def link trcConName trcProc
hi def link trcCorrID Constant
hi def link trcMsgID Constant
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" MSG - Payload with ASCII
syn match trcAddr /^[0-9a-fA-F]\{16}  / nextgroup=trcHex
syn match trcHex /\([0-9a-fA-F ]\{8} \)\{4}/ contained nextgroup=trcAscii
syn match trcAscii /\S\+ / contained nextgroup=trcEBCDIC
syn match trcEBCDIC /.*$/ contained

hi def link trcAddr Comment
hi def link trcHex Comment
hi def link trcAscii Normal
hi def link trcEBCDIC Comment
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


let b:current_syntax = "tracer"

" Vim syntax file
" Language: Snowglobe
" Latest Revision: 02 May 2022

if exists("b:current_syntax")
	finish
endif

if (v:version == 704 && has("patch-7.4.1142")) || v:version > 704
	syn iskeyword 48-57,65-90,95,97-122
else
	setl isk=48-57,65-90,95,97-122
endif




" Keywords
syn keyword		sgTypeKeyword			bool f32 f64 i8 i16 i32 i64 never u8 u16 u32 u64
syn keyword		sgBooleanValueKeyword	false true
syn keyword		sgStructKeyword			enum struct 
syn keyword		sgConditionalKeyword	elif else if match
syn keyword		sgLoopKeyword			for while
syn keyword		sgOtherKeyword			as break continue copyable func in locally none ref return reversed some swap var with

" Character/String literals
syn match		sgEscapeSequence		contained '\\[\\nrt0"\']'
syn match		sgInvalidEscapeSequence	contained '\\[^\\nrt0"\']'
syn match		sgCharacterLiteral		"'\(\\.\|[^\\']\)'" contains=sgEscapeSequence,sgInvalidEscapeSequence
syn region		sgStringLiteral			start='"' skip='\\"' end='"' contains=sgEscapeSequence,sgInvalidEscapeSequence

" Error token
syn match		sgErrorToken			'[a-zA-Z_0-9]\+'

" Name token
syn match		sgName					'[a-zA-Z_][a-zA-Z_0-9]*'

" Numeric literals
syn match		sgIntegerLiteralType	contained '[iu]\(8\|16\|32\|64\)\?'
syn match		sgDecInteger			'\<[0-9][0-9_]*\(\(i\|u\)\(8\|16\|32\|64\)\?\)\?\>' contains=sgIntegerLiteralType
syn match		sgBinInteger			'\<0b[01][01_]*\(\(i\|u\)\(8\|16\|32\|64\)\?\)\?\>' contains=sgIntegerLiteralType
syn match		sgOctInteger			'\<0o[0-7][0-7_]*\(\(i\|u\)\(8\|16\|32\|64\)\?\)\?\>' contains=sgIntegerLiteralType
syn match		sgHexInteger			'\<0x[0-9a-fA-F][0-9a-fA-F_]*\(\(i\|u\)\(8\|16\|32\|64\)\?\)\?\>' contains=sgIntegerLiteralType

syn match		sgFloatLiteralType		contained 'f\(32\|64\)\?'
syn match		sgFloat					'\<[0-9][0-9_]*\.[0-9][0-9_]*\(e\(+\|-\)\?[0-9][0-9_]*\)\?\(f\(32\|64\)\?\)\?\>' contains=sgFloatLiteralType
syn match       sgFloat                 '\<[0-9][0-9_]*f\(32\|64\)\?\>' contains=sgFloatLiteralType

" Parenthesis etc. ranges
syn match		sgParenthesisError		')'
syn region		sgParenthesisRange		start='(' end=')' transparent contains=TOP,sgParenthesisError
syn match		sgBracketsError			']'
syn region		sgBracketsRange			start='\[' end=']' transparent contains=TOP,sgBracketsError
syn match		sgBracesError			'}'
syn region		sgBracesRange			start='{' end='}' transparent fold contains=TOP,sgBracesError

" Comments
syn keyword		sgTodoKeyword			contained TODO FIXME
syn match 		sgSingleLineComment 	'//.*$' contains=sgTodoKeyword
syn region		sgMultiLineComment 		start='/\*' end='\*/' contains=sgTodoKeyword



" The weird setting that has to be set
syn sync fromstart

" Set colors
let b:current_syntax = "snowglobe"
hi def link sgTypeKeyword			Type
hi def link sgBooleanValueKeyword	Boolean
hi def link	sgStructKeyword			Structure
hi def link sgConditionalKeyword	Conditional
hi def link sgLoopKeyword			Repeat
hi def link sgOtherKeyword			Keyword

hi def link sgEscapeSequence		SpecialChar
hi def link sgInvalidEscapeSequence	Error
hi def link sgStringLiteral			String
hi def link sgCharacterLiteral		Character

" hi def link sgName					Identifier	" It makes almost everything highlighted

hi def link sgIntegerLiteralType	Special
hi def link sgDecInteger			Number
hi def link sgBinInteger			Number
hi def link sgOctInteger			Number
hi def link sgHexInteger			Number
hi def link sgFloatLiteralType		Special
hi def link sgFloat					Float

hi def link sgParenthesisError		Error
hi def link sgBracketsError			Error
hi def link sgBracesError			Error

hi def link sgErrorToken			Error

hi def link sgTodoKeyword			Todo
hi def link sgSingleLineComment		Comment
hi def link sgMultiLineComment		Comment


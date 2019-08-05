/*
 * Copyright (c) 2019 Broadcom.
 * The term "Broadcom" refers to Broadcom Inc. and/or its subsidiaries.
 *
 * This program and the accompanying materials are made
 * available under the terms of the Eclipse Public License 2.0
 * which is available at https://www.eclipse.org/legal/epl-2.0/
 *
 * SPDX-License-Identifier: EPL-2.0
 *
 * Contributors:
 *   Broadcom, Inc. - initial API and implementation
 */
    
lexer grammar CobolLexer;

LEVEL: ([1-9])|([0][1-9])|([1234][0-9])| '77';
LEVEL_NUMBER_66 : '66';
LEVEL_NUMBER_88 : '88';

// keywords
ABORT : A B O R T;
ACCEPT : A C C E P T;
ACCESS : A C C E S S;
ADD : A D D;
ADDRESS : A D D R E S S;
ADVANCING : A D V A N C I N G;
AFTER : A F T E R;
ALIGNED : A L I G N E D;
ALL : A L L;
ALPHABET : A L P H A B E T;
ALPHABETIC : A L P H A B E T I C;
ALPHABETIC_LOWER : A L P H A B E T I C MINUSCHAR L O W E R;
ALPHABETIC_UPPER : A L P H A B E T I C MINUSCHAR U P P E R;
ALPHANUMERIC : A L P H A N U M E R I C;
ALPHANUMERIC_EDITED : A L P H A N U M E R I C MINUSCHAR E D I T E D;
ALSO : A L S O;
ALTER : A L T E R;
ALTERNATE : A L T E R N A T E;
AND : A N D;
ANY : A N Y;
APPLY: A P P L Y;
ARE : A R E;
AREA : A R E A;
AREAS : A R E A S;
AS : A S;
ASCENDING : A S C E N D I N G;
ASCII : A S C I I;
ASSIGN : A S S I G N;
ASSOCIATED_DATA : A S S O C I A T E D MINUSCHAR D A T A;
ASSOCIATED_DATA_LENGTH : A S S O C I A T E D MINUSCHAR D A T A MINUSCHAR L E N G T H;
AT : A T;
ATTRIBUTE : A T T R I B U T E;
AUTHOR : A U T H O R;
AUTO : A U T O;
AUTO_SKIP : A U T O MINUSCHAR S K I P;
BACKGROUND_COLOR : B A C K G R O U N D MINUSCHAR C O L O R;
BACKGROUND_COLOUR : B A C K G R O U N D MINUSCHAR C O L O U R;
BASIS : B A S I S;
BEEP : B E E P;
BEFORE : B E F O R E;
BEGINNING : B E G I N N I N G;
BELL : B E L L;
BINARY : B I N A R Y;
BIT : B I T;
BLANK : B L A N K;
BLINK : B L I N K;
BLOB : B L O B;
BLOCK : B L O C K;
BOUNDS : B O U N D S;
BOTTOM : B O T T O M;
BY : B Y;
BYFUNCTION : B Y F U N C T I O N;
BYTITLE : B Y T I T L E;
CALL : C A L L;
CANCEL : C A N C E L;
CAPABLE : C A P A B L E;
CCSVERSION : C C S V E R S I O N;
CD : C D;
CF : C F;
CH : C H;
CHAINING : C H A I N I N G;
CHANGED : C H A N G E D;
CHANNEL : C H A N N E L;
CHARACTER : C H A R A C T E R;
CHARACTERS : C H A R A C T E R S;
CLASS : C L A S S;
CLASS_ID : C L A S S MINUSCHAR I D;
CLOB : C L O B;
CLOCK_UNITS : C L O C K MINUSCHAR U N I T S;
CLOSE : C L O S E;
CLOSE_DISPOSITION : C L O S E MINUSCHAR D I S P O S I T I O N;
COBOL : C O B O L;
CODE : C O D E;
CODE_SET : C O D E MINUSCHAR S E T;
COLLATING : C O L L A T I N G;
COL : C O L;
COLUMN : C O L U M N;
COM_REG : C O M MINUSCHAR R E G;
COMMA : C O M M A;
COMMITMENT : C O M M I T M E N T;
COMMON : C O M M O N;
COMMUNICATION : C O M M U N I C A T I O N;
COMP : C O M P;
COMP_1 : C O M P MINUSCHAR '1';
COMP_2 : C O M P MINUSCHAR '2';
COMP_3 : C O M P MINUSCHAR '3';
COMP_4 : C O M P MINUSCHAR '4';
COMP_5 : C O M P MINUSCHAR '5';
COMPUTATIONAL : C O M P U T A T I O N A L;
COMPUTATIONAL_1 : C O M P U T A T I O N A L MINUSCHAR '1';
COMPUTATIONAL_2 : C O M P U T A T I O N A L MINUSCHAR '2';
COMPUTATIONAL_3 : C O M P U T A T I O N A L MINUSCHAR '3';
COMPUTATIONAL_4 : C O M P U T A T I O N A L MINUSCHAR '4';
COMPUTATIONAL_5 : C O M P U T A T I O N A L MINUSCHAR '5';
COMPUTE : C O M P U T E;
CONFIGURATION : C O N F I G U R A T I O N;
CONTAINS : C O N T A I N S;
CONTENT : C O N T E N T;
CONTINUE : C O N T I N U E;
CONTROL : C O N T R O L;
CONTROL_POINT : C O N T R O L MINUSCHAR P O I N T;
CONTROLS : C O N T R O L S;
CONVENTION : C O N V E N T I O N;
CONVERTING : C O N V E R T I N G;
COPY : C O P Y;
CORR : C O R R;
CORRESPONDING : C O R R E S P O N D I N G;
COUNT : C O U N T;
CRUNCH : C R U N C H;
CURRENCY : C U R R E N C Y;
CURSOR : C U R S O R;
DATA : D A T A;
DATA_BASE : D A T A MINUSCHAR B A S E;
DATE : D A T E;
DATE_COMPILED : D A T E MINUSCHAR C O M P I L E D;
DATE_WRITTEN : D A T E MINUSCHAR W R I T T E N;
DAY : D A Y;
DAY_OF_WEEK : D A Y MINUSCHAR O F MINUSCHAR W E E K;
DBCS : D B C S;
DBCLOB : D B C L O B;
DE : D E;
DEBUG_CONTENTS : D E B U G MINUSCHAR C O N T E N T S;
DEBUG_ITEM : D E B U G MINUSCHAR I T E M;
DEBUG_LINE : D E B U G MINUSCHAR L I N E;
DEBUG_NAME : D E B U G MINUSCHAR N A M E;
DEBUG_SUB_1 : D E B U G MINUSCHAR S U B MINUSCHAR '1';
DEBUG_SUB_2 : D E B U G MINUSCHAR S U B MINUSCHAR '2';
DEBUG_SUB_3 : D E B U G MINUSCHAR S U B MINUSCHAR '3';
DEBUGGING : D E B U G G I N G;
DECIMAL_POINT : D E C I M A L MINUSCHAR P O I N T;
DECLARATIVES : D E C L A R A T I V E S;
DEFAULT : D E F A U L T;
DEFAULT_DISPLAY : D E F A U L T MINUSCHAR D I S P L A Y;
DEFINITION : D E F I N I T I O N;
DELETE : D E L E T E;
DELIMITED : D E L I M I T E D;
DELIMITER : D E L I M I T E R;
DEPENDING : D E P E N D I N G;
DESCENDING : D E S C E N D I N G;
DESTINATION : D E S T I N A T I O N;
DETAIL : D E T A I L;
DFHRESP : D F H R E S P;
DFHVALUE : D F H V A L U E;
DISABLE : D I S A B L E;
DISK : D I S K;
DISPLAY : D I S P L A Y;
DISPLAY_1 : D I S P L A Y MINUSCHAR '1';
DIVIDE : D I V I D E;
DIVISION : D I V I S I O N;
DONTCARE : D O N T C A R E;
DOUBLE : D O U B L E;
DOWN : D O W N;
DUPLICATES : D U P L I C A T E S;
DYNAMIC : D Y N A M I C;
EBCDIC : E B C D I C;
EGCS : E G C S; // E X T E N S I O N
EGI : E G I;
ELSE : E L S E;
EMI : E M I;
EMPTY_CHECK : E M P T Y MINUSCHAR C H E C K;
ENABLE : E N A B L E;
ENCODING: E N C O D I N G;
END : E N D;
END_ACCEPT : E N D MINUSCHAR A C C E P T;
END_ADD : E N D MINUSCHAR A D D;
END_CALL : E N D MINUSCHAR C A L L;
END_COMPUTE : E N D MINUSCHAR C O M P U T E;
END_DELETE : E N D MINUSCHAR D E L E T E;
END_DIVIDE : E N D MINUSCHAR D I V I D E;
END_EVALUATE : E N D MINUSCHAR E V A L U A T E;
END_IF : E N D MINUSCHAR I F;
END_MULTIPLY : E N D MINUSCHAR M U L T I P L Y;
END_OF_PAGE : E N D MINUSCHAR O F MINUSCHAR P A G E;
END_PERFORM : E N D MINUSCHAR P E R F O R M;
END_READ : E N D MINUSCHAR R E A D;
END_RECEIVE : E N D MINUSCHAR R E C E I V E;
END_REMARKS : E N D MINUSCHAR R E M A R K S;
END_RETURN : E N D MINUSCHAR R E T U R N;
END_REWRITE : E N D MINUSCHAR R E W R I T E;
END_SEARCH : E N D MINUSCHAR S E A R C H;
END_START : E N D MINUSCHAR S T A R T;
END_STRING : E N D MINUSCHAR S T R I N G;
END_SUBTRACT : E N D MINUSCHAR S U B T R A C T;
END_UNSTRING : E N D MINUSCHAR U N S T R I N G;
END_WRITE : E N D MINUSCHAR W R I T E;
END_XML : E N D MINUSCHAR X M L;
ENDING : E N D I N F;
ENTER : E N T E R;
ENTRY : E N T R Y;
ENTRY_PROCEDURE : E N T R Y MINUSCHAR P R O C E D U R E;
ENVIRONMENT : E N V I R O N M E N T;
EOP : E O P;
EQUAL : E Q U A L;
ERASE : E R A S E;
ERROR : E R R O R;
EOL : E O L;
EOS : E O S;
ESCAPE : E S C A P E;
ESI : E S I;
EVALUATE : E V A L U A T E;
EVENT : E V E N T;
EVERY : E V E R Y;
EXCEPTION : E X C E P T I O N;
EXCLUSIVE : E X C L U S I V E;
EXHIBIT : E X H I B I T;
EXIT : E X I T;
EXPORT : E X P O R T;
EXTEND : E X T E N D;
EXTENDED : E X T E N D E D;
EXTERNAL : E X T E R N A L;
FALSE : F A L S E;
FD : F D;
FILE : F I L E;
FILE_CONTROL : F I L E MINUSCHAR C O N T R O L;
FILLER : F I L L E R;
FINAL : F I N A L;
FIRST : F I R S T;
FOOTING : F O O T I N G;
FOR : F O R;
FOREGROUND_COLOR : F O R E G R O U N D MINUSCHAR C O L O R;
FOREGROUND_COLOUR : F O R E G R O U N D MINUSCHAR C O L O U R;
FROM : F R O M;
FULL : F U L L;
FUNCTION : F U N C T I O N;
FUNCTIONNAME : F U N C T I O N N A M E;
FUNCTION_POINTER : F U N C T I O N MINUSCHAR P O I N T E R;
GENERATE : G E N E R A T E;
GOBACK : G O B A C K;
GIVING : G I V I N G;
GLOBAL : G L O B A L;
GO : G O;
GREATER : G R E A T E R;
GRID : G R I D;
GROUP : G R O U P;
GROUP_USAGE: G R O U P MINUSCHAR U S A G E;
HEADING : H E A D I N G;
HIGHLIGHT : H I G H L I G H T;
HIGH_VALUE : H I G H MINUSCHAR V A L U E;
HIGH_VALUES : H I G H MINUSCHAR V A L U E S;
I_O : I MINUSCHAR O;
I_O_CONTROL : I MINUSCHAR O MINUSCHAR C O N T R O L;
ID : I D;
IDENTIFICATION : I D E N T I F I C A T I O N;
IF : I F;
IMPLICIT : I M P L I C I T;
IMPORT : I M P O R T;
IN : I N;
INDEX : I N D E X;
INDEXED : I N D E X E D;
INDICATE : I N D I C A T E;
INITIAL : I N I T I A L;
INITIALIZE : I N I T I A L I Z E;
INITIATE : I N I T I A T E;
INPUT : I N P U T;
INPUT_OUTPUT : I N P U T MINUSCHAR O U T P U T;
INSPECT : I N S P E C T;
INSTALLATION : I N S T A L L A T I O N;
INTEGER : I N T E G E R;
INTO : I N T O;
INVALID : I N V A L I D;
INVOKE : I N V O K E;
IS : I S;
JUST : J U S T;
JUSTIFIED : J U S T I F I E D;
KANJI : K A N J I;
KEPT : K E P T;
KEY : K E Y;
KEYBOARD : K E Y B O A R D;
LABEL : L A B E L;
LANGUAGE : L A N G U A G E;
LAST : L A S T;
LB : L B;
LD : L D;
LEADING : L E A D I N G;
LEFT : L E F T;
LEFTLINE : L E F T L I N E;
LENGTH : L E N G T H;
LENGTH_CHECK : L E N G T H MINUSCHAR C H E C K;
LESS : L E S S;
LIBACCESS : L I B A C C E S S;
LIBPARAMETER : L I B P A R A M E T E R;
LIBRARY : L I B R A R Y;
LIMIT : L I M I T;
LIMITS : L I M I T S;
LINAGE : L I N A G E;
LINAGE_COUNTER : L I N A G E MINUSCHAR C O U N T E R;
LINE : L I N E;
LINES : L I N E S;
LINE_COUNTER : L I N E MINUSCHAR C O U N T E R;
LINKAGE : L I N K A G E;
LIST : L I S T;
LOCAL : L O C A L;
LOCAL_STORAGE : L O C A L MINUSCHAR S T O R A G E;
LOCK : L O C K;
LONG_DATE : L O N G MINUSCHAR D A T E;
LONG_TIME : L O N G MINUSCHAR T I M E;
LOWER : L O W E R;
LOWLIGHT : L O W L I G H T;
LOW_VALUE : L O W MINUSCHAR V A L U E;
LOW_VALUES : L O W MINUSCHAR V A L U E S;
MEMORY : M E M O R Y;
MERGE : M E R G E;
MESSAGE : M E S S A G E;
MMDDYYYY : M M D D Y Y Y Y;
MODE : M O D E;
MODULES : M O D U L E S;
MORE_LABELS : M O R E MINUSCHAR L A B E L S;
MOVE : M O V E;
MULTIPLE : M U L T I P L E;
MULTIPLY : M U L T I P L Y;
NAMED : N A M E D;
NATIONAL : N A T I O N A L;
NATIONAL_EDITED : N A T I O N A L MINUSCHAR E D I T E D;
NATIVE : N A T I V E;
NEGATIVE : N E G A T I V E;
NETWORK : N E T W O R K;
NEXT : N E X T;
NO : N O;
NO_ECHO : N O MINUSCHAR E C H O;
NOT : N O T;
NULL : N U L L;
NULLS : N U L L S;
NUMBER : N U M B E R;
NUMERIC : N U M E R I C;
NUMERIC_DATE : N U M E R I C MINUSCHAR D A T E;
NUMERIC_EDITED : N U M E R I C MINUSCHAR E D I T E D;
NUMERIC_TIME : N U M E R I C MINUSCHAR T I M E;
OBJECT_COMPUTER : O B J E C T MINUSCHAR C O M P U T E R;
OCCURS : O C C U R S;
ODT : O D T;
OF : O F;
OFF : O F F;
OMITTED : O M I T T E D;
ON : O N;
OPEN : O P E N;
OPTIONAL : O P T I O N A L;
OR : O R;
ORDER : O R D E R;
ORDERLY : O R D E R L Y;
ORGANIZATION : O R G A N I Z A T I O N;
OTHER : O T H E R;
OUTPUT : O U T P U T;
OVERFLOW : O V E R F L O W;
OVERLINE : O V E R L I N E;
OWN : O W N;
PACKED_DECIMAL : P A C K E D MINUSCHAR D E C I M A L;
PADDING : P A D D I N G;
PAGE : P A G E;
PAGE_COUNTER : P A G E MINUSCHAR C O U N T E R;
PARSE: P A R S E;
PASSWORD : P A S S W O R D;
PERFORM : P E R F O R M;
PF : P F;
PH : P H;
PIC : P I C  -> pushMode(PICTURECLAUSE);
PICTURE : P I C T U R E -> pushMode(PICTURECLAUSE);
PLUS : P L U S;
POINTER : P O I N T E R;
POSITION : P O S I T I O N;
POSITIVE : P O S I T I V E;
PORT : P O R T;
PRINTER : P R I N T E R;
PRINTING : P R I N T I N G;
PRIVATE : P R I V A T E;
PROCEDURE : P R O C E D U R E;
PROCEDURE_POINTER : P R O C E D U R E MINUSCHAR P O I N T E R;
PROCEDURES : P R O C E D U R E S;
PROCEED : P R O C E E D;
PROCESS : P R O C E S S;
PROCESSING: P R O C E S S I N G;
PROGRAM : P R O G R A M;
PROGRAM_ID : P R O G R A M MINUSCHAR I D;
PROGRAM_LIBRARY : P R O G R A M MINUSCHAR L I B R A R Y;
PROMPT : P R O M P T;
PURGE : P U R G E;
QUEUE : Q U E U E;
QUOTE : Q U O T E;
QUOTES : Q U O T E S;
RANDOM : R A N D O M;
READER : R E A D E R;
REMOTE : R E M O T E;
RD : R D;
REAL : R E A L;
READ : R E A D;
RECEIVE : R E C E I V E;
RECEIVED : R E C E I V E D;
RECORD : R E C O R D;
RECORDING : R E C O R D I N G;
RECORDS : R E C O R D S;
RECURSIVE : R E C U R S I V E;
REDEFINES : R E D E F I N E S;
REEL : R E E L;
REF : R E F;
REFERENCE : R E F E R E N C E;
REFERENCES : R E F E R E N C E S;
RELATIVE : R E L A T I V E;
RELEASE : R E L E A S E;
RELOAD: R E L O A D;
REMAINDER : R E M A I N D E R;
REMARKS : R E M A R K S;
REMOVAL : R E M O V A L;
REMOVE : R E M O V E;
RENAMES : R E N A M E S;
REPLACE : R E P L A C E;
REPLACING : R E P L A C I N G;
REPORT : R E P O R T;
REPORTING : R E P O R T I N G;
REPORTS : R E P O R T S;
REQUIRED : R E Q U I R E D;
RERUN : R E R U N;
RESERVE : R E S E R V E;
REVERSE_VIDEO : R E S E R V E MINUSCHAR V I D E O;
RESET : R E S E T;
RETURN : R E T U R N;
RETURN_CODE : R E T U R N MINUSCHAR C O D E;
RETURNING: R E T U R N I N G;
REVERSED : R E V E R S E D;
REWIND : R E W I N D;
REWRITE : R E W R I T E;
RF : R F;
RH : R H;
RIGHT : R I G H T;
ROUNDED : R O U N D E D;
RUN : R U N;
SAME : S A M E;
SAVE : S A V E;
SCREEN : S C R E E N;
SD : S D;
SEARCH : S E A R C H;
SECTION : S E C T I O N;
SECURE : S E C U R E;
SECURITY : S E C U R I T Y;
SEGMENT : S E G M E N T;
SEGMENT_LIMIT : S E G M E N T MINUSCHAR L I M I T;
SELECT : S E L E C T;
SEND : S E N D;
SENTENCE : S E N T E N C E;
SEPARATE : S E P A R A T E;
SEQUENCE : S E Q U E N C E;
SEQUENTIAL : S E Q U E N T I A L;
SERVICE: S E R V I C E;
SET : S E T;
SHARED : S H A R E D;
SHAREDBYALL : S H A R E D B Y A L L;
SHAREDBYRUNUNIT : S H A R E D B Y R U N U N I T;
SHARING : S H A R I N G;
SHIFT_IN : S H I F T MINUSCHAR I N;
SHIFT_OUT : S H I F T MINUSCHAR O U T;
SHORT_DATE : S H O R T MINUSCHAR D A T E;
SIGN : S I G N;
SIZE : S I Z E;
SKIP1: S K I P '1';
SKIP2: S K I P '2';
SKIP3: S K I P '3';
SORT : S O R T;
SORT_CONTROL : S O R T MINUSCHAR C O N T R O L;
SORT_CORE_SIZE : S O R T MINUSCHAR C O R E MINUSCHAR S I Z E;
SORT_FILE_SIZE : S O R T MINUSCHAR F I L E MINUSCHAR S I Z E;
SORT_MERGE : S O R T MINUSCHAR M E R G E;
SORT_MESSAGE : S O R T MINUSCHAR M E S S A G E;
SORT_MODE_SIZE : S O R T MINUSCHAR M O D E MINUSCHAR S I Z E;
SORT_RETURN : S O R T MINUSCHAR R E T U R N;
SOURCE : S O U R C E;
SOURCE_COMPUTER : S O U R C E MINUSCHAR C O M P U T E R;
SPACE : S P A C E;
SPACES : S P A C E S;
SPECIAL_NAMES : S P E C I A L MINUSCHAR N A M E S;
SQL : S Q L;
STANDARD : S T A N D A R D;
STANDARD_1 : S T A N D A R D MINUSCHAR '1';
STANDARD_2 : S T A N D A R D MINUSCHAR '2';
START : S T A R T;
STATUS : S T A T U S;
STOP : S T O P;
STRING : S T R I N G;
SUB_QUEUE_1 : S U B MINUSCHAR Q U E U E MINUSCHAR '1';
SUB_QUEUE_2 : S U B MINUSCHAR Q U E U E MINUSCHAR '2';
SUB_QUEUE_3 : S U B MINUSCHAR Q U E U E MINUSCHAR '3';
SUBTRACT : S U B T R A C T;
SUM : S U M;
SUPPRESS : S U P P R E S S;
SYMBOL : S Y M B O L;
SYMBOLIC : S Y M B O L I C;
SYNC : S Y N C;
SYNCHRONIZED : S Y N C H R O N I Z E D;
TABLE : T A B L E;
TALLY : T A L L Y;
TALLYING : T A L L Y I N G;
TASK : T A S K;
TAPE : T A P E;
TERMINAL : T E R M I N A L;
TERMINATE : T E R M I N A T E;
TEST : T E S T;
TEXT : T E X T;
THAN : T H A N;
THEN : T H E N;
THREAD : T H R E A D;
THREAD_LOCAL : T H R E A D MINUSCHAR L O C A L;
THROUGH : T H R O U G H;
THRU : T H R U;
TIME : T I M E;
TIMER : T I M E R;
TIMES : T I M E S;
TITLE : T I T L E;
TO : T O;
TODAYS_DATE : T O D A Y S MINUSCHAR D A T E;
TODAYS_NAME : T O D A Y S MINUSCHAR N A M E;
TOP : T O P;
TRAILING : T R A I L I N G;
TRUE : T R U E;
TRUNCATED : T R U N C A T E D;
TYPE : T Y P E;
TYPEDEF : T Y P E D E F;
UNDERLINE : U N D E R L I N E;
UNIT : U N I T;
UNSTRING : U N S T R I N G;
UNTIL : U N T I L;
UP : U P;
UPON : U P O N;
USAGE : U S A G E;
USE : U S E;
USING : U S I N G;
VALIDATING: V A L I D A T I N G;
VALUE : V A L U E;
VALUES : V A L U E S;
VARYING : V A R Y I N G;
VIRTUAL : V I R T U A L;
WAIT : W A I T;
WHEN : W H E N;
WHEN_COMPILED : W H E N MINUSCHAR C O M P I L E D;
WITH : W I T H;
WORDS : W O R D S;
WORKING_STORAGE : W O R K I N G MINUSCHAR S T O R A G E;
WRITE_ONLY : W R I T E MINUSCHAR O N L Y;
WRITE : W R I T E;
XML: X M L;
YEAR : Y E A R;
YYYYMMDD : Y Y Y Y M M D D;
YYYYDDD : Y Y Y Y D D D;
ZERO : Z E R O;
ZERO_FILL : Z E R O MINUSCHAR F I L L;
ZEROS : Z E R O S;
ZEROES : Z E R O E S;
CR: C R;
DB: D B;

// symbols
AMPCHAR : '&';
ASTERISKCHAR : '*';
DOUBLEASTERISKCHAR : '**';
COLONCHAR : ':';
COMMACHAR : ',';
COMMENTENTRYTAG : '*>CE';
COMMENTTAG : '*>';
DOLLARCHAR : '$';
DOUBLEQUOTE : '"';
// period full stopPosition
DOT_FS : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ | '.' EOF;
DOT : '.';
EQUALCHAR : '=';
EXECCICSTAG : '*>EXECCICS';
EXECSQLTAG : '*>EXECSQL';
EXECSQLIMSTAG : '*>EXECSQLIMS';
LESSTHANCHAR : '<';
LESSTHANOREQUAL : '<=';
LPARENCHAR : '(';
MINUSCHAR : '-';
MORETHANCHAR : '>';
MORETHANOREQUAL : '>=';
NOTEQUALCHAR : '<>';
PLUSCHAR : '+';
SINGLEQUOTE : '\'';
RPARENCHAR : ')';
SLASHCHAR : '/';

INTEGERLITERAL : (PLUSCHAR | MINUSCHAR)? DIGIT+;

NUMERICLITERAL : (PLUSCHAR | MINUSCHAR)? DIGIT* (DOT | COMMACHAR) DIGIT+ (('e' | 'E') (PLUSCHAR | MINUSCHAR)? DIGIT+)?;

NONNUMERICLITERAL : UNTRMSTRINGLITERAL | STRINGLITERAL | DBCSLITERAL | HEXNUMBER | NULLTERMINATED;

IDENTIFIER : ([a-zA-Z0-9]+ ([-_]+ [a-zA-Z0-9]+)*);

// whitespace, line breaks, comments, ...
NEWLINE : '\r'? '\n' -> channel(HIDDEN);
EXECCICSLINE : EXECCICSTAG WS ~('\n' | '\r' | '}')* ('\n' | '\r' | '}');
EXECSQLIMSLINE : EXECSQLIMSTAG WS ~('\n' | '\r' | '}')* ('\n' | '\r' | '}');
EXECSQLLINE : EXECSQLTAG WS ~('\n' | '\r' | '}')* ('\n' | '\r' | '}');
COMMENTENTRYLINE : COMMENTENTRYTAG WS ~('\n' | '\r')*;
COMMENTLINE : COMMENTTAG WS ~('\n' | '\r')* -> channel(HIDDEN);
WS : [ \t\f;]+ -> channel(HIDDEN);
SEPARATOR : ', ' -> channel(HIDDEN);

fragment DIGIT: [0-9];

fragment HEXNUMBER :
	X '"' [0-9A-F]+ '"'
	| X '\'' [0-9A-F]+ '\''
;

fragment NULLTERMINATED :
	Z '"' (~["\n\r] | '""' | '\'')* '"'
	| Z '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment STRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')* '"'
	| '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment UNTRMSTRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')*
	| '\'' (~['\n\r] | '\'\'' | '"')*
;

fragment DBCSLITERAL :
	[GN] '"' (~["\n\r] | '""' | '\'')* '"'
	| [GN] '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');

mode PICTURECLAUSE;
FINALCHARSTRING: CHARSTRING+ ->popMode;
CHARSTRING: PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)
			PICTURECHARSGROUP1+ PICTURECHARSGROUP2? LParIntegralRPar?|
			PICTURECHARSGROUP1+ '.' PICTURECHARSGROUP1+|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP1* PICTURECHARSGROUP2? PICTURECHARSGROUP1+ LParIntegralRPar?|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar? '.'? (PICTURECHARSGROUP1|PICTURECHARSGROUP2)|
			PICTURECHARSGROUP2 PICTURECHARSGROUP1* LParIntegralRPar?
;

DOT_FS2 : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ -> popMode;
PICTURECHARSGROUP1: PICTURECharAcceptedMultipleTime+;
PICTURECHARSGROUP2: PICTURECharAcceptedOneTime+;
WS2 : [ \t\f;]+ -> channel(HIDDEN);
IS2: I S;
LParIntegralRPar: LPARENCHAR INTEGERLITERAL RPARENCHAR;
fragment PICTURECharAcceptedMultipleTime: ('0'|'9'|A|B|G|N|P|X|Z|PLUSCHAR|MINUSCHAR|SLASHCHAR|ASTERISKCHAR|COMMACHAR|DOLLARCHAR);
fragment PICTURECharAcceptedOneTime: (V|E|S|CR|DB);
#include "bcpl.h"
#include "emas.h"
#include "listhdr.h"
#include "comphdr.h"
#include "redhdr.h"

#include <ctype.h>

// KRC LEX ANALYSER

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

#define DECIMALS 0	// code for reading decimals in equation numbers is ill

// Global variables owned by lex.c
WORD ERRORFLAG, EQNFLAG, EXPFLAG, COMMENTFLAG;
BOOL SKIPCOMMENTS;
LIST TOKENS = 0;
ATOM THE_ID = 0;
LIST THE_CONST = 0;
WORD THE_NUM, THE_DECIMALS;

// Local function declarations
#ifdef DECIMALS
static WORD	PEEKDIGIT(void);
#endif
static TOKEN	READTOKEN(void);
static WORD	READ_DECIMALS(void); //RETURNS VALUE IN HUNDREDTHS
static WORD	PEEKALPHA(void);

// Local variables
static BOOL EXPECTFILE=FALSE;
static TOKEN MISSING;

// READS THE NEXT LINE INTO "TOKENS"
VOID
READLINE()
{  do
   {  LIST *P=&TOKENS;
      WRITES(emas_PROMPT);
      TOKEN T=0;
      MISSING=0;
      TOKENS=NIL;
      THE_DECIMALS=0;
      ERRORFLAG=FALSE;
      EXPFLAG=FALSE;  // WILL GET SET IF THE LINE CONTAINS "?" OR "!"
      COMMENTFLAG=0;  //>0 records number of lines in the comment
      EXPECTFILE=FALSE;
      EQNFLAG=FALSE;  //WILL GET SET IF THE LINE CONTAINS "="
      do { T=READTOKEN();
           *P=CONS((LIST)T,NIL); P=&(TL(*P)); // GCC
         } REPEATUNTIL (T==(TOKEN)EOL || T==(TOKEN)ENDSTREAMCH ||
			T==(TOKEN)BADTOKEN);
      // Ignore first line of Unix script file
      IF HD(TOKENS)==(LIST)'#' && ISCONS(TL(TOKENS)) &&
         HD(TL(TOKENS))==(LIST)'!' DO continue;;
      IF T==(TOKEN)EOL || T==(TOKEN)ENDSTREAMCH DO return;
      WRITES("Closing quote missing - line ignored\n");
      ERRORFLAG=TRUE; return;
   } REPEAT;
}

#define NOTCH(CH) (CH=='\\'||CH=='~' && LEGACY)

static TOKEN
READTOKEN(void)
// TOKEN ::= CHAR | <CERTAIN DIGRAPHS, REPRESENTED BY NOS ABOVE 256> |
//          | CONS(IDENT,ATOM) | CONS(CONST,<ATOM|NUM>)
{  WORD CH=RDCH();
   WHILE (CH==' '||CH=='\t') DO CH=RDCH();
   IF CH=='\n' DO return (TOKEN)EOL;
   IF CH==EOF  DO return (TOKEN)ENDSTREAMCH;
   IF ('a'<=CH && CH<='z') || ('A'<=CH && CH<='Z')
      // || (CH=='_' && PEEKALPHA()) //expt to allow _ID, discontinued
	|| (EXPECTFILE && !isspace(CH))
   DO {do{  BUFCH(CH);
            CH=RDCH();
         } while ('a'<=CH&&CH<='z'||'A'<=CH&&CH<='Z'||
			isdigit(CH)||CH=='\''||CH=='_'||
			(EXPECTFILE && !isspace(CH)));
         UNRDCH(CH);
      {  LIST X=(LIST)PACKBUFFER();
         IF TOKENS!=NIL && HD(TOKENS)==(TOKEN)'/' &&
	     TL(TOKENS)==NIL && MEMBER(FILECOMMANDS,X)
         DO EXPECTFILE=TRUE;
         return CONS((LIST)IDENT,X);  }  }
#if DECIMALS
   // EMAS's READN GOBBLES THE FIRST CHAR AFTER THE NUMBER AND LEAVES IT IN
   // GLOBAL VARIABLE "TERMINATOR". obcpl ALSO EATS THE FOLLOWING CHAR BUT
   // DOESN'T HAVE "TERMINATOR" WHILE Richards' 2013 BCPL DOESN'T GOBBLE IT.
   // CONCLUSION: DON'T USE READN()
   IF isdigit(CH) || CH=='.' && TOKENS==NIL && PEEKDIGIT()
   DO {  TEST CH=='.'
         THEN {  THE_NUM==0;
                 TERMINATOR=='.';  }
         OR {  UNRDCH(CH) ; THE_NUM=READN();  }
         TEST TOKENS==NIL && TERMINATOR=='.'  //LINE NUMBERS (ONLY) ARE
         THEN THE_DECIMALS==READ_DECIMALS();  //ALLOWED A DECIMAL PART
         OR UNRDCH(CH);
         return CONS(CONST,STONUM(THE_NUM)); }
#else
   IF isdigit(CH)
   DO {  THE_NUM  = 0;
         WHILE isdigit(CH)
         DO {  THE_NUM = THE_NUM * 10 + CH - '0';
               IF THE_NUM < 0
               DO {  WRITES("\n**integer overflow**\n");
                     ESCAPETONEXTCOMMAND();  }
	       CH = RDCH();  }
         IF CH != EOF DO UNRDCH(CH);
         return CONS((TOKEN)CONST,STONUM(THE_NUM)); }
#endif
   IF CH=='"'
   DO {  ATOM A;
         CH=RDCH();
         UNTIL (CH=='"'||CH=='\n'||CH==EOF)
         DO {  TEST CH=='\\' //add C escape chars, DT 2015
               THEN { CH=RDCH();
                      switch(CH)
                      { case 'a': BUFCH('\a'); break;
                        case 'b': BUFCH('\b'); break;
                        case 'f': BUFCH('\f'); break;
                        case 'n': BUFCH('\n'); break;
                        case 'r': BUFCH('\r'); break;
                        case 't': BUFCH('\t'); break;
                        case 'v': BUFCH('\v'); break;
                        case '\\': BUFCH('\\'); break;
                        case '\'': BUFCH('\''); break;
                        case '\"': BUFCH('\"'); break;
                        case '\n': return (TOKEN)BADTOKEN;
                        default: IF '0'<=CH&&CH<='9'
                                 DO { int i=3,n=CH-'0',n1;
                                      CH=RDCH();
                                      WHILE --i && '0'<=CH&&CH<='9' && (n1=10*n+CH-'0')<256
                                      DO n=n1, CH=RDCH();
                                      BUFCH(n);
                                      UNRDCH(CH); }
                    } }
               OR BUFCH(CH);
               CH=RDCH();  }
         A=PACKBUFFER();
         return CH!='"' ? (TOKEN)BADTOKEN : CONS(CONST,(LIST)A);  }
{  WORD CH2=RDCH();
   IF CH==':' && CH2=='-' && TOKENS!=NIL && ISCONS(HD(TOKENS)) &&
      HD(HD(TOKENS))==IDENT && TL(TOKENS)==NIL
   DO {  LIST C=NIL;
         LIST SUBJECT=TL(HD(TOKENS));
         COMMENTFLAG=1;
         //SUPPRESSPROMPTS(); FIXME
         CH=RDCH();
         WHILE CH=='\n' DO COMMENTFLAG++,CH=RDCH(); //IGNORE BLANK LINES
         IF SKIPCOMMENTS  //option -s
         DO { UNTIL CH==';' || CH==EOF
              DO { IF CH=='\n' DO COMMENTFLAG++;
                   CH=RDCH(); }
              return NIL; }
         IF CH==';' DO return NIL;
         UNTIL CH==';' || CH==EOF
         DO TEST CH=='\n'
            THEN {  C=CONS((LIST)PACKBUFFER(),C);
                    do { COMMENTFLAG++;
                         CH=RDCH(); } while (CH=='\n');
                                    //IGNORE BLANK LINES
                 }
            OR {  BUFCH(CH); CH=RDCH();  }
         TEST CH==EOF
         THEN { WRITEF("%s :- ...",PRINTNAME((ATOM)SUBJECT)),
	        WRITES(" missing \";\"\n");
	        COMMENTFLAG--;
	        SYNTAX(); }
         OR C=CONS((LIST)PACKBUFFER(),C);
         return REVERSE(C); }
   IF CH==CH2
   DO {  IF CH=='+' DO return PLUSPLUS_SY;
         IF CH=='.' DO return DOTDOT_SY;
         IF CH=='-' DO return DASHDASH_SY;
         IF CH=='*' DO return STARSTAR_SY;
         IF CH=='='   DO return EQ_SY; // ADDED DT 2015
         IF CH=='|' DO // COMMENT TO END OF LINE (NEW)
         do{ CH=RDCH();
             IF CH=='\n' DO return EOL;
             IF CH==EOF  DO return ENDSTREAMCH;
         } REPEAT;
      }
   IF CH=='<' && '-'==CH2 DO return BACKARROW_SY;
   IF CH2=='='
   DO {  IF CH=='>'   DO return GE_SY;
         IF CH=='<'   DO return LE_SY;
         IF NOTCH(CH) DO return NE_SY;
      }
   UNRDCH(CH2);
   IF CH=='?'||CH=='!' DO EXPFLAG=TRUE;
   IF CH=='=' && !LEGACY DO EQNFLAG=TRUE;
   return (TOKEN)(NOTCH(CH) ? '\\' : CH);  // GCC WARNING EXPECTED
}  }

WORD
CASECONV(WORD CH)
{
   return tolower(CH);
}

#ifdef DECIMALS
WORD
PEEKDIGIT()
{  WORD CH=RDCH();
   UNRDCH(CH);
   return (isdigit(CH));
}

static WORD
READ_DECIMALS(void)         //RETURNS VALUE IN HUNDREDTHS
{  WORD N=0,F=10,D;
   do {
      D=RDCH()-'0';
      UNLESS (0<=D && D<=9)
      DO {  D=D+'0';
            WHILE D==' ' DO D=RDCH();
            UNLESS D==')' DO SYNTAX();
            UNRDCH(D);
            return N;  }
      N=N+F*D; //NOTE THAT DECIMAL PLACES AFTER THE 2ND WILL HAVE NO
      F=F/10;  //EFFECT ON THE ANSWER
   } REPEAT;
}
#endif

static WORD
PEEKALPHA()
{  WORD CH=RDCH();
   UNRDCH(CH);
   return (('a'<=CH && CH<='z') || ('A'<=CH && CH<='Z'));
}

VOID
WRITETOKEN(TOKEN T)
{  TEST T<(TOKEN)256 && T>(TOKEN)32 THEN WRCH((WORD)T); OR
   switch(  (WORD)T )
   {  case (WORD)'\n':   WRITES("newline"); break; 
      case (WORD)PLUSPLUS_SY: WRITES("++"); break; 
      case (WORD)DASHDASH_SY: WRITES("--"); break; 
      case (WORD)STARSTAR_SY: WRITES("**"); break; 
      case (WORD)GE_SY:       WRITES(">="); break; 
      case (WORD)LE_SY:       WRITES("<="); break; 
      case (WORD)NE_SY:       WRITES("\\="); break; 
      case (WORD)EQ_SY:       WRITES("=="); break;  
      case (WORD)BACKARROW_SY: WRITES("<-"); break; 
      case (WORD)DOTDOT_SY: WRITES(".."); break; 
      default: TEST !(ISCONS(T) && (HD(T)==IDENT || HD(T)==CONST))
	       THEN WRITEF("<UNKNOWN TOKEN<%p>>",T); OR
	       TEST HD(T)==IDENT
	       THEN WRITES(PRINTNAME((ATOM)(
			ISCONS(TL(T)) && HD(TL(T))==(LIST)ALPHA
				 ? TL(TL(T)) : TL(T)))); OR
	       TEST ISNUM(TL(T))
	       THEN WRITEN(GETNUM(TL(T)));
	       OR WRITEF("\"%s\"",PRINTNAME((ATOM)TL(T)));
}  }

BOOL
HAVE(TOKEN T)
{  IF TOKENS==NIL || HD(TOKENS)!=T DO return FALSE;
   TOKENS=TL(TOKENS);
   return TRUE; }

VOID
CHECK(TOKEN T)
{ IF HAVE(T) DO return;
  ERRORFLAG=TRUE;
  IF MISSING==0 DO MISSING=T; }

VOID
SYNTAX()
{  ERRORFLAG=TRUE; }

WORD
HAVEID()
{  UNLESS ISCONS(HD(TOKENS)) && HD(HD(TOKENS))==IDENT
   DO return FALSE;
   THE_ID=(ATOM) TL(HD(TOKENS));
   TOKENS=TL(TOKENS);
   return TRUE; }

WORD
HAVECONST()
{  UNLESS ISCONS(HD(TOKENS)) && HD(HD(TOKENS))==CONST
   DO return FALSE;
   THE_CONST=TL(HD(TOKENS));
   TOKENS=TL(TOKENS);
   return TRUE; }

WORD
HAVENUM()
{  UNLESS ISCONS(HD(TOKENS)) && HD(HD(TOKENS))==CONST &&
          ISNUM(TL(HD(TOKENS))) DO return FALSE;
   THE_NUM=GETNUM(TL(HD(TOKENS)));
   TOKENS=TL(TOKENS);
   return TRUE;  }

VOID
SYNTAX_ERROR(char *message) //syntax error diagnosis (needs refining)
{  IF ISCONS(TOKENS) && HD(TOKENS)!=BADTOKEN //unclosed string quotes
   DO { WRITES("**unexpected `"),WRITETOKEN(HD(TOKENS)),WRCH('\'');
        IF MISSING && MISSING!=EOL && MISSING!=(TOKEN)';' && MISSING!=(TOKEN)'\''
        DO { WRITES(", missing `"),WRITETOKEN(MISSING),WRCH('\'');
             IF MISSING==(TOKEN)'?' DO WRITES(" or `!'"); }
        WRCH('\n'); }
   WRITES(message);
}

//KRC COMPILER

// Note: What is now '{' here was '{ ' in the BCPL.

#include "bcpl.h"
#include "listhdr.h"
#include "comphdr.h"

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------

// Local function declarations
static BOOL ISOP(LIST X);
static BOOL ISINFIX(LIST X);
static BOOL ISRELOP(LIST X);
static WORD DIPRIO(OPERATOR OP);
static OPERATOR MKINFIX(TOKEN T);
static void PRINTZF_EXP(LIST X);
static BOOL ISLISTEXP(LIST E);
static BOOL ISRELATION(LIST X);
static BOOL ISRELATION_BEGINNING(LIST A,LIST X);
static WORD LEFTPREC(OPERATOR OP);
static WORD RIGHTPREC(OPERATOR OP);
static BOOL ROTATE(LIST E);
static BOOL PARMY(LIST X);
static LIST REST(LIST C);
static LIST SUBTRACT(LIST X, LIST Y);
static void EXPR(WORD N);
static BOOL STARTFORMAL(TOKEN T);
static BOOL STARTSIMPLE(TOKEN T);
static void COMBN(void);
static void SIMPLE(void);
static void COMPILENAME(ATOM N);
static WORD QUALIFIER(void);
static void PERFORM_ALPHA_CONVERSIONS();
static BOOL ISGENERATOR(LIST T);
static void ALPHA_CONVERT(LIST VAR, LIST P);
static LIST SKIPCHUNK(LIST P);
static void CONV1(LIST T, LIST VAR, LIST VAR1);
static LIST FORMAL(void);
static LIST INTERNALISE(LIST VAL);
static LIST PATTERN(void);
static void COMPILELHS(LIST LHS, WORD NARGS);
static void COMPILEFORMAL(LIST X, WORD I);
static void PLANT0(INSTRUCTION OP);
static void PLANT1(INSTRUCTION OP, LIST A);
static void PLANT2(INSTRUCTION OP, LIST A, LIST B);
static LIST COLLECTCODE(void);


// Global variables
void (*TRUEWRCH)(WORD C) = bcpl_WRCH;
LIST LASTLHS=NIL;
LIST TRUTH, FALSITY, INFINITY;


// SETUP_INFIXES() - Interesting elements start at [1]
// The indices correspond to the OPERATOR values in comphdr.h
static TOKEN INFIXNAMEVEC[] = {
	(TOKEN)0,
	(TOKEN) ':',
	PLUSPLUS_SY,
	DASHDASH_SY,
	(TOKEN) '|',
	(TOKEN) '&',
	(TOKEN) '>',
	GE_SY,
	NE_SY,
        EQ_SY, //WAS (TOKEN) '=', CHANGED DT MAY 2015
	LE_SY,
	(TOKEN) '<',
	(TOKEN) '+',
	(TOKEN) '-',
	(TOKEN) '*',
	(TOKEN) '/',
	(TOKEN) '%',
	STARSTAR_SY,
	(TOKEN)	'.',
};
static WORD INFIXPRIOVEC[] = { 0, 0,0,0,1,2,3,3,3,3,3,3,4,4,5,5,5,6,6 };

        // BASES FOR GARBAGE COLLECTION
static LIST CODEV = NIL;// store for opcodes and ther params, which
			// may be operators, various CONStructs or the
			// addresses of C functions.
static LIST ENV[100];   // Appears to be a store for formal parameters
static WORD ENVP;

void
INIT_CODEV() {
   ENVP=-1;
   CODEV=NIL;
}


static BOOL ISOP(LIST X) { return X==(LIST)ALPHA || X==(LIST)INDIR ||
                              ((LIST)QUOTE<=X && X<=(LIST)QUOTE_OP);  }

static BOOL ISINFIX(LIST X) { return (LIST)COLON_OP<=X && X<=(LIST)DOT_OP; }

static BOOL ISRELOP(LIST X) { return (LIST)GR_OP<=X && X<=(LIST)LS_OP; }

// Return the priority of an operator from its index in INFIX*
static WORD DIPRIO(OPERATOR OP)
{  return OP==-1 ? -1 : INFIXPRIOVEC[OP];  }

static OPERATOR
MKINFIX(TOKEN T)// TAKES A TOKEN , RETURNS AN OPERATOR
                                // OR -1 IF T NOT THE NAME OF AN INFIX
{  WORD I=1;
   IF T==(TOKEN)'=' DO return EQ_OP; //legacy, accept "=" for "=="
   UNTIL I>DOT_OP || INFIXNAMEVEC[I]==T DO I=I+1;
   IF I>DOT_OP DO return -1;
   return I;   }

void
PRINTEXP(LIST E, WORD N)    // N IS THE PRIORITY LEVEL
{  if ( E==NIL
   ) WRITES("[]"); else
   if ( ISATOM(E)
   ) WRITES(PRINTNAME((ATOM)E)); else
   if ( ISNUM(E)
   ) { WORD X=GETNUM(E);
          if ( X<0 && N>5 
          ) { WRCH('('); WRITEN(X); WRCH(')'); }
          else WRITEN(X); }
   else {  UNLESS ISCONS(E)
         DO {  if ( E==(LIST)NOT_OP ) WRITES("'\\'"); else
               if ( E==(LIST)LENGTH_OP ) WRITES("'#'");
               else WRITEF("<internal value:%p>",E);
               return; }
      {  LIST OP=HD(E);		// Maybe could be OPERATOR
         if ( !ISOP(OP) && N<=7
         ) {  PRINTEXP(OP,7);
                 WRCH(' ');
                 PRINTEXP(TL(E),8);  }  else
         if ( OP==(LIST)QUOTE
         ) { PRINTATOM((ATOM)TL(E),TRUE); } else
         if ( OP==(LIST)INDIR || OP==(LIST)ALPHA
         ) PRINTEXP(TL(E),N); else
         if ( OP==(LIST)DOTDOT_OP || OP==(LIST)COMMADOTDOT_OP
         ) {  WRCH('[');
                 E=TL(E);
                 PRINTEXP(HD(E),0);
                 IF OP==(LIST)COMMADOTDOT_OP
                 DO {  WRCH(',');
                       E=TL(E);
                       PRINTEXP(HD(E),0);  }
                 WRITES("..");
                 UNLESS TL(E)==INFINITY DO PRINTEXP(TL(E),0);
                 WRCH(']');  } else
         if ( OP==(LIST)ZF_OP
         ) {  WRCH('{');
                 PRINTZF_EXP(TL(E));
                 WRCH('}');  } else
         if ( OP==(LIST)NOT_OP && N<=3
         ) {  WRCH('\\');
                 PRINTEXP(TL(E),3); } else
         if ( OP==(LIST)NEG_OP && N<=5
         ) {  WRCH('-');
                 PRINTEXP(TL(E),5);  } else
         if ( OP==(LIST)LENGTH_OP && N<=7
         ) {  WRCH('#');
                 PRINTEXP(TL(E),7);  } else
         if ( OP==(LIST)QUOTE_OP
         ) {  WRCH('\'');
		 if ( TL(E)==(LIST)LENGTH_OP ) WRCH('#'); else
		 if ( TL(E)==(LIST)NOT_OP ) WRCH('\\'); else
		 WRITETOKEN(INFIXNAMEVEC[(WORD)TL(E)]);
		 WRCH('\''); }  else
         if ( ISLISTEXP(E)
         ) {  WRCH('[');
                 UNTIL E==NIL
                 DO {  PRINTEXP(HD(TL(E)),0);
                       UNLESS TL(TL(E))==NIL DO WRCH(',');
                       E=TL(TL(E));  }
                 WRCH(']');  } else
         if ( OP==(LIST)AND_OP && N<=3 && ROTATE(E) && ISRELATION(HD(TL(E)))
	      && ISRELATION_BEGINNING(TL(TL(HD(TL(E)))),TL(TL(E)))
         ) {  //CONTINUED RELATIONS
                 PRINTEXP(HD(TL(HD(TL(E)))),4);
                 WRCH(' ');
                 WRITETOKEN(INFIXNAMEVEC[(WORD)HD(HD(TL(E)))]);
                 WRCH(' ');
                 PRINTEXP(TL(TL(E)),2);  } else
         if ( ISINFIX(OP) && INFIXPRIOVEC[(WORD)OP]>=N
         ) {  PRINTEXP(HD(TL(E)),LEFTPREC((OPERATOR)OP));
                 UNLESS OP==(LIST)COLON_OP DO WRCH(' '); //DOT.OP should be spaced, DT 2015
                 WRITETOKEN(INFIXNAMEVEC[(WORD)OP]);
                 UNLESS OP==(LIST)COLON_OP DO WRCH(' ');
                 PRINTEXP(TL(TL(E)),RIGHTPREC((OPERATOR)OP));  }
          else {  WRCH('(');
                PRINTEXP(E,0);
                WRCH(')');   }
   }  }  }

static void
PRINTZF_EXP(LIST X)
{  LIST Y=X;
   UNTIL TL(Y)==NIL DO Y=TL(Y);
   PRINTEXP(HD(Y),0);  //BODY
// PRINT "SUCH THAT" AS BAR IF A GENERATOR DIRECTLY FOLLOWS
   if ( ISCONS(HD(X)) && HD(HD(X))==(LIST)GENERATOR ) WRCH('|'); else WRCH(';');
   UNTIL TL(X)==NIL
   DO {  LIST QUALIFIER=HD(X);
         if ( ISCONS(QUALIFIER) && HD(QUALIFIER)==(LIST)GENERATOR
         ) {  PRINTEXP(HD(TL(QUALIFIER)),0);
                 WHILE ISCONS(TL(X)) && //DEALS WITH REPEATED GENERATORS
#ifdef INSTRUMENT_KRC_GC
		       ISCONS(HD(TL(X))) &&
#endif
                       HD(HD(TL(X)))==(LIST)GENERATOR &&
                       EQUAL(TL(TL(HD(TL(X)))),TL(TL(QUALIFIER)))
                 DO {  X=TL(X);
                       QUALIFIER=HD(X);
                       WRCH(',');
                       PRINTEXP(HD(TL(QUALIFIER)),0); }
                 WRITES("<-");
                 PRINTEXP(TL(TL(QUALIFIER)),0);  }
         else PRINTEXP(QUALIFIER,0);
         X=TL(X);
         UNLESS TL(X)==NIL DO WRCH(';');  }
}

static BOOL
ISLISTEXP(LIST E)
{  WHILE ISCONS(E) && HD(E)==(LIST)COLON_OP
   DO {  LIST E1=TL(TL(E));
         WHILE ISCONS(E1) && HD(E1)==(LIST)INDIR
         DO E1=TL(E1);
         TL(TL(E))=E1;
         E=E1;  }
   return E==NIL;   }

static BOOL
ISRELATION(LIST X) { return ISCONS(X) && ISRELOP(HD(X)); }

static BOOL
ISRELATION_BEGINNING(LIST A,LIST X)
{   return (ISRELATION(X) && EQUAL(HD(TL(X)),A)) ||
             (ISCONS(X) && HD(X)==(LIST)AND_OP &&
             ISRELATION_BEGINNING(A,HD(TL(X))));   }

static WORD
LEFTPREC(OPERATOR OP)
{    return OP==COLON_OP||OP==APPEND_OP||OP==LISTDIFF_OP||
              OP==AND_OP||OP==OR_OP||OP==EXP_OP||ISRELOP((LIST)OP) ?
             INFIXPRIOVEC[OP] + 1 : INFIXPRIOVEC[OP];  }

        // RELOPS ARE NON-ASSOCIATIVE
        // COLON, APPEND, AND, OR ARE RIGHT-ASSOCIATIVE
        // ALL OTHER INFIXES ARE LEFT-ASSOCIATIVE

static WORD
RIGHTPREC(OPERATOR OP)
{      return OP==COLON_OP || OP==APPEND_OP || OP==LISTDIFF_OP ||
                OP==AND_OP || OP==OR_OP || OP==EXP_OP ?
             INFIXPRIOVEC[OP] : INFIXPRIOVEC[OP] + 1;  }

static BOOL
ROTATE(LIST E)
                    //PUTS NESTED AND'S INTO RIGHTIST FORM TO ENSURE
                    //DETECTION OF CONTINUED RELATIONS
{  WHILE ISCONS(HD(TL(E))) && HD(HD(TL(E)))==(LIST)AND_OP
   DO {  LIST X=TL(HD(TL(E))), C=TL(TL(E));
         LIST A=HD(X), B=TL(X);
         HD(TL(E))=A, TL(TL(E))=CONS((LIST)AND_OP,CONS(B,C)); }
   return TRUE;  }

//DECOMPILER

void
DISPLAY(ATOM ID, BOOL WITHNOS, BOOL DOUBLESPACING)
                // THE VAL FIELD OF EACH USER DEFINED NAME
                // CONTAINS - CONS(CONS(NARGS,COMMENT),<LIST OF EQNS>)
   {  IF VAL(ID)==NIL
      DO {  WRITEF("\"%s\" - not defined\n",PRINTNAME(ID));
            return; }
   {  LIST X = HD(VAL(ID)), EQNS = TL(VAL(ID));
      WORD NARGS = (WORD)(HD(X));
      LIST COMMENT = TL(X);
      WORD N = LENGTH(EQNS), I;
      LASTLHS=NIL;
      UNLESS COMMENT==NIL
      DO {  LIST C=COMMENT;
            WRITEF("    %s :-",PRINTNAME(ID));
            UNTIL C==NIL
            DO {  WRITES(PRINTNAME((ATOM)HD(C)));
                  C = TL(C);
                  UNLESS C==NIL 
                  DO {  NEWLINE();
                        IF DOUBLESPACING DO NEWLINE(); }
               }
            WRITES(";\n");
            IF DOUBLESPACING DO NEWLINE();  }
      IF COMMENT!=NIL && N==1 && HD(TL(HD(EQNS)))==(LIST)CALL_C 
	 DO return;
      for (I=1; I<=N; I++)
         {  if ( WITHNOS && (N>1 || COMMENT!=NIL)
            ) WRITEF("%2" W ") ",I);
            else WRITES("    ");
            REMOVELINENO(HD(EQNS));
            DISPLAYEQN(ID,NARGS,HD(EQNS));
            IF DOUBLESPACING DO NEWLINE();
            EQNS=TL(EQNS);
   }  }  }

static void
SHCH(WORD CH)
{  TRUEWRCH(' '); }

void
DISPLAYEQN(ATOM ID, WORD NARGS, LIST EQN)    //EQUATION DECODER
   {  LIST LHS = HD(EQN), CODE = TL(EQN);
      if ( NARGS==0
      ) {  WRITES(PRINTNAME(ID)); LASTLHS=(LIST)ID;  }
      else {  if ( EQUAL(LHS,LASTLHS)
            ) _WRCH=SHCH;
            else LASTLHS=LHS;
            PRINTEXP(LHS,0);
            _WRCH=TRUEWRCH;  }
      WRITES(" = ");
      if ( HD(CODE)==(LIST)CALL_C ) WRITES("<primitive function>");
      else DISPLAYRHS(LHS,NARGS,CODE);
      NEWLINE();
   }

void
DISPLAYRHS(LIST LHS, WORD NARGS, LIST CODE)
{  LIST V[100];
   WORD I = NARGS, J; BOOL IF_FLAG = FALSE;
   WHILE I>0 //UNPACK FORMAL PARAMETERS INTO V
   DO {  I = I-1;
	 V[I] = TL(LHS);
	 LHS = HD(LHS); }
   I = NARGS-1;
   do
   {  switch(  (WORD)(HD(CODE)) )
      {  case LOAD_C: CODE=TL(CODE);
                      I=I+1;
                      V[I]=HD(CODE);
                      break; 
         case LOADARG_C: CODE=TL(CODE);
                         I=I+1;
                         V[I]=V[(WORD)(HD(CODE))];
                         break; 
         case APPLY_C: I=I-1;
                       V[I]=CONS(V[I],V[I+1]);
                       break; 
         case APPLYINFIX_C: CODE=TL(CODE);
                            I=I-1;
                            V[I]=CONS(HD(CODE),CONS(V[I],V[I+1]));
                            break; 
         case CONTINUE_INFIX_C: CODE=TL(CODE);
                                V[I-1]=CONS(HD(CODE),
                                          CONS(V[I-1],V[I]));
                         //NOTE THAT 2ND ARG IS LEFT IN PLACE ABOVE
                         //NEW EXPRESSION
                                break; 
         case IF_C: IF_FLAG=TRUE;
                    break; 
         case FORMLIST_C: CODE=TL(CODE);
                          I=I+1;
                          V[I]=NIL;
                          for (J=1; J<=(WORD)(HD(CODE)); J++)
                             {  I=I-1;
                                V[I]=CONS((LIST)COLON_OP,CONS(V[I],V[I+1]));
                             }
                          break; 
         case FORMZF_C: CODE=TL(CODE);
                        I=I-(WORD)(HD(CODE));
                        V[I]=CONS(V[I],NIL);
                        for (J=(WORD)(HD(CODE)); J>=1; J=J-1)
                           V[I] = CONS(V[I+J],V[I]);
                        V[I] = CONS((LIST)ZF_OP,V[I]);
                        break; 
         case CONT_GENERATOR_C:
                CODE = TL(CODE);
                for (J=1; J<=(WORD)(HD(CODE)); J++)
                   V[I-J] = CONS((LIST)GENERATOR,CONS(V[I-J],
                                    TL(TL(V[I]))));
                break; 
         case MATCH_C:
         case MATCHARG_C:
                       CODE=TL(CODE);
                       CODE=TL(CODE);
                       break; 
         case MATCHPAIR_C: CODE=TL(CODE);
                        {  LIST X = V[(WORD)HD(CODE)];
                           I=I+2;
                           V[I-1]=HD(TL(X)), V[I]=TL(TL(X));  }
                           break; 
         case STOP_C: PRINTEXP(V[I],0);
                      UNLESS IF_FLAG DO return;
                      WRITES(", ");
                      PRINTEXP(V[I-1],0);
                      return;
         default: WRITES("IMPOSSIBLE INSTRUCTION IN \"DISPLAYRHS\"\n");
      } //END OF SWITCH
      CODE=TL(CODE);
   } while(1);;
}

LIST
PROFILE(LIST EQN) //EXTRACTS THAT PART OF THE CODE WHICH 
                       //DETERMINES WHICH CASES THIS EQUATION APPLIES TO
{  LIST CODE=TL(EQN);
   IF HD(CODE)==(LIST)LINENO_C
   DO CODE=TL(TL(CODE));
{  LIST C=CODE;
   WHILE PARMY(HD(C)) DO C=REST(C);
{  LIST HOLD=C;
   UNTIL HD(C)==(LIST)IF_C||HD(C)==(LIST)STOP_C DO C=REST(C);
   if ( HD(C)==(LIST)IF_C
   ) return SUBTRACT(CODE,C);
   else return SUBTRACT(CODE,HOLD);
}  }  }

static BOOL
PARMY(LIST X)
{  return X==(LIST)MATCH_C||X==(LIST)MATCHARG_C||X==(LIST)MATCHPAIR_C;
}

static LIST
REST(LIST C)   //REMOVES ONE COMPLETE INSTRUCTION FROM C
{  LIST X=HD(C);
   C=TL(C);
   IF X==(LIST)APPLY_C||X==(LIST)IF_C||X==(LIST)STOP_C DO return C;
   C=TL(C);
   UNLESS X==(LIST)MATCH_C||X==(LIST)MATCHARG_C DO return C;
   return TL(C);  }

static LIST
SUBTRACT(LIST X, LIST Y)  //LIST SUBTRACTION
{  LIST Z=NIL;
   UNTIL X==Y
   DO Z = CONS(HD(X),Z), X = TL(X);
   return Z; //NOTE THE RESULT IS REVERSED - FOR OUR PURPOSES THIS
}              //DOES NOT MATTER

void
REMOVELINENO(LIST EQN)
  //CALLED WHENEVER THE DEFINIENDUM IS SUBJECT OF A
  //DISPLAY,REORDER OR (PARTIAL)DELETE COMMAND - HAS THE EFFECT OF
  //RESTORING THE STANDARD LINE NUMBERING
   { IF HD(TL(EQN))==(LIST)LINENO_C
   DO TL(EQN)=TL(TL(TL(EQN)));
}

//COMPILER FOR KRC EXPRESSIONS AND EQUATIONS

LIST
EXP()
{  INIT_CODEV();
   EXPR(0);
   PLANT0(STOP_C);
   return COLLECTCODE();
}

LIST
EQUATION()      //RETURNS A TRIPLE: CONS(SUBJECT,CONS(NARGS,EQN))
{  LIST SUBJECT = 0, LHS = 0;
   WORD NARGS = 0;
   INIT_CODEV();
   if ( HAVEID()
   ) {  SUBJECT=(LIST)THE_ID,LHS=(LIST)THE_ID;
           WHILE STARTFORMAL(HD(TOKENS))
           DO {  LHS=CONS(LHS,FORMAL());
                 NARGS=NARGS+1;  }
        } else
   if ( HD(TOKENS)==(LIST)'=' && LASTLHS!=NIL
   ) {  SUBJECT=LASTLHS,LHS=LASTLHS;
           WHILE ISCONS(SUBJECT)
           DO SUBJECT=HD(SUBJECT),NARGS=NARGS+1;
        }
   else {  SYNTAX(), WRITES("missing LHS\n");
         return NIL;  }
   COMPILELHS(LHS,NARGS);
{  LIST CODE=COLLECTCODE();
   CHECK((TOKEN)'=');
   EXPR(0);
   PLANT0(STOP_C);
{  LIST EXPCODE=COLLECTCODE();
   if ( HAVE((TOKEN)',') //CHANGE FROM EMAS/KRC TO ALLOW GUARDED SIMPLE DEF
   ) {  EXPR(0);
           PLANT0(IF_C);
           CODE=APPEND(CODE,APPEND(COLLECTCODE(),EXPCODE));  }
   else CODE=APPEND(CODE,EXPCODE);
   UNLESS HD(TOKENS)==ENDSTREAMCH DO CHECK(EOL);
   UNLESS ERRORFLAG DO LASTLHS=LHS;
   IF NARGS==0 DO LHS=0;//IN THIS CASE THE LHS FIELD IS USED TO REMEMBER
       //THE VALUE OF THE VARIABLE - 0 MEANS NOT YET SET
   return CONS(SUBJECT,CONS((LIST)NARGS,CONS(LHS,CODE))); // OK
}  }  }

static void
EXPR(WORD N)  //N IS THE PRIORITY LEVEL
   {  if ( N<=3 &&(HAVE((TOKEN)'\\') || HAVE((TOKEN)'~'))
      ) {  PLANT1(LOAD_C,(LIST)NOT_OP);
              EXPR(3);
              PLANT0(APPLY_C);  } else
      if ( N<=5 && HAVE((TOKEN)'+') ) EXPR(5); else
      if ( N<=5 && HAVE((TOKEN)'-')
      ) {  PLANT1(LOAD_C,(LIST)NEG_OP);
              EXPR(5);
              PLANT0(APPLY_C);  } else
      if ( HAVE((TOKEN)'#')
      ) {  PLANT1(LOAD_C,(LIST)LENGTH_OP);
              COMBN();
              PLANT0(APPLY_C);  } else
      if ( STARTSIMPLE(HD(TOKENS))
      ) COMBN();
      else { SYNTAX(); return; }
   {  OPERATOR OP=MKINFIX(HD(TOKENS));
      WHILE DIPRIO(OP)>=N
      DO {  WORD I, AND_COUNT=0; //FOR CONTINUED RELATIONS
            TOKENS=TL(TOKENS);
            EXPR(RIGHTPREC(OP));
            IF ERRORFLAG DO return;;
            WHILE ISRELOP((LIST)OP) && ISRELOP((LIST)MKINFIX(HD(TOKENS)))
            DO {  //CONTINUED RELATIONS
                  AND_COUNT=AND_COUNT+1;
                  PLANT1(CONTINUE_INFIX_C,(LIST)OP);
                  OP=MKINFIX(HD(TOKENS));
                  TOKENS=TL(TOKENS);
                  EXPR(4);
                  IF ERRORFLAG DO return;  }
            PLANT1(APPLYINFIX_C,(LIST)OP);
            for (I=1; I<=AND_COUNT; I++)
	       PLANT1(APPLYINFIX_C,(LIST)AND_OP);
                        //FOR CONTINUED RELATIONS
            OP=MKINFIX(HD(TOKENS));  }
}  }

static void
COMBN()
{ SIMPLE();
  WHILE STARTSIMPLE(HD(TOKENS))
  DO { SIMPLE();
       PLANT0(APPLY_C); }
}

static BOOL
STARTFORMAL(TOKEN T)
{  return ISCONS(T) ? (HD(T)==IDENT || HD(T)==(LIST)CONST) :
   T==(TOKEN)'(' || T==(TOKEN)'[' || T == (TOKEN)'-';  }

static BOOL
STARTSIMPLE(TOKEN T)
{  return ISCONS(T) ? (HD(T)==IDENT || HD(T)==(LIST)CONST) :
   T==(TOKEN)'(' || T==(TOKEN)'[' || T==(TOKEN)'{' || T==(TOKEN)'\'';  }

static void
SIMPLE()
{  if ( HAVEID()
   ) COMPILENAME(THE_ID); else
   if ( HAVECONST()
   ) PLANT1(LOAD_C,(LIST)INTERNALISE(THE_CONST)); else
   if ( HAVE((TOKEN)'(')
   ) {  EXPR(0); CHECK((TOKEN)')');  } else
   if ( HAVE((TOKEN)'[')
   ) if ( HAVE((TOKEN)']')
        ) PLANT1(LOAD_C,NIL);
        else {  WORD N=1;
              EXPR(0);
              IF HAVE((TOKEN)',')
              DO {  EXPR(0);
                    N=N+1;  }
              if ( HAVE(DOTDOT_SY)
              ) {  if ( HD(TOKENS)==(TOKEN)']'
                      ) PLANT1(LOAD_C,INFINITY);
                      else EXPR(0);
                      IF N==2 DO PLANT0(APPLY_C);
                      PLANT1(APPLYINFIX_C,
			 (LIST)(N==1 ? DOTDOT_OP : COMMADOTDOT_OP));  } // OK
              else {  WHILE HAVE((TOKEN)',')
                    DO {  EXPR(0);
                          N=N+1;  }
                    PLANT1(FORMLIST_C,(LIST)N);  } // OK
              CHECK((TOKEN)']');  } else
    if ( HAVE((TOKEN)'{')  // ZF EXPRESSIONS	BUG?
    ) {  WORD N = 0;
            LIST HOLD = TOKENS;
            PERFORM_ALPHA_CONVERSIONS();
            EXPR(0);
            //if ( HD(TOKENS)==BACKARROW_SY  //IMPLICIT ZF BODY
                      //NO LONGER LEGAL
            //) TOKENS=HOLD; else
            CHECK((TOKEN)';');
            do N = N + QUALIFIER(); while(HAVE((TOKEN)';'));
            PLANT1(FORMZF_C,(LIST)N); // OK
            CHECK((TOKEN)'}'); }  else
   if ( HAVE((TOKEN)'\'') //OPERATOR DENOTATION
   ) {  if ( HAVE((TOKEN)'#') ) PLANT1(LOAD_C,(LIST)LENGTH_OP); else
	   if ( HAVE((TOKEN)'\\') || HAVE((TOKEN)'~') ) PLANT1(LOAD_C,(LIST)NOT_OP);
           else {  OPERATOR OP=MKINFIX((TOKEN)(HD(TOKENS)));
                 if ( ISINFIX((LIST)OP) ) TOKENS=TL(TOKENS);
                 else SYNTAX(); //MISSING INFIX OR PREFIX OPERATOR
                 PLANT1(LOAD_C,(LIST)QUOTE_OP);
                 PLANT1(LOAD_C,(LIST)OP);
                 PLANT0(APPLY_C); }
           CHECK((TOKEN)'\'');  }
   else SYNTAX(); //MISSING identifier|constant|(|[|{
}

static void
COMPILENAME(ATOM N)
   {  WORD I=0;
      UNTIL I>ENVP || ENV[I]==(LIST)N
      DO I=I+1;
      if ( I>ENVP
      ) PLANT1(LOAD_C,(LIST)N);
      else PLANT1(LOADARG_C,(LIST)I); //OK
   }

static WORD
QUALIFIER()
{  if ( ISGENERATOR(TL(TOKENS))  //WHAT ABOUT MORE GENERAL FORMALS?
   ) {  WORD N=0;
           do {
              HAVEID();
              PLANT1(LOAD_C,(LIST)THE_ID);
              N = N+1;
           } while(HAVE((TOKEN)','));
           CHECK(BACKARROW_SY);
           EXPR(0);
           PLANT1(APPLYINFIX_C,(LIST)GENERATOR);
           IF N>1 DO PLANT1(CONT_GENERATOR_C,(LIST)(N-1)); // OK
           return N; }
   else {  EXPR(0) ; return 1;  }
}

static void
PERFORM_ALPHA_CONVERSIONS()
  //ALSO RECOGNISES THE "SUCH THAT" BAR AND CONVERTS IT TO ';'
  //TO DISTINGUISH IT FROM "OR"
   {  LIST P=TOKENS;
      UNTIL HD(P)==(TOKEN)'}' || HD(P)==(TOKEN)']' || HD(P)==EOL
      DO {  IF HD(P)==(TOKEN)'[' || HD(P)==(TOKEN)'{'
            DO {  P = SKIPCHUNK(P);
                  continue;;  }
            IF HD(P)==(TOKEN)'|' && ISID(HD(TL(P))) && ISGENERATOR(TL(TL(P)))
            DO HD(P) = (TOKEN)';' ;
            IF ISID(HD(P)) && ISGENERATOR(TL(P))
            DO ALPHA_CONVERT(HD(P),TL(P));
            P=TL(P);  }  }

BOOL
ISID(LIST X) { return ISCONS(X) && HD(X)==IDENT; }

static BOOL
ISGENERATOR(LIST T)
{    return !ISCONS(T) ? FALSE :
     HD(T)==BACKARROW_SY ||
     (HD(T)==(TOKEN)',' && ISID(HD(TL(T))) && ISGENERATOR(TL(TL(T))));
}

static void
ALPHA_CONVERT(LIST VAR, LIST P)
   {  LIST T=TOKENS;
      LIST VAR1=CONS((LIST)ALPHA,TL(VAR));
      LIST EDGE=T;
      UNTIL HD(EDGE)==(TOKEN)';' || HD(EDGE)==BACKARROW_SY || HD(EDGE)==EOL
      DO EDGE=SKIPCHUNK(EDGE);
      UNTIL T==EDGE
      DO {  CONV1(T,VAR,VAR1);
            T=TL(T);  }
      T=P;
      UNTIL HD(T)==(TOKEN)';' || HD(T)==EOL DO T=SKIPCHUNK(T);
      EDGE=T;
      UNTIL HD(EDGE)==(TOKEN)'}' || HD(EDGE)==(TOKEN)']' || HD(EDGE)==EOL
      DO EDGE=SKIPCHUNK(EDGE);
      UNTIL T==EDGE
      DO {  CONV1(T,VAR,VAR1);
            T=TL(T);  }
      TL(VAR)=VAR1;
   }

static LIST
SKIPCHUNK(LIST P)
{  WORD KET = HD(P)==(TOKEN)'{' ? '}' : HD(P)==(TOKEN)'[' ? ']' : -1;
   P=TL(P);
   IF KET==-1 DO return P;
   UNTIL HD(P)==(LIST)KET || HD(P)==EOL // OK
   DO P = SKIPCHUNK(P);
   UNLESS HD(P)==EOL DO P=TL(P);
   return(P);
}

static void
CONV1(LIST T, LIST VAR, LIST VAR1)
{  IF EQUAL(HD(T),VAR) && HD(T)!=VAR DO TL(HD(T))=VAR1;  }

static
LIST FORMAL()
{  if ( HAVEID() ) return (LIST)THE_ID; else
   if ( HAVECONST() ) return INTERNALISE(THE_CONST); else
   if ( HAVE((TOKEN)'(')
   ) {  LIST P=PATTERN();
           CHECK((TOKEN)')');
           return P;  } else
   if ( HAVE((TOKEN)'[')
   ) {  LIST PLIST=NIL,P=NIL;
           IF HAVE((TOKEN)']') DO return NIL;
           do PLIST=CONS(PATTERN(),PLIST);
           while(HAVE((TOKEN)','));  //NOTE THEY ARE IN REVERSE ORDER
           CHECK((TOKEN)']');
           UNTIL PLIST==NIL
           DO {  P=CONS((TOKEN)COLON_OP,CONS(HD(PLIST),P));
                 PLIST=TL(PLIST);  } //NOW THEY ARE IN CORRECT ORDER
           return P;  } else
   if ( HAVE((TOKEN)'-') && HAVENUM()
   ) {  THE_NUM = -THE_NUM;
           return STONUM(THE_NUM);  }
   else {  SYNTAX(); //MISSING identifier|constant|(|[
         return NIL;
}  }

static LIST
INTERNALISE(LIST VAL)
{     return VAL==TL(TRUTH) ? TRUTH :
               VAL==TL(FALSITY) ? FALSITY :
               ISATOM(VAL) ? CONS((LIST)QUOTE,VAL) : VAL;  }

static LIST
PATTERN()
{  LIST P=FORMAL();
   IF HAVE((TOKEN)':')
   DO P=CONS((LIST)COLON_OP,CONS(P,PATTERN()));
   return P;  }

static void
COMPILELHS(LIST LHS, WORD NARGS)
   {  WORD I;
      ENVP=NARGS-1;
      for (I=1; I<=NARGS; I++)
      {  ENV[NARGS-I]=TL(LHS);
         LHS=HD(LHS);  }
      for (I=0; I<=NARGS-1; I++) COMPILEFORMAL(ENV[I],I);
   }

static void
COMPILEFORMAL(LIST X, WORD I)
{  if ( ISATOM(X)  //IDENTIFIER
   ) {  WORD J=0;
           UNTIL J>=I || ENV[J]==X
           DO J=J+1;  // IS THIS A REPEATED NAME?
           if ( J>=I
           ) return;   // NO, NO CODE COMPILED
           else PLANT2(MATCHARG_C,(LIST)I,(LIST)J);  } else
   if ( ISNUM(X) || X==NIL || (ISCONS(X) && HD(X)==(LIST)QUOTE)
   ) PLANT2(MATCH_C,(LIST)I,X); else
   if ( ISCONS(X) && HD(X)==(TOKEN)COLON_OP && ISCONS(TL(X))
   ) {  PLANT1(MATCHPAIR_C,(LIST)I); // OK
           ENVP=ENVP+2;
        {  WORD A=ENVP-1,B=ENVP;
           ENV[A]=HD(TL(X)), ENV[B]=TL(TL(X));
           COMPILEFORMAL(ENV[A],A);
           COMPILEFORMAL(ENV[B],B);
        }  }
   else WRITES("Impossible event in \"COMPILEFORMAL\"\n");
}

// PLANT stores INSTRUCTIONs and their operands in the code vector
// OP is always an instruction code (*_C);
// A and B can be operators (*_OP), INTs, CONSTs, IDs (names) or
// the address of a C function - all are mapped to LIST type.

// APPLY_C IF_C STOP_C
static void
PLANT0(INSTRUCTION OP)
   {  CODEV=CONS((LIST)OP, CODEV); }

// everything else
static void
PLANT1(INSTRUCTION OP, LIST A)
   { CODEV=CONS((LIST)OP, CODEV);
     CODEV=CONS(A, CODEV); }

// MATCH_C MATCHARG_C
static void
PLANT2(INSTRUCTION OP, LIST A, LIST B)
   { CODEV=CONS((LIST)OP, CODEV);
     CODEV=CONS(A, CODEV);
     CODEV=CONS(B, CODEV); }

static LIST
COLLECTCODE()          //FLUSHES THE CODE BUFFER
{  LIST TMP=CODEV;
   CODEV=NIL;
   return REVERSE(TMP);
}

// Mark elements in CODEV and ENV for preservation by the GC.
// This routine should be called by your BASES() function.
void
COMPILER_BASES(void (*F)(LIST *))
{  WORD I;

   F(&CODEV);
   // ENVP indexes the last used element and starts as -1.
   for (I=0; I<=ENVP ; I++) F(&ENV[I]);
}

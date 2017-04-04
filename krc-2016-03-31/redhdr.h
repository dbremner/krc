// Global function declarations
void	SETUP_PRIMFNS_ETC(void);
void	PRINTVAL(LIST E, WORD FORMAT);
LIST	BUILDEXP(LIST CODE);

// GLOBAL FUNCTIONS IN REDUCER
void	INIT_ARGSPACE(void);
void	ESCAPETONEXTCOMMAND(void);
void	INITSTATS(void);
void	OUTSTATS(void);
void	FIXUP_S(void);
char *	SCASECONV(char *S);

// GLOBAL FUNCTIONS IN MAIN
void	CLOSECHANNELS(void);
FILE *	FINDCHANNEL(char *F);
void	ENTERSCRIPT(ATOM A);

// GLOBAL VARIABLES IN REDUCER
extern LIST MEMORIES;
extern WORD LISTBASE;
extern BOOL ABORTED;

//----------------------------------------------------------------------
//The KRC system is Copyright (c) D. A. Turner 1981
//All  rights reserved.  It is distributed as free software under the
//terms in the file "COPYING", which is included in the distribution.
//----------------------------------------------------------------------


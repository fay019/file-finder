&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR hf-new-path AS CHAR NO-UNDO INIT "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS i-filen-lw i-filen-pf l-folder ~
btn-search-new btn-back i-folder i-filter l-filen btn-search f-p f-w f-r ~
btn-test f-text f-csv f-txt i-text f-all l-finded t-info t-filen l-error-1 ~
l-label-1 l-label-2 l-error-2 f-error-1 
&Scoped-Define DISPLAYED-OBJECTS i-filen-lw i-filen-pf l-folder i-folder ~
i-filter l-filen f-p f-w f-r f-text f-csv f-txt i-text f-all l-finded ~
t-info l-error-1 l-label-1 l-label-2 l-error-2 f-error-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-check-filter C-Win 
FUNCTION f-check-filter RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-check-path C-Win 
FUNCTION f-check-path RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-get-ext C-Win 
FUNCTION f-get-ext RETURNS CHARACTER
  (INPUT hf-name AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-back 
     LABEL "Back" 
     SIZE 9 BY .69.

DEFINE BUTTON btn-search 
     LABEL "Search" 
     SIZE 15 BY 1.12.

DEFINE BUTTON btn-search-new 
     LABEL "Search" 
     SIZE 9 BY .69.

DEFINE BUTTON btn-test 
     LABEL "TesT" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE l-filen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE l-folder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE l-finded AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 43 BY 4.85 NO-UNDO.

DEFINE VARIABLE f-error-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE i-filen-lw AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder path" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE i-filen-pf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE i-text AS CHARACTER FORMAT "X(256)":U 
     LABEL "text" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE l-error-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE l-error-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-1 AS CHARACTER FORMAT "X(256)":U INITIAL "List of Folder:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-2 AS CHARACTER FORMAT "X(256)":U INITIAL "List of File:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE t-filen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder path" 
      VIEW-AS TEXT 
     SIZE 54 BY .62 NO-UNDO.

DEFINE VARIABLE t-info AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 47 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE f-text AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Equal", 1,
"MAtch", 2
     SIZE 9 BY 2.15 NO-UNDO.

DEFINE VARIABLE f-all AS LOGICAL INITIAL yes 
     LABEL "all other" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE f-csv AS LOGICAL INITIAL yes 
     LABEL ".csv" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE f-p AS LOGICAL INITIAL yes 
     LABEL ".p" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE f-r AS LOGICAL INITIAL yes 
     LABEL ".r" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE f-txt AS LOGICAL INITIAL yes 
     LABEL ".txt" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE f-w AS LOGICAL INITIAL yes 
     LABEL ".w" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .69 NO-UNDO.

DEFINE VARIABLE i-filter AS LOGICAL INITIAL no 
     LABEL "Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.

DEFINE VARIABLE i-folder AS LOGICAL INITIAL yes 
     LABEL "Folder" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-filen-lw AT ROW 2.62 COL 10 COLON-ALIGNED WIDGET-ID 2
     i-filen-pf AT ROW 2.62 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     l-folder AT ROW 4.19 COL 13.57 NO-LABEL WIDGET-ID 12
     btn-search-new AT ROW 4.23 COL 42 WIDGET-ID 38
     btn-back AT ROW 4.23 COL 52 WIDGET-ID 40
     i-folder AT ROW 4.23 COL 66 WIDGET-ID 32
     i-filter AT ROW 5.31 COL 66 WIDGET-ID 16
     l-filen AT ROW 5.69 COL 13.57 NO-LABEL WIDGET-ID 18
     btn-search AT ROW 7.46 COL 61 WIDGET-ID 10
     f-p AT ROW 8.69 COL 4 WIDGET-ID 20
     f-w AT ROW 9.5 COL 4 WIDGET-ID 22
     f-r AT ROW 10.19 COL 4 WIDGET-ID 24
     btn-test AT ROW 10.69 COL 51 WIDGET-ID 48
     f-text AT ROW 10.81 COL 67 NO-LABEL WIDGET-ID 52
     f-csv AT ROW 11 COL 4 WIDGET-ID 26
     f-txt AT ROW 11.81 COL 4 WIDGET-ID 28
     i-text AT ROW 12.04 COL 49 COLON-ALIGNED WIDGET-ID 50
     f-all AT ROW 12.62 COL 4 WIDGET-ID 30
     l-finded AT ROW 13.92 COL 32 NO-LABEL WIDGET-ID 56
     t-info AT ROW 1.27 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     t-filen AT ROW 2.81 COL 10 COLON-ALIGNED WIDGET-ID 8
     l-error-1 AT ROW 4.35 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     l-label-1 AT ROW 4.38 COL 1 NO-LABEL WIDGET-ID 34
     l-label-2 AT ROW 5.85 COL 1 NO-LABEL WIDGET-ID 36
     l-error-2 AT ROW 5.85 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     f-error-1 AT ROW 13.65 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 1.31
         SIZE 77 BY 18.85 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "File Finder"
         HEIGHT             = 19.35
         WIDTH              = 80
         MAX-HEIGHT         = 39.12
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 39.12
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
ASSIGN 
       btn-back:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btn-search-new:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-all:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-csv:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-error-1:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       f-p:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-r:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-txt:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       f-w:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       l-error-1:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       l-error-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX l-filen IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-filen:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       l-finded:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX l-folder IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-folder:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN l-label-1 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-1:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-2 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN t-filen IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       t-filen:HIDDEN IN FRAME F-Main           = TRUE
       t-filen:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       t-info:HIDDEN IN FRAME F-Main           = TRUE
       t-info:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* File Finder */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* File Finder */
DO:
   MESSAGE "Sind Sie sicher? " SKIP "Wollen Sie das Programm beenden?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      TITLE "Schliessungsbest„tigung" UPDATE hf-wahl AS LOGICAL.
      CASE hf-wahl:
         WHEN TRUE THEN DO:
            /* This event will close the window and terminate the procedure.  */
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY. 
         END.
      END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-back C-Win
ON CHOOSE OF btn-back IN FRAME F-Main /* Back */
DO:
   DEF VAR firstdpplpkt AS INTEGER NO-UNDO.
   DEF VAR lastbackslash AS INTEGER NO-UNDO.
   
   ASSIGN 
      firstdpplpkt = INDEX(t-filen:SCREEN-VALUE, ":")
      lastbackslash = R-INDEX(t-filen:SCREEN-VALUE, "~\")
      l-error-1:HIDDEN = TRUE      
      l-error-2:HIDDEN = TRUE.
      hf-new-path = SUBSTRING(t-filen:SCREEN-VALUE, 1, lastbackslash - 1).   
   IF LENGTH(hf-new-path) < 3 THEN DO:
      ASSIGN
         SELF:HIDDEN = TRUE 
         hf-new-path = hf-new-path + "~\".   
   END.
   IF firstdpplpkt <> 0 THEN 
      ASSIGN 
         i-filen-lw:SCREEN-VALUE = SUBSTRING(hf-new-path, 1, firstdpplpkt - 1).
   ELSE   
      ASSIGN
         i-filen-lw:SCREEN-VALUE = "".
   IF lastbackslash <> 0 THEN
      ASSIGN
         i-filen-pf:SCREEN-VALUE = SUBSTRING(hf-new-path, firstdpplpkt + 1).
   //ELSE
      //ASSIGN
         //i-filen-pf:SCREEN-VALUE = "~\".
   ASSIGN                 
      t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" + i-filen-pf:SCREEN-VALUE.
   ASSIGN 
      hf-new-path = t-filen:SCREEN-VALUE.
      i-filen-lw:HIDDEN = TRUE.
      i-filen-pf:HIDDEN = TRUE.
      t-filen:HIDDEN = FALSE.
   RUN get-filelist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search C-Win
ON CHOOSE OF btn-search IN FRAME F-Main /* Search */
DO:
   IF t-filen:SCREEN-VALUE <> "" THEN DO: 
      ASSIGN
         i-filen-lw:HIDDEN = TRUE.
         i-filen-pf:HIDDEN = TRUE.
         t-filen:HIDDEN = FALSE.
      RUN get-filelist.      
   END.
   ELSE DO:
      ASSIGN
         t-info:HIDDEN = FALSE
         t-info:SCREEN-VALUE = "Select folder first". 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-search-new
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search-new C-Win
ON CHOOSE OF btn-search-new IN FRAME F-Main /* Search */
DO:   
   IF l-folder:SCREEN-VALUE <> "" AND l-folder:SCREEN-VALUE <> ? THEN DO:
      IF i-filen-pf:SCREEN-VALUE = "~\" THEN  
         ASSIGN 
            hf-new-path = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + l-folder:SCREEN-VALUE
            t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + l-folder:SCREEN-VALUE
         i-filen-pf:SCREEN-VALUE = i-filen-pf:SCREEN-VALUE + l-folder:SCREEN-VALUE.
      ELSE
         ASSIGN           
            hf-new-path = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE
            t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE
         i-filen-pf:SCREEN-VALUE = i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE.
         
      ASSIGN 
         i-filen-lw:HIDDEN = TRUE.
         i-filen-pf:HIDDEN = TRUE.
         t-filen:HIDDEN = FALSE.
      //MESSAGE  hf-new-path   VIEW-AS ALERT-BOX.      
      RUN get-filelist.
   END.
   ELSE 
      MESSAGE "No folder" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-test
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-test C-Win
ON CHOOSE OF btn-test IN FRAME F-Main /* TesT */
DO:
   IF i-text:SCREEN-VALUE <> "" THEN
      RUN p-search-text.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-all C-Win
ON VALUE-CHANGED OF f-all IN FRAME F-Main /* all other */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN
         f-error-1:HIDDEN = TRUE.    
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-csv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-csv C-Win
ON VALUE-CHANGED OF f-csv IN FRAME F-Main /* .csv */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN
         f-error-1:HIDDEN = TRUE.    
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-p
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-p C-Win
ON VALUE-CHANGED OF f-p IN FRAME F-Main /* .p */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN
         f-error-1:HIDDEN = TRUE.    
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-r
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-r C-Win
ON VALUE-CHANGED OF f-r IN FRAME F-Main /* .r */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN 
         f-error-1:HIDDEN = TRUE.     
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-txt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-txt C-Win
ON VALUE-CHANGED OF f-txt IN FRAME F-Main /* .txt */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN
         f-error-1:HIDDEN = TRUE. 
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-w
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-w C-Win
ON VALUE-CHANGED OF f-w IN FRAME F-Main /* .w */
DO:
   // RUN p-check-filter.
   DYNAMIC-FUNCTION('f-check-filter':U).
   DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT f-check-path() THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF f-check-filter() THEN DO:
      ASSIGN 
         SELF:SCREEN-VALUE = "yes"
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
   END.
   ELSE DO:
      ASSIGN
         f-error-1:HIDDEN = TRUE.    
      RUN get-filelist.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filen-lw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-lw C-Win
ON MOUSE-SELECT-DBLCLICK OF i-filen-lw IN FRAME F-Main /* Folder path */
DO:
   t-info:HIDDEN = TRUE.
   RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filen-pf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-pf C-Win
ON MOUSE-SELECT-DBLCLICK OF i-filen-pf IN FRAME F-Main
DO:   
   t-info:HIDDEN = TRUE.
   RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filter C-Win
ON VALUE-CHANGED OF i-filter IN FRAME F-Main /* Filter */
DO:
   IF SELF:SCREEN-VALUE = "yes" THEN DO:
      ASSIGN
         f-p:HIDDEN = FALSE
         f-w:HIDDEN = FALSE
         f-r:HIDDEN = FALSE
         f-csv:HIDDEN = FALSE
         f-txt:HIDDEN = FALSE
         f-all:HIDDEN = FALSE.
         RUN get-filelist.
   END.
   ELSE DO:
      ASSIGN
         f-p:HIDDEN = TRUE
         f-p:SCREEN-VALUE = "yes"
         f-w:HIDDEN = TRUE   
         f-w:SCREEN-VALUE = "yes"
         f-r:HIDDEN = TRUE  
         f-r:SCREEN-VALUE = "yes"
         f-csv:HIDDEN = TRUE  
         f-csv:SCREEN-VALUE = "yes"
         f-txt:HIDDEN = TRUE  
         f-txt:SCREEN-VALUE = "yes"
         f-all:HIDDEN = TRUE 
         f-all:SCREEN-VALUE = "yes".
         RUN get-filelist.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-folder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-folder C-Win
ON VALUE-CHANGED OF i-folder IN FRAME F-Main /* Folder */
DO:
   IF SELF:SCREEN-VALUE = "NO" THEN DO:
      ASSIGN
         btn-search-new:HIDDEN = YES
         l-filen:ROW = 4.19.
         l-label-1:SCREEN-VALUE = "List of File:".
      IF l-filen:LIST-ITEMS <> ? THEN
         ASSIGN 
            l-label-2:HIDDEN = YES.
   END.
   ELSE DO:
      ASSIGN
         l-filen:ROW = 5.69. 
      IF l-filen:LIST-ITEMS <> ? THEN
         ASSIGN
            btn-search-new:HIDDEN = YES 
            l-label-1:SCREEN-VALUE = "List of Folder:" 
            l-label-2:HIDDEN = NO.      
   END.
   IF l-filen:LIST-ITEMS <> "" AND l-filen:LIST-ITEMS <> ? THEN
      RUN get-filelist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-filen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-filen C-Win
ON MOUSE-SELECT-CLICK OF t-filen IN FRAME F-Main /* Folder path */
DO:
  SELF:HIDDEN = TRUE.
  i-filen-lw:HIDDEN = FALSE.
  i-filen-pf:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
       
DEFINE VARIABLE hf-w AS INTEGER     NO-UNDO.
   DEFINE VARIABLE hf-h AS INTEGER     NO-UNDO.
   RUN get-screen-size.
   IF hf-w > {&WINDOW-NAME}:WIDTH-PIXELS AND hf-h > {&WINDOW-NAME}:HEIGHT-PIXELS THEN   
      ASSIGN   
         {&WINDOW-NAME}:X = (hf-w / 2) - ({&WINDOW-NAME}:WIDTH-PIXELS / 2 ) 
         {&WINDOW-NAME}:Y = (hf-h / 2) - ({&WINDOW-NAME}:HEIGHT-PIXELS / 2 ).
   ELSE  
      ASSIGN   
         {&WINDOW-NAME}:X = 0 
         {&WINDOW-NAME}:Y = 0.       
       

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
      RUN enable_UI.
      ASSIGN 
         t-filen:HIDDEN = TRUE
         l-filen:HIDDEN = TRUE
         l-folder:HIDDEN = TRUE
         l-label-1:HIDDEN = TRUE
         l-label-2:HIDDEN = TRUE
         f-p:HIDDEN = TRUE
         f-w:HIDDEN = TRUE
         f-r:HIDDEN = TRUE
         f-csv:HIDDEN = TRUE
         f-txt:HIDDEN = TRUE
         f-all:HIDDEN = TRUE
         btn-search-new:HIDDEN = TRUE
         btn-back:HIDDEN = TRUE
         l-error-1:HIDDEN = TRUE
         l-error-2:HIDDEN = TRUE
         f-error-1:HIDDEN = TRUE
         t-info:HIDDEN = TRUE.
     
     
     
      IF NOT THIS-PROCEDURE:PERSISTENT THEN
         WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY i-filen-lw i-filen-pf l-folder i-folder i-filter l-filen f-p f-w f-r 
          f-text f-csv f-txt i-text f-all l-finded t-info l-error-1 l-label-1 
          l-label-2 l-error-2 f-error-1 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE i-filen-lw i-filen-pf l-folder btn-search-new btn-back i-folder 
         i-filter l-filen btn-search f-p f-w f-r btn-test f-text f-csv f-txt 
         i-text f-all l-finded t-info t-filen l-error-1 l-label-1 l-label-2 
         l-error-2 f-error-1 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-dirname C-Win 
PROCEDURE get-dirname :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   DEFINE VARIABLE cFile         AS CHAR  NO-UNDO.
   DEFINE VARIABLE firstdpplpkt  AS INT   NO-UNDO.
   DEFINE VARIABLE lastbackslash AS INT   NO-UNDO.
   DEFINE VARIABLE found-file    AS LOG   NO-UNDO.


   SYSTEM-DIALOG GET-DIR cFile
      TITLE "Select File"
      RETURN-TO-START-DIR
      UPDATE found-file.
   IF found-file THEN DO:
      ASSIGN 
         t-filen:SCREEN-VALUE = cFile
         firstdpplpkt  =   INDEX (cFile, ":")
         lastbackslash = R-INDEX (cFile, "\").
      IF firstdpplpkt <> 0 THEN 
         i-filen-lw:SCREEN-VALUE = SUBSTRING(cFile, 1, firstdpplpkt - 1).
      ELSE   
         i-filen-lw:SCREEN-VALUE = "".
      IF lastbackslash <> 0 THEN DO:
         ASSIGN
            i-filen-pf:SCREEN-VALUE = SUBSTRING(cFile, firstdpplpkt + 1).
      END.
      ELSE
         ASSIGN i-filen-pf:SCREEN-VALUE = "".  
      i-filen-lw:HIDDEN = TRUE.
      i-filen-pf:HIDDEN = TRUE.
      t-filen:HIDDEN = FALSE.
   END.      
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-filelist C-Win 
PROCEDURE get-filelist :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:             
   DEF VAR hf-first-file   AS LOG NO-UNDO INIT NO.
   DEF VAR hf-first-folder AS LOG NO-UNDO INIT NO. 
   DEF VAR cDir            AS CHAR NO-UNDO.
   DEF VAR cFileStream     AS CHAR NO-UNDO.
   DEF VAR hf-type         AS CHAR NO-UNDO.
   DEF VAR hf-path         AS CHAR NO-UNDO.
   DEF VAR hf-ext          AS CHAR NO-UNDO.
   DEF VAR lastpkt         AS INT  NO-UNDO.
   
   l-filen:LIST-ITEMS = "".
   l-folder:LIST-ITEMS = "".
   IF hf-new-path = "" THEN
      ASSIGN 
         cDir = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE.
   ELSE 
      ASSIGN 
         cDir = hf-new-path. 
   //MESSAGE f-check-path(cDir) = YES VIEW-AS ALERT-BOX. 
   IF DYNAMIC-FUNCTION('f-check-path':U) THEN DO:
      INPUT FROM OS-DIR (cDir).
      REPEAT:
         IMPORT cFileStream. // file name 
         
         ASSIGN 
            hf-path = cDir + "~\" + cFileStream. // add "\" between path and file name      
         FILE-INFO:FILE-NAME = hf-path.
         
         ASSIGN 
            hf-type = SUBSTRING(FILE-INFO:FILE-TYPE, 1,1 ). 
            
         IF cFileStream <> "." AND cFileStream <> ".." THEN DO:
            IF hf-type = "F" THEN DO:
               hf-ext = DYNAMIC-FUNCTION('f-get-ext':U,cFileStream).
               IF i-filter:SCREEN-VALUE = "yes" AND f-p:SCREEN-VALUE = "no" AND hf-ext = "p" THEN NEXT.
               IF i-filter:SCREEN-VALUE = "yes" AND f-w:SCREEN-VALUE = "no" AND hf-ext = "w" THEN NEXT.
               IF i-filter:SCREEN-VALUE = "yes" AND f-r:SCREEN-VALUE = "no" AND hf-ext = "r" THEN NEXT.
               IF i-filter:SCREEN-VALUE = "yes" AND f-csv:SCREEN-VALUE = "no" AND hf-ext = "csv" THEN NEXT.
               IF i-filter:SCREEN-VALUE = "yes" AND f-txt:SCREEN-VALUE = "no" AND hf-ext = "txt" THEN NEXT.
               IF i-filter:SCREEN-VALUE = "yes" AND f-all:SCREEN-VALUE = "no" AND 
                  ( hf-ext <> "p" AND hf-ext <> "w" AND hf-ext <> "r" AND hf-ext <> "csv" AND hf-ext <> "txt")  
                  THEN NEXT.
                  
               l-filen:ADD-LAST(cFileStream).
               IF NOT hf-first-file THEN DO:   
                  l-filen:SCREEN-VALUE = cFileStream.
                  ASSIGN 
                     hf-first-file = YES
                     l-filen:HIDDEN = FALSE
                     l-label-1:HIDDEN = FALSE
                     l-error-1:HIDDEN = TRUE
                     l-error-2:HIDDEN = TRUE.
               END.
            END.
            ELSE IF hf-type = "D" AND i-folder:SCREEN-VALUE = "YES" THEN DO:               
               l-folder:ADD-LAST(cFileStream).
               IF NOT hf-first-folder THEN DO:
                  ASSIGN       
                     l-folder:SCREEN-VALUE = cFileStream
                     hf-first-folder = YES 
                     //btn-back:HIDDEN = FALSE
                     btn-search-new:HIDDEN = FALSE
                     l-folder:HIDDEN = FALSE
                     l-label-1:HIDDEN = FALSE
                     l-label-2:HIDDEN = FALSE
                     l-error-1:HIDDEN = TRUE
                     l-error-2:HIDDEN = TRUE.
               END.  
            END.
         END.
      END.
      ASSIGN hf-new-path = "".
      IF (l-folder:SCREEN-VALUE = ? OR l-folder:SCREEN-VALUE = "") AND i-folder:SCREEN-VALUE = "yes" THEN DO:
         ASSIGN 
            l-error-1:SCREEN-VALUE = "Has no folder!"
            l-error-1:HIDDEN = FALSE
            l-folder:HIDDEN = TRUE
            btn-search-new:HIDDEN = TRUE.
      END.
      IF l-filen:SCREEN-VALUE = ? OR l-folder:SCREEN-VALUE = "" THEN DO:
         ASSIGN
            l-filen:HIDDEN = TRUE.
         IF i-folder:SCREEN-VALUE = "yes"  THEN
            ASSIGN 
               l-error-2:SCREEN-VALUE = "Has no File!"
               l-error-2:HIDDEN = FALSE.
         ELSE 
            ASSIGN 
               l-error-1:SCREEN-VALUE = "Has no File!"
               l-error-1:HIDDEN = FALSE
               l-label-2:HIDDEN = YES
               l-filen:HIDDEN= TRUE.     
      END.
      IF LENGTH(t-filen:SCREEN-VALUE) > 3  THEN
         ASSIGN
            btn-back:HIDDEN = FALSE.
      
   END.
   ELSE DO:
      ASSIGN 
         t-info:HIDDEN = FALSE
         t-info:SCREEN-VALUE = "Select folder first".  
   END.

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-filename C-Win 
PROCEDURE get-filename :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Open the file window and get the path and name of the selected file 
            after clicking the "OK" button or by double-clicking the selected file.
            The predefined files are
            Excel *.csv
            Text *.txt
            Program *.p
            Mask *.w  
------------------------------------------------------------------------------*/
/* do with frame {&FRAME-NAME}:                                                                                  */
/*    define variable file-name     as CHARACTER no-undo.                                                        */
/*    define variable firstdpplpkt  as integer   no-undo.                                                        */
/*    define variable lastbackslash as integer   no-undo.                                                        */
/*    define variable lastpoint     as integer   no-undo.                                                        */
/*    define variable found-file    as logical   no-undo.                                                        */
/*                                                                                                               */
/*    system-dialog get-file file-name                                                                           */
/*       filters                                                                                                 */
/*               "alle Dateien (*.*)" "*.*",                                                                     */
/*               "Text (*.txt)" "*.txt",                                                                         */
/*               "Excel (*.csv)" "*.csv",                                                                        */
/*               "Programme (*.p)" "*.p",                                                                        */
/*               "Mask (*.w)" "*.w"                                                                              */
/*       return-to-start-dir                                                                                     */
/*       title "Suche Dateiname"                                                                                 */
/*       update found-file.                                                                                      */
/*                                                                                                               */
/*    if found-file then do:                                                                                     */
/*       // ASSIGN hf-file-path = FILE-NAME.                                                                     */
/*       assign firstdpplpkt  =   index (file-name, ":")                                                         */
/*              lastbackslash = r-index (file-name, "\")                                                         */
/*              lastpoint     = r-index (file-name, ".").                                                        */
/*       if firstdpplpkt <> 0 then                                                                               */
/*          i-filen-lw:screen-value = substring (file-name, 1, firstdpplpkt - 1).                                */
/*       else                                                                                                    */
/*          i-filen-lw:screen-value = "".                                                                        */
/*       if lastbackslash <> 0 THEN DO:                                                                          */
/*          ASSIGN                                                                                               */
/*             i-filen-pf:screen-value = substring (file-name, firstdpplpkt + 1, lastbackslash - firstdpplpkt)   */
/*             i-filen:screen-value = substring (file-name, lastbackslash + 1).                                  */
/*             // w-filen:SCREEN-VALUE = substring (file-name, lastbackslash + 1, lastpoint - lastbackslash - 1) */
/*             // hf-file-ext = SUBSTRING(file-name, lastpoint + 1 ).                                            */
/*       END.                                                                                                    */
/*       else                                                                                                    */
/*          assign i-filen-pf:screen-value = ""                                                                  */
/*                 i-filen:screen-value = file-name.                                                             */
/*    end.                                                                                                       */
/* end.                                                                                                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-screen-size C-Win 
PROCEDURE get-screen-size :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE myScreens AS "System.Windows.Forms.Screen[]" NO-UNDO.
DEFINE VARIABLE myScreen AS System.Windows.Forms.Screen NO-UNDO.
DEFINE VARIABLE oAblParent AS System.IntPtr NO-UNDO.
DEFINE VARIABLE currentScreen AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.
 
/* Get ABL Window HWND into .NET-usable format */
oAblParent = NEW System.IntPtr(ACTIVE-WINDOW:HWND).
 
myScreens = System.Windows.Forms.Screen:AllScreens.

/* Iterate through the array of screens */
DO i = 0 TO myScreens:LENGTH - 1:
 
    /* Get current screen - ABL Window */
    myScreen = System.Windows.Forms.Screen:FromHandle(oAblParent).
 
    /* Get current screen - .NET Form */
    /* myScreen = System.Windows.Forms.Screen:FromControl(THIS-OBJECT). */
 
    ASSIGN currentScreen = myScreen:DeviceName /* Get / store screen where Progress client is displayed; will always be DISPLAY1 when executed in Procedure Editor */
           myScreen = CAST(myScreens:GetValue(i), "System.Windows.Forms.Screen").

    IF currentScreen = myScreen:DeviceName THEN  DO:  
       ASSIGN 
         hf-w = myScreen:Bounds:WIDTH 
         hf-h = myScreen:Bounds:HEIGHT.
    END.
 
    IF VALID-OBJECT(myScreen) THEN
        DELETE OBJECT myScreen.
END.
 
IF VALID-OBJECT(myScreens) THEN
    DELETE OBJECT myScreens.
 
IF VALID-OBJECT(oAblParent) THEN
    DELETE OBJECT oAblParent.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-check-filter C-Win 
PROCEDURE p-check-filter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:  
   DEF VAR hf-count-f AS INT NO-UNDO INIT 0.   
   
   IF f-p:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-w:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-r:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-csv:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-txt:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-all:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.
   IF hf-count-f > 5 THEN DO:
      ASSIGN              
         f-error-1:HIDDEN = FALSE
         f-error-1:SCREEN-VALUE = "minimum 1 filter".
      SELF:SCREEN-VALUE = "yes".
      PAUSE 1 NO-MESSAGE.
      f-error-1:HIDDEN = TRUE.
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-search-text C-Win 
PROCEDURE p-search-text :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do with frame {&FRAME-NAME}:
    DEF VAR hf-start      AS LOG      NO-UNDO INIT NO.
    DEF VAR hf-find       AS LOG      NO-UNDO INIT NO.
    DEF VAR hf-i          AS INT      NO-UNDO.
    DEF VAR hf-l          AS INT      NO-UNDO.
    DEF VAR hf-text       AS LONGCHAR NO-UNDO.
    DEF VAR hf-char       AS CHAR     NO-UNDO.
    DEF VAR hf-word       AS CHAR     NO-UNDO.
    DEF VAR hf-word-l     AS CHAR     NO-UNDO.
    DEF VAR hf-ext        AS CHAR     NO-UNDO.
    DEF VAR hf-file-path  AS CHAR     NO-UNDO.
    ASSIGN
       hf-l = 1
       hf-text = ""
       hf-word = ""
       hf-word-l = ""
       hf-ext = DYNAMIC-FUNCTION('f-get-ext':U,l-filen:SCREEN-VALUE )
       hf-file-path = t-filen:SCREEN-VALUE + "/" + l-filen:SCREEN-VALUE
       l-finded:SCREEN-VALUE = "".

    IF hf-ext <> "p" AND  hf-ext <> "w" AND  hf-ext <> "r" THEN
        COPY-LOB FILE (hf-file-path) TO hf-text CONVERT SOURCE CODEPAGE "utf-8".
    ELSE
        COPY-LOB FILE (hf-file-path) TO hf-text CONVERT SOURCE CODEPAGE "iso8859-1".


   DO hf-i = 1 TO LENGTH(hf-text):
      ASSIGN hf-char = SUBSTRING(hf-text, hf-i, 1). // get char after char
       // Charachter counter
      IF hf-start = NO AND  hf-char <> " "  THEN DO:  // first char of word
         ASSIGN
            hf-word = hf-char
            hf-start = YES.
      END.
      ELSE IF hf-start = YES AND hf-char <> " "  THEN DO:  // next char in word
         ASSIGN
            hf-word = hf-word + hf-char.
      END.
      ELSE IF hf-start = YES AND hf-char <> " "  THEN DO:   // hier I have my word
         ASSIGN
            hf-start = NO.
      END.
      IF hf-char <> "~n" THEN 
         ASSIGN hf-word-l = hf-word-l + hf-char.
      ELSE
         ASSIGN 
            hf-l = hf-l  + 1
            hf-word-l = "".   
      IF hf-char = "~n" OR hf-char = " " THEN DO: // Line counter 
      
         IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "1" AND hf-word = i-text:SCREEN-VALUE THEN DO:  
               ASSIGN 
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "Word: " + hf-word + "~n" 
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + STRING(hf-l) + ": " + hf-word-l + "~n".
         END.
         ELSE IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "2" AND hf-word MATCHES ( "*" + i-text:SCREEN-VALUE + "*") THEN DO: 
               ASSIGN 
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "Word: " + hf-word + "~n" 
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + STRING(hf-l) + ": " + hf-word-l + "~n".
         END.  
         
         ASSIGN hf-word = "". 
      END.
   END.  
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-check-filter C-Win 
FUNCTION f-check-filter RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: check only if all firter is un checked   
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:  
   DEF VAR hf-count-f AS INT NO-UNDO. 
   ASSIGN hf-count-f = 0.
   IF f-p:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-w:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-r:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-csv:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-txt:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1. 
   IF f-all:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  
   IF hf-count-f > 5 THEN DO:  
      RETURN TRUE. 
   END.
   RETURN FALSE. 
END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-check-path C-Win 
FUNCTION f-check-path RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: check only if all firter is un checked   
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
   
   IF t-filen:SCREEN-VALUE <> ? AND t-filen:SCREEN-VALUE <> "" AND t-filen:SCREEN-VALUE <> ":" THEN DO:  
      RETURN TRUE. 
   END.
   ELSE 
      RETURN FALSE.
END.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-get-ext C-Win 
FUNCTION f-get-ext RETURNS CHARACTER
  (INPUT hf-name AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEF VAR lastpkt   AS INT NO-UNDO.
   DEF VAR hf-result AS CHAR NO-UNDO.
   
   ASSIGN 
      lastpkt = R-INDEX (hf-name, ".").
   IF lastpkt <> 0 THEN
      ASSIGN hf-result = SUBSTRING(hf-name, lastpkt + 1).
   ELSE   
      ASSIGN hf-result =  "".
      
   RETURN hf-result.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


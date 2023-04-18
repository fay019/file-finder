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
f-csv f-txt f-all t-info t-filen l-error-1 l-label-1 l-label-2 l-error-2 
&Scoped-Define DISPLAYED-OBJECTS i-filen-lw i-filen-pf l-folder i-folder ~
i-filter l-filen f-p f-w f-r f-csv f-txt f-all t-info l-error-1 l-label-1 ~
l-label-2 l-error-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE l-filen AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 27 BY .92 NO-UNDO.

DEFINE VARIABLE l-folder AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 27 BY .92 NO-UNDO.

DEFINE VARIABLE i-filen-lw AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder path" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE i-filen-pf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

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
     SIZE 47 BY 1 NO-UNDO.

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
     f-csv AT ROW 11 COL 4 WIDGET-ID 26
     f-txt AT ROW 11.81 COL 4 WIDGET-ID 28
     f-all AT ROW 12.62 COL 4 WIDGET-ID 30
     t-info AT ROW 1.27 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     t-filen AT ROW 2.81 COL 10 COLON-ALIGNED WIDGET-ID 8
     l-error-1 AT ROW 4.35 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     l-label-1 AT ROW 4.38 COL 1 NO-LABEL WIDGET-ID 34
     l-label-2 AT ROW 5.85 COL 1 NO-LABEL WIDGET-ID 36
     l-error-2 AT ROW 5.85 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 1.31
         SIZE 77 BY 15.35 WIDGET-ID 100.


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
         HEIGHT             = 16
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
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
      lastbackslash = R-INDEX(t-filen:SCREEN-VALUE, "~\").      
   IF LENGTH(t-filen:SCREEN-VALUE) > 2 THEN DO: // if the path is biger als "c:" 
      ASSIGN 
         hf-new-path = SUBSTRING(t-filen:SCREEN-VALUE, 1, lastbackslash - 1).
      IF firstdpplpkt <> 0 THEN 
         ASSIGN 
            i-filen-lw:SCREEN-VALUE = SUBSTRING(hf-new-path, 1, firstdpplpkt - 1).
      ELSE   
         ASSIGN
            i-filen-lw:SCREEN-VALUE = "".
      IF lastbackslash <> 0 THEN
         ASSIGN
            i-filen-pf:SCREEN-VALUE = SUBSTRING(hf-new-path, firstdpplpkt + 1).
      ELSE
         ASSIGN
            i-filen-pf:SCREEN-VALUE = "~\".
      ASSIGN                 
         t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" + i-filen-pf:SCREEN-VALUE.
   END.
   ELSE DO: 
      ASSIGN
         i-filen-lw:SCREEN-VALUE = SUBSTRING(t-filen:SCREEN-VALUE, 1, firstdpplpkt - 1).
         i-filen-pf:SCREEN-VALUE = "~\".
   END.                       
   RUN get-filelist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search C-Win
ON CHOOSE OF btn-search IN FRAME F-Main /* Search */
DO:
   IF t-filen:SCREEN-VALUE <> "" THEN
      RUN get-filelist.
   ELSE DO:
      t-info:FGCOLOR = 12.
      t-info:SCREEN-VALUE = "Select folder first". 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-search-new
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search-new C-Win
ON CHOOSE OF btn-search-new IN FRAME F-Main /* Search */
DO:
   MESSAGE l-folder:SCREEN-VALUE  VIEW-AS ALERT-BOX.
   
   IF l-folder:SCREEN-VALUE <> "" AND l-folder:SCREEN-VALUE <> ? THEN DO:
      ASSIGN 
         hf-new-path = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE
         t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE
         i-filen-pf:SCREEN-VALUE = i-filen-pf:SCREEN-VALUE + "~\" + l-folder:SCREEN-VALUE.       
      RUN get-filelist.
   END.
   ELSE 
      MESSAGE "No folder" VIEW-AS ALERT-BOX.
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
      f-p:HIDDEN = FALSE.
      f-w:HIDDEN = FALSE.
      f-r:HIDDEN = FALSE.
      f-csv:HIDDEN = FALSE.
      f-txt:HIDDEN = FALSE.
      f-all:HIDDEN = FALSE.
   END.
   ELSE DO:
      f-p:HIDDEN = TRUE.
      f-w:HIDDEN = TRUE.
      f-r:HIDDEN = TRUE.
      f-csv:HIDDEN = TRUE.
      f-txt:HIDDEN = TRUE.
      f-all:HIDDEN = TRUE.
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
         l-error-2:HIDDEN = TRUE.
     
     
     
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
          f-csv f-txt f-all t-info l-error-1 l-label-1 l-label-2 l-error-2 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE i-filen-lw i-filen-pf l-folder btn-search-new btn-back i-folder 
         i-filter l-filen btn-search f-p f-w f-r f-csv f-txt f-all t-info 
         t-filen l-error-1 l-label-1 l-label-2 l-error-2 
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
   
   l-filen:LIST-ITEMS = "".
   l-folder:LIST-ITEMS = "".
   IF hf-new-path = "" THEN
      ASSIGN 
         cDir = i-filen-lw:SCREEN-VALUE + ":" +  i-filen-pf:SCREEN-VALUE.
   ELSE 
      ASSIGN 
         cDir = hf-new-path.
   IF cDir <> ? AND cDir <> "" THEN DO:
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
               l-filen:ADD-LAST(cFileStream).
               IF NOT hf-first-file THEN DO:   
                  l-filen:SCREEN-VALUE = cFileStream.
                  ASSIGN 
                     hf-first-file = YES
                     l-filen:HIDDEN = FALSE
                     l-label-1:HIDDEN = FALSE.
               END.
            END.
            ELSE IF hf-type = "D" AND i-folder:SCREEN-VALUE = "YES" THEN DO:               
               l-folder:ADD-LAST(cFileStream).
               IF NOT hf-first-folder THEN DO:
                  l-folder:SCREEN-VALUE = cFileStream.
                  ASSIGN 
                     hf-first-folder = YES 
                     btn-back:HIDDEN = FALSE
                     btn-search-new:HIDDEN = FALSE
                     l-folder:HIDDEN = FALSE
                     l-label-1:HIDDEN = FALSE
                     l-label-2:HIDDEN = FALSE.
               END.  
            END.
         END.
      END.
      ASSIGN hf-new-path = "".
   END.
   ELSE
      MESSAGE "path not ok" VIEW-AS ALERT-BOX.
   
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


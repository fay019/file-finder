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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS i-filen-lw i-filen-pf l-filen btn-search ~
t-filen 
&Scoped-Define DISPLAYED-OBJECTS i-filen-lw i-filen-pf l-filen 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-search 
     LABEL "Search" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE l-filen AS CHARACTER FORMAT "X(256)":U 
     LABEL "List of File" 
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

DEFINE VARIABLE t-filen AS CHARACTER FORMAT "X(256)":U 
     LABEL "Folder path" 
      VIEW-AS TEXT 
     SIZE 54 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-filen-lw AT ROW 2.62 COL 9.57 COLON-ALIGNED WIDGET-ID 2
     i-filen-pf AT ROW 2.62 COL 13.72 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     l-filen AT ROW 4.23 COL 2.71 WIDGET-ID 12
     btn-search AT ROW 4.23 COL 63 WIDGET-ID 10
     t-filen AT ROW 2.81 COL 9.57 COLON-ALIGNED WIDGET-ID 8
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
/* SETTINGS FOR COMBO-BOX l-filen IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-filen:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN t-filen IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       t-filen:HIDDEN IN FRAME F-Main           = TRUE.

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


&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search C-Win
ON CHOOSE OF btn-search IN FRAME F-Main /* Search */
DO:
  RUN get-filelist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filen-lw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-lw C-Win
ON MOUSE-SELECT-DBLCLICK OF i-filen-lw IN FRAME F-Main /* Folder path */
DO:
  RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filen-pf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-pf C-Win
ON MOUSE-SELECT-DBLCLICK OF i-filen-pf IN FRAME F-Main /* Fill 2 */
DO:
  RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-filen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-filen C-Win
ON MOUSE-SELECT-DBLCLICK OF t-filen IN FRAME F-Main /* Folder path */
DO:
  RUN get-dirname.
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
  
  t-filen:HIDDEN = TRUE.
  l-filen:HIDDEN = TRUE.
  
  
  
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
  DISPLAY i-filen-lw i-filen-pf l-filen 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE i-filen-lw i-filen-pf l-filen btn-search t-filen 
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
   DEFINE VARIABLE cDir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFileStream AS CHARACTER NO-UNDO.
   DEF VAR hf-first AS LOG NO-UNDO INIT NO.
   DEF VAR hf-count AS INT NO-UNDO.

   l-filen:LIST-ITEMS = "".
   ASSIGN 
      hf-count = 0
      cDir = t-filen:SCREEN-VALUE.
   INPUT FROM OS-DIR (cDir).
   REPEAT:
      IMPORT cFileStream.
      FILE-INFO:FILE-NAME = cDir + cFileStream. 
      IF cFileStream <> "." AND cFileStream <> ".." THEN DO: 
         l-filen:ADD-LAST(cFileStream).
         IF NOT hf-first THEN DO:   
            l-filen:SCREEN-VALUE = cFileStream.
            ASSIGN hf-first = YES.
         END.
      END.
   END.
   IF hf-count > 5  AND  hf-count <= 10 THEN
      l-filen:INNER-LINES = hf-count.
   ELSE IF hf-count > 10 THEN
      l-filen:INNER-LINES = 10.
   l-filen:HIDDEN = FALSE.   
   
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


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
&Scoped-Define ENABLED-OBJECTS i-filen-lw i-filen-pf 
&Scoped-Define DISPLAYED-OBJECTS i-filen-lw i-filen-pf 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE i-filen-lw AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE i-filen-pf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     i-filen-lw AT ROW 2.62 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     i-filen-pf AT ROW 2.62 COL 14 COLON-ALIGNED NO-LABEL WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
  DISPLAY i-filen-lw i-filen-pf 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE i-filen-lw i-filen-pf 
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
   if found-file then do:
      assign firstdpplpkt  =   index (cFile, ":")
             lastbackslash = r-index (cFile, "\").
      if firstdpplpkt <> 0 then
         i-filen-lw:screen-value = substring (cFile, 1, firstdpplpkt - 1).
      else
         i-filen-lw:screen-value = "".
      if lastbackslash <> 0 THEN DO:
         ASSIGN
            i-filen-pf:screen-value = substring (cFile, firstdpplpkt + 1, lastbackslash - firstdpplpkt).
      END.
      else
         assign i-filen-pf:screen-value = "".
      end.   


   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-files-list C-Win 
PROCEDURE get-files-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


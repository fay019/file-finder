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

DEF VAR hf-new-path        AS CHAR NO-UNDO INIT "". // Triggers "btn-back, btn-search-new" procedur "get-filelist"
DEF VAR hf-file-path-g     AS CHAR NO-UNDO. // procedure "p-search-text, p-search-file-or-folder"
DEF VAR hf-ext-g           AS CHAR NO-UNDO. // procedure "p-search-text, p-search-file-or-folder"
DEF VAR hf-file-name-g     AS CHAR NO-UNDO. // procedure "p-search-text, p-search-file-or-folder"
DEF VAR hf-file-num        AS INT  NO-UNDO. // procedure "p-search-text, p-search-file-or-folder"
DEF VAR hf-file-num-f      AS INT  NO-UNDO. // procedure "p-search-text"               
DEF VAR hf-fertig          AS LOG  NO-UNDO INIT NO. // procedure "p-search-text, p-search-file-or-folder"
DEF VAR hf-export-choice-g AS INT  NO-UNDO INIT 1. // export variable choice default 1
DEF VAR hf-export-path-g   AS CHAR NO-UNDO INIT "C:~\temp~\tableau-fichier.html". // export path 
DEF VAR hf-kill            AS LOG  NO-UNDO INIT NO. // procedur killer  
DEF VAR hf-ext-array-g     AS CHAR NO-UNDO EXTENT 3 INIT ["html","csv","txt"]. //


DEF TEMP-TABLE tt-file-line
   FIELD id AS INT
   FIELD txt AS CHAR
   INDEX id IS PRIMARY UNIQUE id.
   
DEF TEMP-TABLE tt-gefunden
   FIELD datei-path AS CHAR
   FIELD datei-name AS CHAR
   FIELD wort AS CHAR
   FIELD linie-num AS INT
   FIELD linie AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS progress-bar-1 img-logo btn-start l-folder ~
btn-search-new btn-back i-folder i-filter l-filen f-p f-csv f-file-or-fold ~
f-w f-txt i-text f-r f-all f-text r-export i-export btn-confirm e-filen-lw ~
e-filen-pf e-filen btn-search-text l-finded t-logo t-info t-filen l-error-1 ~
l-label-1 l-label-2 l-error-2 l-label-3 l-label-4 f-error-1 l-label-5 ~
l-label-6 l-label-7 w-filen t-finded 
&Scoped-Define DISPLAYED-OBJECTS l-folder i-folder i-filter l-filen f-p ~
f-csv f-file-or-fold f-w f-txt i-text f-r f-all f-text r-export i-export ~
l-finded t-logo t-info l-error-1 l-label-1 l-label-2 l-error-2 l-label-3 ~
l-label-4 f-error-1 l-label-5 l-label-6 l-label-7 t-finded 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-chang-ext C-Win 
FUNCTION f-chang-ext RETURNS CHARACTER // Definiert den RÅckgabetyp der Funktion (Zeichenkette)

(INPUT hf-name AS CHAR, INPUT hf-ext AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-check-filter C-Win 
FUNCTION f-check-filter RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logisch)

( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-check-path C-Win 
FUNCTION f-check-path RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logisch)
( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-get-ext C-Win 
FUNCTION f-get-ext RETURNS CHARACTER // Definiert den RÅckgabetyp der Funktion (Zeichenkette)

(INPUT hf-name AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-in-array C-Win 
FUNCTION f-in-array RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logischer Wert)

(INPUT hf-word AS CHAR, INPUT lists AS CHAR EXTENT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-progressBar C-Win 
FUNCTION f-progressBar RETURNS LOGICAL
  ( i-loop AS INT, j-loop AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-reset-progressBar C-Win 
FUNCTION f-reset-progressBar RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD logo-hidden C-Win 
FUNCTION logo-hidden RETURNS WIDGET-HANDLE
  ( )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Datei 
       MENU-ITEM m_Select_Ordner LABEL "&Select Ordner" ACCELERATOR "F3"
       MENU-ITEM m_Schliessen   LABEL "&Schlie·en"     ACCELERATOR "F10".

DEFINE SUB-MENU m_Export 
       MENU-ITEM m_HTML         LABEL "&HTML"          ACCELERATOR "F5"
       MENU-ITEM m_CSV          LABEL "&CSV"           ACCELERATOR "F6"
       MENU-ITEM m_TXT          LABEL "TXT"            ACCELERATOR "F7".

DEFINE SUB-MENU m_Hilfe 
       MENU-ITEM m_Readme       LABEL "&Readme"        ACCELERATOR "F1"
       RULE
       MENU-ITEM m_Autor        LABEL "&Autor"         ACCELERATOR "SHIFT-F1".

DEFINE MENU MENU-BAR-W-Win MENUBAR
       SUB-MENU  m_Datei        LABEL "&Datei"        
       SUB-MENU  m_Export       LABEL "&Export"       
       SUB-MENU  m_Hilfe        LABEL "&?"            .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-back 
     LABEL "Back" 
     SIZE 9 BY 1.

DEFINE BUTTON btn-confirm 
     LABEL "Confirm" 
     SIZE 9 BY 1.

DEFINE BUTTON btn-search 
     LABEL "OK" 
     SIZE 7 BY 1.15
     BGCOLOR 15 .

DEFINE BUTTON btn-search-new 
     LABEL "Search" 
     SIZE 9 BY 1.

DEFINE BUTTON btn-search-text 
     LABEL "Suche" 
     SIZE 23 BY 1.

DEFINE BUTTON btn-start 
     LABEL "Start =>" 
     SIZE 10.57 BY 1.

DEFINE VARIABLE i-filen-lw AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 5 BY .92 NO-UNDO.

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
     SIZE 77.86 BY 7 NO-UNDO.

DEFINE VARIABLE e-filen AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE e-filen-lw AS CHARACTER FORMAT "x(1)" 
     VIEW-AS FILL-IN 
     SIZE 3.86 BY 1 NO-UNDO.

DEFINE VARIABLE e-filen-pf AS CHARACTER FORMAT "x(200)" 
     VIEW-AS FILL-IN 
     SIZE 31.86 BY 1 NO-UNDO.

DEFINE VARIABLE f-error-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE i-filen-pf AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY .92 NO-UNDO.

DEFINE VARIABLE i-text AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE l-error-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE l-error-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-0 AS CHARACTER FORMAT "X(256)":U INITIAL "Folder path:" 
      VIEW-AS TEXT 
     SIZE 10.86 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-1 AS CHARACTER FORMAT "X(256)":U INITIAL "List of Folder:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-2 AS CHARACTER FORMAT "X(256)":U INITIAL "List of File:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Wo zu suchen:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Wort:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-6 AS CHARACTER FORMAT "X(256)":U INITIAL "Export:" 
      VIEW-AS TEXT 
     SIZE 12 BY .62 NO-UNDO.

DEFINE VARIABLE l-label-7 AS CHARACTER FORMAT "X(256)":U INITIAL "File Name:" 
      VIEW-AS TEXT 
     SIZE 11 BY .62 NO-UNDO.

DEFINE VARIABLE t-filen AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 53.14 BY .62 NO-UNDO.

DEFINE VARIABLE t-finded AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 28.72 BY .62 NO-UNDO.

DEFINE VARIABLE t-info AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 47 BY 1
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE t-logo AS CHARACTER FORMAT "X(256)":U INITIAL "INHOUSE GmbH" 
      VIEW-AS TEXT 
     SIZE 17 BY 1.08
     FONT 0 NO-UNDO.

DEFINE VARIABLE w-filen AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 56.86 BY .62
     FONT 6 NO-UNDO.

DEFINE IMAGE img-logo
     FILENAME "H:\DPW_TEST\FM_Test\fm\file-finder\logo-inhouse-red.png":U CONVERT-3D-COLORS
     STRETCH-TO-FIT RETAIN-SHAPE
     SIZE 48.72 BY 3.08.

DEFINE VARIABLE f-file-or-fold AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Datei", 1,
"Ordner", 2
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE f-text AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Gleiches", 1,
"Match", 2
     SIZE 22 BY .81 NO-UNDO.

DEFINE VARIABLE i-export AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          ".html", 1,
".csv", 2,
".txt", 3
     SIZE 32.43 BY .81 NO-UNDO.

DEFINE VARIABLE r-export AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nein", no,
"Ja", yes
     SIZE 23 BY .81 NO-UNDO.

DEFINE RECTANGLE progress-bar-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28.86 BY 1.08.

DEFINE RECTANGLE progress-bar-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE .14 BY 1
     BGCOLOR 10 FGCOLOR 10 .

DEFINE VARIABLE f-all AS LOGICAL INITIAL yes 
     LABEL "all other" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .65 NO-UNDO.

DEFINE VARIABLE f-csv AS LOGICAL INITIAL yes 
     LABEL ".csv" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .65 NO-UNDO.

DEFINE VARIABLE f-p AS LOGICAL INITIAL yes 
     LABEL ".p" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .65 NO-UNDO.

DEFINE VARIABLE f-r AS LOGICAL INITIAL yes 
     LABEL ".r" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .65 NO-UNDO.

DEFINE VARIABLE f-txt AS LOGICAL INITIAL yes 
     LABEL ".txt" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .65 NO-UNDO.

DEFINE VARIABLE f-w AS LOGICAL INITIAL yes 
     LABEL ".w" 
     VIEW-AS TOGGLE-BOX
     SIZE 6 BY .65 NO-UNDO.

DEFINE VARIABLE i-filter AS LOGICAL INITIAL no 
     LABEL "Filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .77 NO-UNDO.

DEFINE VARIABLE i-folder AS LOGICAL INITIAL yes 
     LABEL "Ordner" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.14 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btn-search AT ROW 2.54 COL 67.43 WIDGET-ID 10
     i-filen-lw AT ROW 2.62 COL 10.86 COLON-ALIGNED NO-LABEL WIDGET-ID 100
     i-filen-pf AT ROW 2.62 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     btn-start AT ROW 2.62 COL 54.57 WIDGET-ID 106
     l-folder AT ROW 4.19 COL 13.57 NO-LABEL WIDGET-ID 12
     btn-search-new AT ROW 4.19 COL 42 WIDGET-ID 38
     btn-back AT ROW 4.19 COL 52 HELP
          "HALLOO btn bfb" WIDGET-ID 40
     i-folder AT ROW 4.23 COL 66 WIDGET-ID 32
     i-filter AT ROW 5.35 COL 66 WIDGET-ID 16
     l-filen AT ROW 5.65 COL 13.57 NO-LABEL WIDGET-ID 18
     f-p AT ROW 6.38 COL 56 WIDGET-ID 20
     f-csv AT ROW 6.38 COL 63 WIDGET-ID 26
     f-file-or-fold AT ROW 7 COL 15 NO-LABEL WIDGET-ID 58
     f-w AT ROW 7.27 COL 56 WIDGET-ID 22
     f-txt AT ROW 7.27 COL 63 WIDGET-ID 28
     i-text AT ROW 8.08 COL 11.57 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     f-r AT ROW 8.15 COL 56 WIDGET-ID 24
     f-all AT ROW 8.15 COL 63 WIDGET-ID 30
     f-text AT ROW 9.35 COL 15 NO-LABEL WIDGET-ID 52
     r-export AT ROW 10.69 COL 15 NO-LABEL WIDGET-ID 72
     i-export AT ROW 10.69 COL 41.57 NO-LABEL WIDGET-ID 80
     btn-confirm AT ROW 11.88 COL 71 WIDGET-ID 96
     e-filen-lw AT ROW 11.92 COL 10.86 COLON-ALIGNED NO-LABEL WIDGET-ID 86
     e-filen-pf AT ROW 11.92 COL 17.29 NO-LABEL WIDGET-ID 88
     e-filen AT ROW 11.92 COL 48 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     btn-search-text AT ROW 14.19 COL 13.72 WIDGET-ID 48
     l-finded AT ROW 15.69 COL 1.43 NO-LABEL WIDGET-ID 56
     t-logo AT ROW 1.08 COL 50.57 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     t-info AT ROW 1.31 COL 19 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     l-label-0 AT ROW 2.81 COL 1.43 NO-LABEL WIDGET-ID 94
     t-filen AT ROW 2.81 COL 10.86 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     l-error-1 AT ROW 4.35 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     l-label-1 AT ROW 4.38 COL 1 NO-LABEL WIDGET-ID 34
     l-label-2 AT ROW 5.85 COL 1 NO-LABEL WIDGET-ID 36
     l-error-2 AT ROW 5.85 COL 13 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     l-label-3 AT ROW 7 COL 1 NO-LABEL WIDGET-ID 70
     l-label-4 AT ROW 8.31 COL 1 NO-LABEL WIDGET-ID 68
     f-error-1 AT ROW 9 COL 54 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     l-label-5 AT ROW 9.5 COL 1 NO-LABEL WIDGET-ID 76
     l-label-6 AT ROW 10.77 COL 1 NO-LABEL WIDGET-ID 78
     l-label-7 AT ROW 12.08 COL 1 NO-LABEL WIDGET-ID 92
     w-filen AT ROW 12.12 COL 11.14 COLON-ALIGNED NO-LABEL WIDGET-ID 90
     t-finded AT ROW 13.23 COL 41.14 NO-LABEL WIDGET-ID 62
     progress-bar-1 AT ROW 14.19 COL 41.14 WIDGET-ID 64
     progress-bar-2 AT ROW 14.23 COL 41.29 WIDGET-ID 66
     img-logo AT ROW 1.04 COL 3.29 WIDGET-ID 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.43 ROW 1.35
         SIZE 79.43 BY 22.31 WIDGET-ID 100.


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
         HEIGHT             = 22.62
         WIDTH              = 82.43
         MAX-HEIGHT         = 39.15
         MAX-WIDTH          = 274.29
         VIRTUAL-HEIGHT     = 39.15
         VIRTUAL-WIDTH      = 274.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-W-Win:HANDLE.
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

/* SETTINGS FOR BUTTON btn-search IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       btn-search:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btn-search-new:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN e-filen IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       e-filen:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN e-filen-lw IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       e-filen-lw:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN e-filen-pf IN FRAME F-Main
   NO-DISPLAY ALIGN-L                                                   */
ASSIGN 
       e-filen-pf:HIDDEN IN FRAME F-Main           = TRUE.

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
       i-export:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR COMBO-BOX i-filen-lw IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       i-filen-lw:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN i-filen-pf IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       i-filen-pf:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       l-error-1:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       l-error-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX l-filen IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-filen:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       l-finded:AUTO-INDENT IN FRAME F-Main      = TRUE
       l-finded:AUTO-RESIZE IN FRAME F-Main      = TRUE
       l-finded:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR COMBO-BOX l-folder IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-folder:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN l-label-0 IN FRAME F-Main
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       l-label-0:HIDDEN IN FRAME F-Main           = TRUE
       l-label-0:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-1 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-1:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-2 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-3 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-3:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-4 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-4:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-5 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-5:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-6 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-6:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN l-label-7 IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       l-label-7:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE progress-bar-2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       progress-bar-2:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN t-filen IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       t-filen:HIDDEN IN FRAME F-Main           = TRUE
       t-filen:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN t-finded IN FRAME F-Main
   ALIGN-L                                                              */
ASSIGN 
       t-finded:HIDDEN IN FRAME F-Main           = TRUE
       t-finded:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       t-info:HIDDEN IN FRAME F-Main           = TRUE
       t-info:READ-ONLY IN FRAME F-Main        = TRUE.

ASSIGN 
       t-logo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-filen IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       w-filen:HIDDEN IN FRAME F-Main           = TRUE
       w-filen:READ-ONLY IN FRAME F-Main        = TRUE.

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
      TITLE "SchliessungsbestÑtigung" UPDATE hf-wahl AS LOGICAL.
      CASE hf-wahl:
         WHEN TRUE THEN DO:
            /* This event will close the window and terminate the procedure.  */
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY. 
         END.
         OTHERWISE DO:
            RETURN NO-APPLY.
         END.
      END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main C-Win
ON GO OF FRAME F-Main
DO:
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
      lastbackslash = R-INDEX(t-filen:SCREEN-VALUE, "~\")
      l-error-1:HIDDEN = TRUE      
      l-error-2:HIDDEN = TRUE.
      hf-new-path = SUBSTRING(t-filen:SCREEN-VALUE, 1, lastbackslash - 1).   
   IF LENGTH(hf-new-path) < 3 THEN DO:
      ASSIGN
         SELF:HIDDEN = TRUE
         hf-new-path = i-filen-lw:SCREEN-VALUE + ":~\".
   END.
/*    IF firstdpplpkt <> 0 THEN                                                    */
/*       ASSIGN                                                                    */
/*          i-filen-lw:SCREEN-VALUE = SUBSTRING(hf-new-path, 1, firstdpplpkt - 1). */
/*    ELSE                                                                         */
/*       ASSIGN                                                                    */
/*          i-filen-lw:SCREEN-VALUE = "".                                          */
   IF lastbackslash <> 0 THEN
      ASSIGN
         i-filen-pf:SCREEN-VALUE = SUBSTRING(hf-new-path, firstdpplpkt + 1).
   ASSIGN                 
      t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":" + i-filen-pf:SCREEN-VALUE.
   ASSIGN 
      hf-new-path = t-filen:SCREEN-VALUE
      i-filen-lw:HIDDEN = TRUE
      i-filen-pf:HIDDEN = TRUE
      t-filen:HIDDEN    = FALSE  
      btn-search:HIDDEN = TRUE.
   RUN get-filelist.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-confirm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-confirm C-Win
ON CHOOSE OF btn-confirm IN FRAME F-Main /* Confirm */
DO:
   DEF VAR hf-temp-ext  AS CHAR NO-UNDO.
   DEF VAR hf-index     AS INT NO-UNDO. 
   IF e-filen-lw:SCREEN-VALUE <> "" AND  e-filen-pf:SCREEN-VALUE <> "" AND e-filen:SCREEN-VALUE <> "" THEN 
      ASSIGN 
         hf-temp-ext = DYNAMIC-FUNCTION('f-get-ext':U, e-filen:SCREEN-VALUE).
         
   IF DYNAMIC-FUNCTION('f-in-array':U, hf-temp-ext, hf-ext-array-g) THEN DO:
      ASSIGN
         SELF:HIDDEN = TRUE
         w-filen:SCREEN-VALUE =  e-filen-lw:SCREEN-VALUE + ":~\" + e-filen-pf:SCREEN-VALUE + e-filen:SCREEN-VALUE
         e-filen-lw:HIDDEN = TRUE
         e-filen-pf:HIDDEN = TRUE
         e-filen:HIDDEN = TRUE
         w-filen:HIDDEN = FALSE
         btn-search-text:HIDDEN = FALSE
         hf-export-path-g = w-filen:SCREEN-VALUE.     
   END.
   ELSE
      MESSAGE "Falsche Datei-Endung" VIEW-AS ALERT-BOX. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search C-Win
ON CHOOSE OF btn-search IN FRAME F-Main /* OK */
DO:

   IF i-filen-pf:SCREEN-VALUE <> "" AND t-filen:SCREEN-VALUE <> "" THEN DO: 
      ASSIGN
         i-filen-lw:HIDDEN = TRUE
         i-filen-pf:HIDDEN = TRUE
         t-filen:HIDDEN    = FALSE
         btn-search:HIDDEN = TRUE.
      RUN get-filelist.      
   END.
   ELSE IF i-filen-pf:SCREEN-VALUE = "" AND t-filen:SCREEN-VALUE <> "" THEN DO:
      ASSIGN 
         i-filen-pf:SCREEN-VALUE = "~\"
         i-filen-lw:HIDDEN = TRUE
         i-filen-pf:HIDDEN = TRUE
         t-filen:HIDDEN    = FALSE
         btn-search:HIDDEN = TRUE.
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


&Scoped-define SELF-NAME btn-search-text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-search-text C-Win
ON CHOOSE OF btn-search-text IN FRAME F-Main /* Suche */
DO:
   IF i-text:SCREEN-VALUE <> "" THEN DO:
      DYNAMIC-FUNCTION('f-reset-progressBar':U).
      RUN p-search-file-or-folder.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-start
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-start C-Win
ON CHOOSE OF btn-start IN FRAME F-Main /* Start => */
DO:
  DYNAMIC-FUNCTION('logo-hidden':U).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-filen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-filen C-Win
ON MOUSE-SELECT-DBLCLICK OF e-filen IN FRAME F-Main
DO:    
  run get-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-filen-lw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-filen-lw C-Win
ON ANY-KEY OF e-filen-lw IN FRAME F-Main
DO:
  // color reset if was error befor
  i-filen-lw:SIDE-LABEL-HANDLE:FGCOLOR = 1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-filen-lw C-Win
ON MOUSE-SELECT-DBLCLICK OF e-filen-lw IN FRAME F-Main
DO:
  run get-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME e-filen-pf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL e-filen-pf C-Win
ON MOUSE-SELECT-DBLCLICK OF e-filen-pf IN FRAME F-Main
DO:      
  run get-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-all
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-all C-Win
ON VALUE-CHANGED OF f-all IN FRAME F-Main /* all other */
DO:
   // RUN p-check-filter.
   //DYNAMIC-FUNCTION('f-check-filter':U).
   //DYNAMIC-FUNCTION('f-check-path':U).    
   IF NOT DYNAMIC-FUNCTION('f-check-path':U) THEN DO:
      RUN get-filelist.
      ASSIGN SELF:SCREEN-VALUE = "yes".
      LEAVE.
   END.
   IF DYNAMIC-FUNCTION('f-check-filter':U) THEN DO:
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


&Scoped-define SELF-NAME i-export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-export C-Win
ON VALUE-CHANGED OF i-export IN FRAME F-Main
DO:
   ASSIGN 
      hf-export-choice-g = INTEGER(SELF:SCREEN-VALUE).
   IF r-export:SCREEN-VALUE = "yes"  THEN DO:  
      MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = TRUE.
      MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML".
      MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV".
      MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT".
      
      CASE hf-export-choice-g:
         WHEN 1 THEN DO:    
            MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML   Selected".   
         END.
         WHEN 2 THEN DO:
            MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_CSV:LABEL IN MENU m_Export      = "&CSV   Selected".
         END.
         WHEN 3 THEN DO: 
            MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_TXT:LABEL IN MENU m_Export      = "&TXT   Selected".
         END.
      END CASE.
      RUN p-export-block.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME i-filen-pf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-pf C-Win
ON LEAVE OF i-filen-pf IN FRAME F-Main
DO:
   ASSIGN 
      t-filen:SCREEN-VALUE = i-filen-lw:SCREEN-VALUE + ":~\" + i-filen-pf:SCREEN-VALUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-pf C-Win
ON MOUSE-SELECT-DBLCLICK OF i-filen-pf IN FRAME F-Main
DO:
   t-info:HIDDEN = TRUE.
   RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-filen-pf C-Win
ON VALUE-CHANGED OF i-filen-pf IN FRAME F-Main
DO:
   FILE-INFO:FILE-NAME = i-filen-lw:SCREEN-VALUE + ":~\" + SELF:SCREEN-VALUE.
   
   IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:  
       ASSIGN
         btn-search:HIDDEN = TRUE
         t-info:HIDDEN = FALSE
         t-info:SCREEN-VALUE = "Dieses Verzeichnis existiert nicht.".
   END.
   ELSE DO:
      ENABLE btn-search WITH FRAME F-Main.
      ASSIGN 
         t-info:HIDDEN = TRUE
         t-info:SCREEN-VALUE = ""
         btn-search:HIDDEN = FALSE.
   END.
   
   
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
ON VALUE-CHANGED OF i-folder IN FRAME F-Main /* Ordner */
DO:
   IF SELF:SCREEN-VALUE = "NO" THEN DO:
      ASSIGN
         btn-search-new:HIDDEN = NO
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


&Scoped-define SELF-NAME i-text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL i-text C-Win
ON ANY-KEY OF i-text IN FRAME F-Main
DO:
   IF CHR(LASTKEY) = chr(13) AND i-text:SCREEN-VALUE <> "" THEN DO: 
      DYNAMIC-FUNCTION('f-reset-progressBar':U).
      RUN p-search-file-or-folder.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Autor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Autor C-Win
ON CHOOSE OF MENU-ITEM m_Autor /* Autor */
DO:
   MESSAGE "WKO Inhouse GmbH" SKIP
           "Wien-" YEAR(TODAY) CHR(169) SKIP
           "Version: 1.0.0"
         VIEW-AS ALERT-BOX TITLE "File Finder".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_CSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_CSV C-Win
ON CHOOSE OF MENU-ITEM m_CSV /* CSV */
DO:
   ASSIGN 
      hf-export-choice-g    = 2.   
   MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = TRUE.
   MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = FALSE.
   MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = TRUE.
   MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML".
   MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV    Selected".
   MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT".
   DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN 
         i-export:SCREEN-VALUE = STRING(hf-export-choice-g)
         r-export:SCREEN-VALUE = "YES"
         i-export:HIDDEN = FALSE.
   END. 
   RUN p-export-block.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Export C-Win
ON MENU-DROP OF MENU m_Export /* Export */
DO:
  DYNAMIC-FUNCTION('logo-hidden':U).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_HTML C-Win
ON CHOOSE OF MENU-ITEM m_HTML /* HTML */
DO:
   ASSIGN 
      hf-export-choice-g    = 1. 
   MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = FALSE.
   MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = TRUE.
   MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = TRUE.
   MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML   Selected".
   MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV".
   MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT".   
   DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN 
         i-export:SCREEN-VALUE = STRING(hf-export-choice-g)
         r-export:SCREEN-VALUE = "YES"
         i-export:HIDDEN = FALSE.
   END.
   RUN p-export-block.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Readme
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Readme C-Win
ON CHOOSE OF MENU-ITEM m_Readme /* Readme */
DO:
   OS-COMMAND SILENT VALUE("start  https://github.com/fay019/file-finder/blob/master/README.md ").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Schliessen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Schliessen C-Win
ON CHOOSE OF MENU-ITEM m_Schliessen /* Schlie·en */
DO:
   MESSAGE "Sind Sie sicher? " SKIP "Wollen Sie das Programm beenden?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      TITLE "SchliessungsbestÑtigung" UPDATE hf-wahl AS LOGICAL.
      CASE hf-wahl:
         WHEN TRUE THEN DO:
            /* This event will close the window and terminate the procedure.  */
            APPLY "CLOSE":U TO THIS-PROCEDURE.
            RETURN NO-APPLY. 
         END.
         OTHERWISE DO:
            RETURN NO-APPLY.
         END.
      END CASE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Select_Ordner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Select_Ordner C-Win
ON CHOOSE OF MENU-ITEM m_Select_Ordner /* Select Ordner */
DO:   
  DYNAMIC-FUNCTION('logo-hidden':U).  
  RUN get-dirname.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_TXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_TXT C-Win
ON CHOOSE OF MENU-ITEM m_TXT /* TXT */
DO:   
   ASSIGN 
      hf-export-choice-g = 3.   
   MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = TRUE.
   MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = TRUE.
   MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = FALSE.
   MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML".
   MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV".
   MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT    Selected".
   DO WITH FRAME {&FRAME-NAME}: 
      ASSIGN 
         i-export:SCREEN-VALUE = STRING(hf-export-choice-g)
         r-export:SCREEN-VALUE = "YES"
         i-export:HIDDEN = FALSE.
   END.
   RUN p-export-block.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME r-export
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL r-export C-Win
ON VALUE-CHANGED OF r-export IN FRAME F-Main
DO:
   IF SELF:SCREEN-VALUE = "yes"  THEN DO:  
      MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = TRUE.
      MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML".
      MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV".
      MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT".        
      CASE hf-export-choice-g:
         WHEN 1 THEN DO:    
            MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML   Selected". 
         END.
         WHEN 2 THEN DO:
            MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_CSV:LABEL IN MENU m_Export      = "&CSV   Selected".
         END.
         WHEN 3 THEN DO: 
            MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export  = FALSE.
            MENU-ITEM m_TXT:LABEL IN MENU m_Export      = "&TXT   Selected".
         END.
      END CASE.
      RUN p-export-block.
      ASSIGN
         i-export:SCREEN-VALUE = STRING(hf-export-choice-g)
         i-export:HIDDEN = FALSE
         l-label-7:HIDDEN = FALSE
         btn-confirm:HIDDEN = FALSE.
         btn-search-text:HIDDEN = TRUE.
   END.
   ELSE DO:
      MENU-ITEM m_HTML:SENSITIVE IN MENU m_Export  = TRUE.
      MENU-ITEM m_CSV:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_TXT:SENSITIVE IN MENU m_Export   = TRUE.
      MENU-ITEM m_HTML:LABEL IN MENU m_Export      = "&HTML".
      MENU-ITEM m_CSV:LABEL IN MENU m_Export       = "&CSV".
      MENU-ITEM m_TXT:LABEL IN MENU m_Export       = "&TXT". 
      ASSIGN 
         i-export:HIDDEN = TRUE  
         e-filen-lw:HIDDEN = TRUE
         e-filen-pf:HIDDEN = TRUE
         e-filen:HIDDEN = TRUE
         w-filen:HIDDEN = TRUE
         l-label-7:HIDDEN = TRUE
         btn-confirm:HIDDEN = TRUE
         btn-search-text:HIDDEN = FALSE.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME t-filen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL t-filen C-Win
ON MOUSE-SELECT-CLICK OF t-filen IN FRAME F-Main
DO:
   SELF:HIDDEN = TRUE.
   ASSIGN 
      i-filen-lw:HIDDEN = FALSE
      i-filen-pf:HIDDEN = FALSE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-filen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-filen C-Win
ON MOUSE-SELECT-CLICK OF w-filen IN FRAME F-Main
DO:   
   ASSIGN        
      SELF:HIDDEN = TRUE  
      e-filen-lw:HIDDEN = FALSE
      e-filen-pf:HIDDEN = FALSE
      e-filen:HIDDEN = FALSE
      btn-confirm:HIDDEN = FALSE 
      btn-search-text:HIDDEN = TRUE. 
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

ASSIGN   
   {&WINDOW-NAME}:HEIGHT-PIXEL = 94. 
            

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
      RUN get-drivers.
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
         t-info:HIDDEN = TRUE 
         btn-search-text:HIDDEN = TRUE
         f-file-or-fold:HIDDEN = TRUE
         i-text:HIDDEN = TRUE
         f-text:HIDDEN = TRUE
         l-finded:HIDDEN = TRUE
         progress-bar-1:HIDDEN = TRUE
         progress-bar-2:HIDDEN = TRUE
         i-export:HIDDEN = TRUE 
         e-filen-lw:HIDDEN = TRUE
         e-filen-pf:HIDDEN = TRUE
         e-filen:HIDDEN = TRUE
         w-filen:HIDDEN = TRUE
         l-label-7:HIDDEN = TRUE   
         btn-confirm:HIDDEN = TRUE.   
     
/*       IF NOT THIS-PROCEDURE:PERSISTENT THEN */
/*          WAIT-FOR CLOSE OF THIS-PROCEDURE.  */
   
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
  DISPLAY l-folder i-folder i-filter l-filen f-p f-csv f-file-or-fold f-w f-txt 
          i-text f-r f-all f-text r-export i-export l-finded t-logo t-info 
          l-error-1 l-label-1 l-label-2 l-error-2 l-label-3 l-label-4 f-error-1 
          l-label-5 l-label-6 l-label-7 t-finded 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE progress-bar-1 img-logo btn-start l-folder btn-search-new btn-back 
         i-folder i-filter l-filen f-p f-csv f-file-or-fold f-w f-txt i-text 
         f-r f-all f-text r-export i-export btn-confirm e-filen-lw e-filen-pf 
         e-filen btn-search-text l-finded t-logo t-info t-filen l-error-1 
         l-label-1 l-label-2 l-error-2 l-label-3 l-label-4 f-error-1 l-label-5 
         l-label-6 l-label-7 w-filen t-finded 
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
      INITIAL-DIR t-filen:SCREEN-VALUE
      TITLE "Datei auswÑhlen"
      RETURN-TO-START-DIR
      UPDATE found-file.
   IF found-file THEN DO:
      ASSIGN 
         t-filen:SCREEN-VALUE = cFile
         firstdpplpkt  =   INDEX (cFile, ":")
         lastbackslash = R-INDEX (cFile, "\").
      IF firstdpplpkt <> 0 THEN DO:
         i-filen-lw:SCREEN-VALUE = SUBSTRING(cFile, 1, firstdpplpkt - 1).
      END.
      ELSE   
         i-filen-lw:SCREEN-VALUE = "".
      IF lastbackslash <> 0 THEN DO:
         ASSIGN
            i-filen-pf:SCREEN-VALUE = SUBSTRING(cFile, firstdpplpkt + 1).
      END.
      ELSE
         ASSIGN i-filen-pf:SCREEN-VALUE = "".  
      ASSIGN 
         i-filen-lw:HIDDEN = TRUE
         i-filen-pf:HIDDEN = TRUE
         t-filen:HIDDEN    = FALSE
         btn-search:HIDDEN = TRUE.
   END. 
   IF t-filen:SCREEN-VALUE <> ? THEN
      RUN get-filelist.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-drivers C-Win 
PROCEDURE get-drivers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:   
   DEF VAR hf-drivers   AS CHAR  NO-UNDO.
   DEF VAR hf-char    AS CHAR  NO-UNDO.
   DEF VAR hf-i     AS INT   NO-UNDO.
   DEF VAR hf-last      AS INT   NO-UNDO.
   
   ASSIGN
      hf-drivers = OS-DRIVES
      hf-drivers = REPLACE(hf-drivers, ":", "").
   
   DO hf-i = 1 TO NUM-ENTRIES(hf-drivers, ","):
      ASSIGN
         hf-char = ENTRY(hf-i, hf-drivers, ",").
      i-filen-lw:ADD-LAST(hf-char).
      IF hf-i = 1 THEN
         ASSIGN       
            i-filen-lw:SCREEN-VALUE = hf-char. 
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
         cDir = i-filen-lw:SCREEN-VALUE + ":~\" +  i-filen-pf:SCREEN-VALUE.
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
      ASSIGN
         btn-search-text:HIDDEN = FALSE
         f-file-or-fold:HIDDEN = FALSE
         i-text:HIDDEN = FALSE
         f-text:HIDDEN = FALSE
         l-finded:HIDDEN = FALSE.     
      ASSIGN   
         {&WINDOW-NAME}:HEIGHT-PIXELS = 580.
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
do with frame {&FRAME-NAME}:
   define variable file-name     as CHARACTER no-undo.
   define variable firstdpplpkt  as integer   no-undo.
   define variable lastbackslash as integer   no-undo.
   define variable lastpoint     as integer   no-undo.
   define variable found-file    as logical   no-undo.
 
   system-dialog get-file file-name
      filters  
              "alle Dateien (*.*)" "*.*",
              "Text (*.txt)" "*.txt",
              "Excel (*.csv)" "*.csv",                
              "Web (*.html)" "*.html"
      return-to-start-dir
      title "Suche Dateiname"
      update found-file.
 
   if found-file then do:
      ASSIGN firstdpplpkt  =   index (file-name, ":")
             lastbackslash = r-index (file-name, "\")
             lastpoint     = r-index (file-name, ".").
      if firstdpplpkt <> 0 then
         e-filen-lw:screen-value = substring (file-name, 1, firstdpplpkt - 1).
      else
         e-filen-lw:SCREEN-VALUE = "".
      if lastbackslash <> 0 THEN DO:
         ASSIGN 
            e-filen-pf:screen-value = substring (file-name, firstdpplpkt + 1, lastbackslash - firstdpplpkt)
            e-filen:screen-value = substring (file-name, lastbackslash + 1)
            w-filen:SCREEN-VALUE = FILE-NAME
            hf-export-path-g = FILE-NAME.
      END.
      else
         assign e-filen-pf:screen-value = ""
                e-filen:screen-value = file-name.
   end.
   ASSIGN 
      e-filen-lw:HIDDEN = TRUE
      e-filen-pf:HIDDEN = TRUE
      e-filen:HIDDEN = TRUE
      w-filen:HIDDEN = FALSE.
end.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-export-block C-Win 
PROCEDURE p-export-block :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hf-temp-ext AS CHAR NO-UNDO.
DEF VAR lastbackslash AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:      
   //ASSIGN r-export:SCREEN-VALUE = "yes". 
   IF e-filen:SCREEN-VALUE <> "" THEN  DO:
      ASSIGN hf-temp-ext = DYNAMIC-FUNCTION('f-get-ext':U, hf-export-path-g ).
   IF hf-temp-ext <>  hf-ext-array-g[hf-export-choice-g] THEN DO: 
      MESSAGE "Die Zieldatei hat eine andere Dateierweiterung" 
          SKIP "mîchten Sie die Dateierweiterung Ñndern?" 
          SKIP "Zieldateierweiterung      ~"." hf-temp-ext "~"," 
          SKIP "gewÑhlte Dateierweiterung ~"." hf-ext-array-g[hf-export-choice-g] "~"." 
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          TITLE "Exportieren zu " + CAPS(hf-ext-array-g[hf-export-choice-g]) UPDATE lChoice AS LOGICAL.
        CASE lChoice:
          WHEN TRUE THEN /* Yes */ DO:
            ASSIGN
               hf-export-path-g = DYNAMIC-FUNCTION('f-chang-ext':U, hf-export-path-g, hf-ext-array-g[hf-export-choice-g])
               w-filen:SCREEN-VALUE = hf-export-path-g
               lastbackslash = r-index (hf-export-path-g, "\")
               e-filen:screen-value = substring (hf-export-path-g, lastbackslash + 1).
          END.
          WHEN FALSE THEN /* No */ DO:
            ASSIGN   
               e-filen-lw:HIDDEN = TRUE
               e-filen-pf:HIDDEN = TRUE
               e-filen:HIDDEN = TRUE
               w-filen:HIDDEN = FALSE.
          END.
        END CASE.
      ASSIGN   
         e-filen-lw:HIDDEN = TRUE
         e-filen-pf:HIDDEN = TRUE
         e-filen:HIDDEN = TRUE
         w-filen:HIDDEN = FALSE.
         btn-confirm:HIDDEN = TRUE.
         btn-search-text:HIDDEN = FALSE.
   END.
   END.  
   ELSE DO:
      ASSIGN 
         hf-export-path-g = DYNAMIC-FUNCTION('f-chang-ext':U, hf-export-path-g, hf-ext-array-g[hf-export-choice-g]) 
         e-filen-lw:HIDDEN = FALSE  
         e-filen-lw:SCREEN-VALUE = "c"
         e-filen-pf:HIDDEN = FALSE   
         e-filen-pf:SCREEN-VALUE = "temp~\"
         e-filen:HIDDEN = FALSE 
         w-filen:SCREEN-VALUE = hf-export-path-g
         lastbackslash = r-index (hf-export-path-g, "\")
         e-filen:screen-value = substring (hf-export-path-g, lastbackslash + 1).
         w-filen:HIDDEN = TRUE.      
   END.
END.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-file-to-table C-Win 
PROCEDURE p-file-to-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
   DEF VAR hf-i         AS INT      NO-UNDO.
   DEF VAR hf-line      AS INT      NO-UNDO.
   DEF VAR hf-temp-txt  AS CHAR     NO-UNDO.
   DEF VAR hf-text      AS LONGCHAR NO-UNDO.
   DEF VAR hf-char      AS CHAR     NO-UNDO.
   
   
   
   IF hf-ext-g <> "p" AND  hf-ext-g <> "w" AND  hf-ext-g <> "r" THEN
     COPY-LOB FILE (hf-file-path-g) TO hf-text CONVERT SOURCE CODEPAGE "utf-8" NO-ERROR.
   ELSE
     COPY-LOB FILE (hf-file-path-g) TO hf-text NO-ERROR.
   
   EMPTY TEMP-TABLE tt-file-line NO-ERROR. // reset the table if we have 
   ASSIGN 
      hf-line = 0
      hf-temp-txt = "".
   DO hf-i = 1 TO LENGTH(hf-text):
      ASSIGN hf-char = SUBSTRING(hf-text, hf-i, 1). // get char after char
      IF hf-char <> CHR(13) AND hf-char <> CHR(10) THEN
         ASSIGN hf-temp-txt = hf-temp-txt + hf-char.
      ELSE DO:       
        // MESSAGE "text :" hf-temp-txt VIEW-AS ALERT-BOX. 
         CREATE tt-file-line.
         ASSIGN   
            hf-i = hf-i + 1
            hf-line = hf-line + 1
            tt-file-line.id = hf-line.
         IF LENGTH(hf-temp-txt) > 200  THEN   
            ASSIGN tt-file-line.txt = SUBSTRING(hf-temp-txt, 1, 200) + "...".
         ELSE IF LENGTH(hf-temp-txt) <= 200 AND LENGTH(hf-temp-txt) > 1 THEN
            ASSIGN tt-file-line.txt = hf-temp-txt.    
         ELSE
            ASSIGN tt-file-line.txt = " ".               
         ASSIGN hf-temp-txt = "".  
      END.
   END.
   IF hf-temp-txt <> "" THEN DO:
      CREATE tt-file-line.
         ASSIGN   
            hf-i = hf-i + 1
            hf-line = hf-line + 1
            tt-file-line.id = hf-line
            tt-file-line.txt = hf-temp-txt.
   END.
/*    MESSAGE "Nombre d'enregistrements dans le temp-table : " hf-line SKIP */
/*          VIEW-AS ALERT-BOX.                                              */
/*    FIND LAST tt-file-line NO-ERROR.                                      */
/*    MESSAGE  "txt: " tt-file-line.txt VIEW-AS ALERT-BOX.                  */
   


END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-search-file-or-folder C-Win 
PROCEDURE p-search-file-or-folder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:
   DEF VAR hf-file-path  AS CHAR NO-UNDO. 
   DEF VAR iNumEntries   AS INT  NO-UNDO.      
   DEF VAR iLoop         AS INT  NO-UNDO.
   ASSIGN
      hf-file-num = 0
      hf-file-num-f = 0
      l-finded:SCREEN-VALUE = ""
      progress-bar-1:HIDDEN = FALSE.
      
   IF f-file-or-fold:SCREEN-VALUE = "1"  THEN DO:  // 1 => File 2 => folder    
      ASSIGN
         hf-ext-g = DYNAMIC-FUNCTION('f-get-ext':U,l-filen:SCREEN-VALUE )
         hf-file-path-g = t-filen:SCREEN-VALUE + "~\" + l-filen:SCREEN-VALUE
         hf-file-name-g = l-filen:SCREEN-VALUE.         
      IF DYNAMIC-FUNCTION('f-progressBar':U, 1, 1) THEN
         RUN p-search-text.   
   END.
   ELSE DO:
      SESSION:SET-WAIT-STATE("no":U).
      ASSIGN
         hf-file-path = l-filen:LIST-ITEMS
         iNumEntries = NUM-ENTRIES(hf-file-path,",")
         hf-file-num = iNumEntries.
         
      DO iLoop = 1 TO iNumEntries:
         ASSIGN
            hf-file-name-g = ENTRY(iLoop,hf-file-path,",")
            hf-file-path-g = t-filen:SCREEN-VALUE + "~\" + ENTRY(iLoop,hf-file-path,",")
            hf-ext-g = DYNAMIC-FUNCTION('f-get-ext':U,hf-file-path-g ).                  
         IF DYNAMIC-FUNCTION('f-progressBar':U, iLoop, iNumEntries) THEN
            RUN p-search-text .  
      END.
   END.    
   IF r-export:SCREEN-VALUE = "YES" THEN
      CASE hf-export-choice-g:
         WHEN 1 THEN
            RUN p-to-html.
         WHEN 2 THEN  
            RUN p-to-csv.
         WHEN 3 THEN  
            RUN p-to-txt.
      END CASE.
   EMPTY TEMP-TABLE tt-gefunden NO-ERROR. // reset the table if we have 
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
DO WITH FRAME {&FRAME-NAME}:
   
   DEF VAR hf-start           AS LOG      NO-UNDO INIT NO.
   DEF VAR hf-file-start      AS LOG      NO-UNDO INIT YES.
   DEF VAR hf-i               AS INT      NO-UNDO.
   DEF VAR hf-line            AS INT      NO-UNDO.
   DEF VAR hf-text            AS LONGCHAR NO-UNDO.
   DEF VAR hf-char            AS CHAR     NO-UNDO.
   DEF VAR hf-word            AS CHAR     NO-UNDO.
   DEF VAR hf-matches-helper  AS CHAR     NO-UNDO.
   ASSIGN
      hf-line = 1
      hf-text = ""
      hf-word = "".    
      
   RUN p-file-to-table.   
   IF hf-ext-g <> "p" AND  hf-ext-g <> "w" AND  hf-ext-g <> "r" THEN
     COPY-LOB FILE (hf-file-path-g) TO hf-text CONVERT SOURCE CODEPAGE "utf-8" NO-ERROR.
   ELSE
     COPY-LOB FILE (hf-file-path-g) TO hf-text CONVERT SOURCE CODEPAGE "iso8859-1" NO-ERROR.
   DO hf-i = 1 TO LENGTH(hf-text):
      ASSIGN 
         hf-char = SUBSTRING(hf-text, hf-i, 1). // get char after char
       // Charachter counter
/*       MESSAGE  ///////////////////////////////////////////////////////////////////// */
/*          "hf-char: " hf-char " -- " ASC(hf-char) SKIP                                */
/*          "hf-start: " hf-start SKIP                                                  */
/*          "hf-word: " hf-word SKIP                                                    */
/*          "hf-line: " hf-line SKIP                                                    */
/*          VIEW-AS ALERT-BOX TITLE "1".                                                */
      IF hf-start = NO AND ( hf-char <> " " OR  hf-char = "~n" OR  hf-char = "~r" ) THEN DO:  // first char of word
         ASSIGN
            hf-word = hf-char
            hf-start = YES.
      END.
      ELSE IF hf-start = YES AND ( hf-char <> " " OR  hf-char = "~n" OR  hf-char = "~r" ) THEN DO:  // next char in word
         ASSIGN
            hf-word = hf-word + hf-char.
      END.
      ELSE IF hf-start = YES AND ( hf-char = " " OR  hf-char = "~n" OR  hf-char = "~r" )  THEN DO:   // hier I have my word
         ASSIGN
            hf-start = NO.
      END.

      IF SUBSTRING(i-text:SCREEN-VALUE, 1, 1) = "*" OR  SUBSTRING(i-text:SCREEN-VALUE, 1, 1) = "." THEN 
         ASSIGN hf-matches-helper = "*~~" + i-text:SCREEN-VALUE + "*". // Take into consideration "*" and ".
      ELSE   
         ASSIGN hf-matches-helper = "*" + i-text:SCREEN-VALUE + "*". // No matter what the word begins or ends with
         
      IF hf-char = "~n" OR hf-char = " " THEN DO: // Line counter      
         IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "1" AND hf-word = i-text:SCREEN-VALUE THEN DO: //f-text -> 1 is only selected File 
            IF hf-file-start THEN DO: 
               ASSIGN
                  hf-file-start = NO 
                  hf-file-num-f = hf-file-num-f + 1
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "File Name: " + hf-file-name-g + "~n"
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "************************************~n".
            END.
            ASSIGN
               l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "Word: " + hf-word + "  N¯: " + STRING(hf-file-num-f) + "~n". 
               FOR EACH tt-file-line WHERE tt-file-line.id = hf-line NO-LOCK:
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + STRING(tt-file-line.id) + "=> " + tt-file-line.txt + "~n".   
               END. 
               l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "-----------------------------------------~n".
            ASSIGN hf-word = "". 
               
         END.
         ELSE IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "2" AND hf-word MATCHES ( hf-matches-helper ) THEN DO:  //f-text -> 2 is all Files selected in order || * ->  indicates that any group of characters is acceptable
            IF hf-file-start THEN DO: 
               ASSIGN 
                  hf-file-start = NO
                  hf-file-num-f = hf-file-num-f + 1.
               l-finded:INSERT-STRING("File Name: " + hf-file-name-g + "  N¯: " + STRING(hf-file-num-f) + "~n").
               l-finded:INSERT-STRING( "************************************~n").
            END.
            l-finded:INSERT-STRING( "Word: " + hf-word + "~n"). 
            FIND FIRST tt-file-line WHERE tt-file-line.id = hf-line NO-LOCK NO-ERROR.
            IF AVAILABLE tt-file-line THEN DO:
               CREATE tt-gefunden.
               ASSIGN
                  tt-gefunden.datei-path = hf-file-path-g    
                  tt-gefunden.datei-name = hf-file-name-g
                  hf-word = TRIM(hf-word) // remove blank and Line break
                  tt-gefunden.wort  = TRIM(hf-word, "<>,.;:!? ~"~ '[]()") // rempve all this char at first and last position after vwe remov blank or line breaker
                  tt-gefunden.linie-num  = tt-file-line.id    
                  tt-gefunden.linie = tt-file-line.txt. 
               l-finded:INSERT-STRING( STRING(tt-file-line.id) + "=> " + tt-file-line.txt + "~n" ).    
            END.
            l-finded:INSERT-STRING( "-----------------------------------------~n").
         END.           
         ASSIGN hf-word = "". 
      END.       
      IF hf-char = "~n" THEN 
         ASSIGN 
            hf-line = hf-line  + 1
            hf-start = NO.
   END.
   // For the last Line if it is no empty.
   IF hf-word <> "" THEN DO:   
      IF SUBSTRING(i-text:SCREEN-VALUE, 1, 1) = "*" OR  SUBSTRING(i-text:SCREEN-VALUE, 1, 1) = "." THEN 
         ASSIGN hf-matches-helper = "*~~" + i-text:SCREEN-VALUE + "*". // Take into consideration "*" and ".
      ELSE   
         ASSIGN hf-matches-helper = "*" + i-text:SCREEN-VALUE + "*". // No matter what the word begins or ends with
         
      //IF hf-char = "~n" OR hf-char = " " THEN DO: // Line counter      
         IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "1" AND hf-word = i-text:SCREEN-VALUE THEN DO: //f-text -> 1 is only selected File 
            IF hf-file-start THEN DO: 
               ASSIGN
                  hf-file-start = NO 
                  hf-file-num-f = hf-file-num-f + 1
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "File Name: " + hf-file-name-g + "~n"
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "************************************~n".
            END.
            ASSIGN
               l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "Word: " + hf-word + "  N¯: " + STRING(hf-file-num-f) + "~n". 
               FOR EACH tt-file-line WHERE tt-file-line.id = hf-line NO-LOCK:
                  l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + STRING(tt-file-line.id) + "=> " + tt-file-line.txt + "~n".   
               END. 
               l-finded:SCREEN-VALUE = l-finded:SCREEN-VALUE + "-----------------------------------------~n".
            ASSIGN hf-word = "". 
               
         END.
         ELSE IF i-text:SCREEN-VALUE <> "" AND f-text:SCREEN-VALUE = "2" AND hf-word MATCHES ( hf-matches-helper ) THEN DO:  //f-text -> 2 is all Files selected in order || * ->  indicates that any group of characters is acceptable
            IF hf-file-start THEN DO: 
               ASSIGN 
                  hf-file-start = NO
                  hf-file-num-f = hf-file-num-f + 1.
               l-finded:INSERT-STRING("File Name: " + hf-file-name-g + "  N¯: " + STRING(hf-file-num-f) + "~n").
               l-finded:INSERT-STRING( "************************************~n").
            END.
            l-finded:INSERT-STRING( "Word: " + hf-word + "~n"). 
            FIND FIRST tt-file-line WHERE tt-file-line.id = hf-line NO-LOCK NO-ERROR.
            IF AVAILABLE tt-file-line THEN DO:
               CREATE tt-gefunden.
               ASSIGN
                  tt-gefunden.datei-path = hf-file-path-g    
                  tt-gefunden.datei-name = hf-file-name-g
                  hf-word = TRIM(hf-word) // remove blank and Line break
                  tt-gefunden.wort  = TRIM(hf-word, "<>,.;:!? ~"~ '[]()") // rempve all this char at first and last position after vwe remov blank or line breaker
                  tt-gefunden.linie-num  = tt-file-line.id    
                  tt-gefunden.linie = tt-file-line.txt. 
               l-finded:INSERT-STRING( STRING(tt-file-line.id) + "=> " + tt-file-line.txt + "~n" ).    
            END.
            l-finded:INSERT-STRING( "-----------------------------------------~n").
         END.
      //END.
         
/*       MESSAGE "path: " hf-file-path-g SKIP                  */
/*               "name: " hf-file-name-g SKIP                  */
/*               "word: " hf-word SKIP                         */
/*               "line: " hf-line  " -- " tt-file-line.id SKIP */
/*               "text: " tt-file-line.txt SKIP                */
/*          VIEW-AS ALERT-BOX.                                 */
      
   END.
   ASSIGN hf-word = "". 
END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-to-csv C-Win 
PROCEDURE p-to-csv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF VAR hf-num AS INT NO-UNDO INIT 0.
   DEF VAR hf-old AS CHAR NO-UNDO INIT "".
   
   OUTPUT TO VALUE(hf-export-path-g) NO-CONVERT.
   PUT UNFORMATTED "N¯;Path;Name;Wort;L N¯;Linie" SKIP.
   
   FOR EACH tt-gefunden :
      IF tt-gefunden.datei-path <> hf-old THEN DO:
         ASSIGN hf-num = hf-num + 1.
         EXPORT DELIMITER ";" hf-num tt-gefunden.datei-path tt-gefunden.datei-name tt-gefunden.wort tt-gefunden.linie-num tt-gefunden.linie.
      END.
      ELSE 
         EXPORT DELIMITER ";" "" "" "" tt-gefunden.wort tt-gefunden.linie-num tt-gefunden.linie.
      ASSIGN hf-old = tt-gefunden.datei-path.  
   END.
   
   OUTPUT CLOSE.
   MESSAGE "Der Job ist fertig" SKIP 
         "und in einer csv-Datei gespeichert." SKIP
         "Wollen Sie die Datei îffnen?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
       TITLE "Fertig -> .csv" UPDATE lChoice AS LOGICAL.
     CASE lChoice:
       WHEN TRUE THEN /* Yes */ DO:            
          OS-COMMAND SILENT VALUE("start " + hf-export-path-g).
       END.
     END CASE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-to-html C-Win 
PROCEDURE p-to-html :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   DEF VAR hf-i AS INT NO-UNDO.
   DEF VAR hf-temp AS CHAR NO-UNDO.
   DEF VAR p-head AS CHAR NO-UNDO.
   DEF VAR p-script AS CHAR NO-UNDO.
   DEF VAR p-search AS CHAR NO-UNDO.
   DEF VAR hf-old AS CHAR NO-UNDO.
   DEF VAR hf-linie AS CHAR NO-UNDO.
   DEF VAR hf-wort AS CHAR NO-UNDO.
   
   ASSIGN
      hf-linie = "".
   
/*    IF hf-ext-g <> "p" AND  hf-ext-g <> "w" AND  hf-ext-g <> "r" THEN                     */
/*      COPY-LOB FILE (hf-file-path-g) TO hf-text CONVERT SOURCE CODEPAGE "utf-8" NO-ERROR. */
/*    ELSE                                                                                  */
   
   OUTPUT TO VALUE(hf-export-path-g) CONVERT SOURCE "ISO8859-1" TARGET "UTF-8". 
   
   ASSIGN  hf-i  = 0.
   // html head
   ASSIGN p-head = "<!DOCTYPE html>~n
<html lang=~"de~">~n
   <head>~n
      <meta charset=~"utf-8~">
      <meta name=~"viewport~" content=~"width=device-width, initial-scale=1.0~">
      <title>Tableau</title>~n
      <style>~n
         table ~{~n
                  border-collapse: collapse; ~n
                  width: 100%;~n
         ~}~n
         th, td ~{~n
            text-align: left;~n
            padding: 8px;~n
         ~}~n
         tr:nth-child(even) ~{ ~n
            background-color: #f2f2f2;~n
         ~}~n
         th ~{ ~n
            background-color: #4CAF50;~n
            color: white;~n
         ~} ~n           
         .group-header ~{~n
           cursor: pointer;~n
           font-weight: bold;~n
         ~}~n
         input[type=~"text~"] ~{
           padding: 5px;
           border: 1px solid #ccc;
           border-radius: 4px;
           box-sizing: border-box;
           margin-top: 10px;
         ~}
         button ~{
           background-color: #4CAF50;
           color: white;
           padding: 5px 10px;
           border: none;
           border-radius: 4px;
           cursor: pointer;
           margin-top: 10px;
         ~}
      </style>~n
   </head> ~n
   <body>~n".
   

   ASSIGN p-script = "<script>~n
   const table = document.querySelector('Dateifinder, Ergebnistabelle');~n
   const rows = Array.from(table.querySelectorAll('tr'));~n

   // Ajouter une classe Ö la premiäre cellule de chaque groupe avec le màme Path~n
   let currentPath = null;~n
   rows.forEach(row => ~{~n
     const pathCell = row.querySelector('#d-path');~n
     if (pathCell) ~{~n
       const path = pathCell.textContent;~n
       if (path !== currentPath) ~{~n
         currentPath = path;~n
         row.classList.add('group-header'); ~n
       ~}  ~n
     ~}  ~n
   ~}); ~n

   // Masquer les cellules suivantes de chaque groupe ~n
   const groupHeaders = table.querySelectorAll('.group-header');~n
   groupHeaders.forEach(header => ~{~n
     const groupRows = []; ~n
     let next = header.nextElementSibling; ~n
     while (next && !next.classList.contains('group-header')) ~{~n
       groupRows.push(next);~n
       next = next.nextElementSibling; ~n
     ~}   ~n
     groupRows.forEach(row => ~{ ~n
       const cells = row.querySelectorAll('td');~n
       for (let i = 1; i < cells.length; i++) ~{ ~n
         cells[i].style.display = 'none'; ~n
       ~} ~n
     ~});~n
   ~}); ~n

   // Ajouter un ÇvÇnement ~"click~" pour afficher/masquer les cellules cachÇes  ~n
   groupHeaders.forEach(header => ~{  ~n
     header.addEventListener('click', () => ~{~n
       const groupRows = [];~n
       let next = header.nextElementSibling; ~n
       while (next && !next.classList.contains('group-header')) ~{ ~n
         groupRows.push(next);~n
         next = next.nextElementSibling;~n
       ~}                ~n
       groupRows.forEach(row => ~{ ~n
         const cells = row.querySelectorAll('td');~n
         for (let i = 1; i < cells.length; i++) ~{ ~n
           cells[i].style.display = cells[i].style.display === 'none' ? '' : 'none'; ~n
         ~} ~n
       ~}); ~n
     ~}); ~n
   ~});  ~n
</script>~n".

   // Script for search table 
   ASSIGN p-search = "function searchTable() ~{~n
  var input, filter, table, tr, td, i, j, txtValue; ~n
  input = document.getElementById(~"searchInput~");~n
  filter = input.value.toUpperCase(); ~n
  table = document.getElementById(~"myTable~"); ~n
  tr = table.getElementsByTagName(~"tr~");  ~n
  for (i = 0; i < tr.length; i++) ~{  ~n
    td = tr[i].getElementsByTagName(~"td~"); ~n
    for (j = 0; j < td.length; j++) ~{~n
      txtValue = td[j].textContent || td[j].innerText;~n
      if (txtValue.toUpperCase().indexOf(filter) > -1) ~{ ~n
        tr[i].style.display = ~"~";  ~n
        break;    ~n
      ~} else ~{ ~n
        tr[i].style.display = ~"none~";~n
      ~} ~n
    ~} ~n
  ~} ~n
~}~n".
   
   PUT UNFORMATTED p-head.
   PUT UNFORMATTED "<input type=~"text~" id=~"searchInput~" placeholder=~"Search~">
<button type=~"button~" onclick=~"searchTable()~">OK</button>".
   PUT UNFORMATTED "<table id=~"myTable~">~n".
   PUT UNFORMATTED "<tr>~n<th>N¯</th>~n<th>Path</th>~n<th>Name</th>~n<th>Wort</th>~n<th>L N¯</th>~n<th>Linie</th>~n</tr>~n".

   FOR EACH tt-gefunden :
      ASSIGN 
         hf-wort = tt-gefunden.wort.
      // remouve the last char if is an line brack, and make a trim
      IF (SUBSTRING(hf-wort, LENGTH(hf-wort), 1) = CHR(10)) THEN DO:
         ASSIGN
            hf-wort = SUBSTRING(hf-wort, 1, LENGTH(hf-wort) - 1)
            hf-wort = TRIM(hf-wort).
      END.
      ASSIGN 
         hf-linie = REPLACE(tt-gefunden.linie, "~"", "~\~"")
         hf-linie = REPLACE(hf-linie, "<", "~\<")
         hf-linie = REPLACE(hf-linie, ">", "~\>").
      PUT UNFORMATTED "<tr>~n".
      IF hf-old <> tt-gefunden.datei-path THEN DO:
         ASSIGN hf-i  = hf-i + 1. 
         PUT UNFORMATTED "<td>" hf-i "</td>~n".
         PUT UNFORMATTED "<td id= ~"d-path~">" tt-gefunden.datei-path "</td>~n".
         PUT UNFORMATTED "<td>" tt-gefunden.datei-name "</td>~n".
      END.
      ELSE DO:  
         PUT UNFORMATTED "<td> - </td>~n".
         PUT UNFORMATTED "<td> - </td>~n".
         PUT UNFORMATTED "<td> - </td>~n".
      END.
      PUT UNFORMATTED "<td><xmp>" hf-wort "</xmp></td>~n".
      PUT UNFORMATTED "<td>" tt-gefunden.linie-num "</td>~n".
      PUT UNFORMATTED "<td><xmp>" TRIM(tt-gefunden.linie) "</xmp></td>~n".
      PUT UNFORMATTED "</tr>~n".
      ASSIGN hf-old = tt-gefunden.datei-path.
   END.

   PUT UNFORMATTED "</table>~n".
   
   PUT UNFORMATTED "<script>~n".
   // make it in json format too.
   PUT UNFORMATTED "const data = [~n".
   FOR EACH tt-gefunden :
      ASSIGN 
         hf-wort = tt-gefunden.wort
         hf-wort = REPLACE(hf-wort, "~"", "~\~"")
         hf-wort = REPLACE(hf-wort, "<", "~\<")
         hf-wort = REPLACE(hf-wort, ">", "~\>").
      // remouve the last char if is an line brack, and make a trim
      IF (SUBSTRING(hf-wort, LENGTH(hf-wort), 1) = CHR(10)) THEN DO:
         ASSIGN
            hf-wort = SUBSTRING(hf-wort, 1, LENGTH(hf-wort) - 1)
            hf-wort = TRIM(hf-wort).
      END.
       PUT UNFORMATTED "~{~n".
       PUT UNFORMATTED "~"path~" : ~"" tt-gefunden.datei-path "~",~n".
       PUT UNFORMATTED "~"name~" : ~"" tt-gefunden.datei-name "~",~n".
       PUT UNFORMATTED "~"wort~" : ~""  hf-wort "~",~n".
       PUT UNFORMATTED "~"l-num~" : " tt-gefunden.linie-num ",~n".
       PUT UNFORMATTED "~"linie~" : ~"" TRIM(hf-linie) "~"~n".
       PUT UNFORMATTED "~},~n".
   END.    
   PUT UNFORMATTED "];~n".
   
   PUT UNFORMATTED p-search. 
   PUT UNFORMATTED "</script>~n". 

   PUT UNFORMATTED "</body>~n".
   PUT UNFORMATTED "</html>~n".

   OUTPUT CLOSE.
   // When the job is finished, you will be asked to open the output file. 
   MESSAGE "Der Job ist fertig" SKIP
         "und in einer html-Datei gespeichert." SKIP
         "Wollen Sie die Datei îffnen?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
       TITLE "Fertig -> .html" UPDATE lChoice AS LOGICAL.
     CASE lChoice:
       WHEN TRUE THEN /* Yes */ DO:            
          OS-COMMAND SILENT VALUE("start " + hf-export-path-g).
       END.
     END CASE.    
END PROCEDURE.
/* DO WITH FRAME {&FRAME-NAME}: */
/*                              */
/* END.     /*  offline B±rot±ren f±r Batteriestatus ±berlesen daher Pnr = 0 */                    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE p-to-txt C-Win 
PROCEDURE p-to-txt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEF VAR hf-temp   AS CHAR NO-UNDO.
   DEF VAR hf-old    AS CHAR NO-UNDO.
   
   OUTPUT TO VALUE(hf-export-path-g) NO-CONVERT.
   FOR EACH tt-gefunden :
      PUT UNFORMATTED tt-gefunden.datei-path "," tt-gefunden.datei-name "," tt-gefunden.wort "," tt-gefunden.linie-num "," tt-gefunden.linie SKIP.
   END.
   
   OUTPUT CLOSE.
   MESSAGE "Der Job ist fertig" SKIP 
         "und in einer txt-Datei gespeichert." SKIP
         "Wollen Sie die Datei îffnen?"
       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
       TITLE "Fertig -> .txt" UPDATE lChoice AS LOGICAL.
     CASE lChoice:
       WHEN TRUE THEN /* Yes */ DO:            
          OS-COMMAND SILENT VALUE("start " + hf-export-path-g).
       END.
     END CASE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-chang-ext C-Win 
FUNCTION f-chang-ext RETURNS CHARACTER // Definiert den RÅckgabetyp der Funktion (Zeichenkette)

(INPUT hf-name AS CHAR, INPUT hf-ext AS CHAR) :  // Definiert zwei Eingabeparameter: "hf-name" vom Typ Zeichenkette und "hf-ext" vom Typ Zeichenkette

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/ 

   DEF VAR lastpkt AS INT NO-UNDO.  // Deklariert eine Variable "lastpkt" vom Typ Integer und initialisiert sie mit dem Wert 0
   DEF VAR hf-result AS CHAR NO-UNDO.  // Deklariert eine Variable "hf-result" vom Typ Zeichenkette

   ASSIGN 
      lastpkt = R-INDEX(hf-name, ".").  // Weist "lastpkt" den Wert der Position des letzten Vorkommens von "." in "hf-name" zu
   IF lastpkt <> 0 THEN  // PrÅft, ob "." in "hf-name" vorhanden ist
      ASSIGN 
         hf-result = SUBSTRING(hf-name, 1, lastpkt) + hf-ext.  // Weist "hf-result" den Wert von "hf-name" bis zur Position von "." (inklusive) und "hf-ext" zu
   ELSE   
      ASSIGN hf-result = hf-name + "." + hf-ext.  // Weist "hf-result" den Wert von "hf-name", "." und "hf-ext" zu
      
   RETURN hf-result.  // Gibt "hf-result" zurÅck
END FUNCTION.  // Beendet die Funktion

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-check-filter C-Win 
FUNCTION f-check-filter RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logisch)

( /* parameter-definitions */ ) :  // Definiert keine Eingabeparameter

/*------------------------------------------------------------------------------
  Purpose:  
    Nur prÅfen, wenn alles zuerst deaktiviert ist
------------------------------------------------------------------------------*/

DO WITH FRAME {&FRAME-NAME}:  // ôffnet ein Fenster, in dem die Funktion ausgefÅhrt wird

   DEF VAR hf-count-f AS INT NO-UNDO.  // Deklariert eine Variable "hf-count-f" vom Typ Integer und initialisiert sie mit dem Wert 0
   ASSIGN hf-count-f = 0.  // Weist "hf-count-f" den Wert 0 zu
   IF f-p:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-p" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend
   IF f-w:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-w" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend
   IF f-r:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-r" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend
   IF f-csv:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-csv" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend
   IF f-txt:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-txt" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend
   IF f-all:SCREEN-VALUE = "no" THEN ASSIGN hf-count-f = hf-count-f + 1.  // PrÅft, ob der Filter "f-all" deaktiviert ist, und erhîht den Wert von "hf-count-f" entsprechend

   IF hf-count-f > 5 THEN DO:  // PrÅft, ob alle Filter deaktiviert sind
      RETURN TRUE.  // Gibt "TRUE" zurÅck, wenn alle Filter deaktiviert sind
   END.
   RETURN FALSE.  // Gibt "FALSE" zurÅck, wenn nicht alle Filter deaktiviert sind

END.  // Schlie·t das Fenster
END FUNCTION.  // Beendet die Funktion

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-check-path C-Win 
FUNCTION f-check-path RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logisch)
( /* parameter-definitions */ ) :  // Definiert keine Eingabeparameter
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: check only if all firter is un checked   
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:  // ôffnet ein Fenster, in dem die Funktion ausgefÅhrt wird
   
   // öberprÅfen, ob der Wert von t-filen:SCREEN-VALUE ungleich "?" und "" und ":" ist.
   IF t-filen:SCREEN-VALUE <> ? AND t-filen:SCREEN-VALUE <> "" AND t-filen:SCREEN-VALUE <> ":" THEN DO:  
      RETURN TRUE. // Wenn die Bedingung wahr ist, dann TRUE zurÅckgeben, 
   END.
   ELSE 
      RETURN FALSE. // Wenn die Bedingung wahr ist, dann FALSE zurÅckgeben.

END.  // Schlie·t das Fenster
END FUNCTION.  // Beendet die Funktion

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-get-ext C-Win 
FUNCTION f-get-ext RETURNS CHARACTER // Definiert den RÅckgabetyp der Funktion (Zeichenkette)

(INPUT hf-name AS CHAR) : // Definiert parameter: "hf-name" vom Typ Zeichenkette

/*------------------------------------------------------------------------------
   Purpose:Ermittelt die Dateityp-Extension fÅr eine Datei aus dem Dateinamen.

   Notes: Der RÅckgabewert ist der Teil des Dateinamens, der nach dem letzten 
   Punkt (".") kommt. 
   Wenn der Dateiname kein Punkt enthÑlt, wird ein leerer String zurÅckgegeben.
------------------------------------------------------------------------------*/
   DEF VAR lastpkt AS INT NO-UNDO.  // Deklariert eine Variable "lastpkt" vom Typ Integer und initialisiert sie mit dem Wert 0
   DEF VAR hf-result AS CHAR NO-UNDO.  // Deklariert eine Variable "hf-result" vom Typ Zeichenkette
   
   ASSIGN 
      lastpkt = R-INDEX(hf-name, ".").  // Weist "lastpkt" den Wert der Position des letzten Vorkommens von "." in "hf-name" zu
   IF lastpkt <> 0 THEN // PrÅft, ob "." in "hf-name" vorhanden ist
      ASSIGN hf-result = SUBSTRING(hf-name, lastpkt + 1). // Weist "hf-result" die Zeichenfolge zu, die nach dem letzten Punkt "."
   ELSE   
      ASSIGN hf-result =  "". //Setzen Sie "hf-result" auf eine leere Zeichenfolge   
   RETURN hf-result.  // Gibt "hf-result" zurÅck
END FUNCTION.  // Beendet die Funktion

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-in-array C-Win 
FUNCTION f-in-array RETURNS LOGICAL // Definiert den RÅckgabetyp der Funktion (logischer Wert)

(INPUT hf-word AS CHAR, INPUT lists AS CHAR EXTENT) :  // Definiert zwei Eingabeparameter: "hf-word" vom Typ Zeichenkette und "lists" vom Typ Zeichenkette-Array

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/ 
   // Kommentarbereich, in dem der Zweck der Funktion und sonstige Notizen angegeben werden kînnen

   DEF VAR i AS INT NO-UNDO.  // Deklariert eine Variable "i" vom Typ Integer und initialisiert sie mit dem Wert 1

   DO i = 1 TO EXTENT(lists):  // Schleife, die von 1 bis zur LÑnge des "lists"-Arrays lÑuft
      IF hf-word = lists[i] THEN  // PrÅft, ob der aktuelle Wert von "lists" mit "hf-word" Åbereinstimmt
         RETURN TRUE.  // Gibt TRUE zurÅck, wenn die Bedingung erfÅllt ist, und beendet die Funktion
   END.   
   RETURN FALSE.  // Gibt FALSE zurÅck, wenn "hf-word" nicht in "lists" enthalten ist

END FUNCTION.  // Beendet die Funktion

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-progressBar C-Win 
FUNCTION f-progressBar RETURNS LOGICAL
  ( i-loop AS INT, j-loop AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF i-loop <= j-loop THEN DO:
   DO WITH FRAME {&FRAME-NAME}: 
      IF ( (( i-loop / j-loop) * 100) * 2) < 1 THEN   
         ASSIGN progress-bar-2:WIDTH-PIXELS = 1.
      ELSE   
         ASSIGN progress-bar-2:WIDTH-PIXELS = (( i-loop / j-loop) * 100) * 2.
      ASSIGN
         progress-bar-2:HIDDEN = FALSE
         progress-bar-2:FILLED = TRUE
         progress-bar-2:BGCOLOR = 10
         t-finded:SCREEN-VALUE = STRING( i-loop) + "/" + STRING(j-loop) + " -- " + STRING( ROUND(( i-loop / j-loop) * 100, 0 )) + "%" .
         
      IF i-loop = j-loop THEN DO:
         SESSION:SET-WAIT-STATE("").                    
         ASSIGN
            t-finded:FONT = 7
            t-finded:COLUMN = 52  
            t-finded:ROW = t-finded:ROW + 1.18
            t-finded:WIDTH-PIXELS = 50
            t-finded:BGCOLOR = 10
            t-finded:SCREEN-VALUE = "FERTIG".
      END.     
   END.
   RETURN TRUE. /* Function return value. */
END.
ELSE DO:
   MESSAGE "Wir haben ein Fehler" SKIP "A kann nicht grî·er sein als B" VIEW-AS ALERT-BOX .
   RETURN FALSE.
END.
   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-reset-progressBar C-Win 
FUNCTION f-reset-progressBar RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:    
   ASSIGN
      progress-bar-2:HIDDEN = TRUE
      progress-bar-2:WIDTH-PIXELS = 1
      progress-bar-2:BGCOLOR = ?
      t-finded:FONT = 1
      t-finded:COLUMN = 41.14
      t-finded:ROW = 13.23
      t-finded:WIDTH-PIXELS = 100
      t-finded:BGCOLOR = ?
      t-finded:SCREEN-VALUE = "".     
   RETURN FALSE.
END.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION logo-hidden C-Win 
FUNCTION logo-hidden RETURNS WIDGET-HANDLE
  ( ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:          
      ENABLE l-label-0 WITH FRAME F-Main.  
      l-label-0:SENSITIVE  IN FRAME F-Main = TRUE. 
      ENABLE i-filen-lw WITH FRAME F-Main.  
      i-filen-lw:SENSITIVE  IN FRAME F-Main = TRUE. 
      ENABLE i-filen-pf WITH FRAME F-Main.  
      i-filen-pf:SENSITIVE  IN FRAME F-Main = TRUE.   
   ASSIGN
      btn-start:HIDDEN = TRUE
      t-logo:HIDDEN = TRUE
      img-logo:HIDDEN = TRUE
      l-label-0:HIDDEN = FALSE
      l-label-0:SCREEN-VALUE = "Folder path:"
      i-filen-lw:HIDDEN = FALSE
      i-filen-pf:HIDDEN = FALSE.
END.      
   RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


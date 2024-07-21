      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0003.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0003.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:11:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0003)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0003))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0003 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      ED_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      MA_ST_NR                       INTEGER NOT NULL,
CCC004*      ED_RCK_STZ                     DECIMAL(5, 2) NOT NULL,
CCC004*      ED_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      ED_BEZ                         CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0003                           *
      ******************************************************************
       01  VIVB0003-GESAMT.
        05  NI-VIVB0003-TAB.
           10  NI-VIVB0003-ELE     PIC S9(04) COMP   OCCURS  6  TIMES.
        05  NI-VIVB0003            REDEFINES  NI-VIVB0003-TAB.
           10 NI-ED-ST-NR          PIC S9(04) COMP.
           10 NI-ED-LAST-UPDATE    PIC S9(04) COMP.
           10 NI-MA-ST-NR          PIC S9(04) COMP.
           10 NI-ED-RCK-STZ        PIC S9(04) COMP.
           10 NI-ED-RCK-STZ-DAT    PIC S9(04) COMP.
           10 NI-ED-BEZ            PIC S9(04) COMP.
        05  DCL-VIVB0003.
           10 ED-ST-NR             PIC S9(9) USAGE COMP.
           10 ED-LAST-UPDATE       PIC X(26).
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 ED-RCK-STZ           PIC S999V99 USAGE COMP-3.
           10 ED-RCK-STZ-DAT       PIC X(10).
           10 ED-BEZ               PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************

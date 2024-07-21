      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0002.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0002.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:22   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:11:28   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0002)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0002))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0002 TABLE
CCC004*    ( GK_ST_NR                       INTEGER NOT NULL,
CCC004*      GK_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      GK_FUNKTION                    SMALLINT NOT NULL,
CCC004*      GK_DAT_VON                     DATE NOT NULL,
CCC004*      GK_DAT_BIS                     DATE NOT NULL,
CCC004*      GK_BEZ                         CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0002                           *
      ******************************************************************
       01  VIVB0002-GESAMT.
        05  NI-VIVB0002-TAB.
           10  NI-VIVB0002-ELE     PIC S9(04) COMP   OCCURS  6  TIMES.
        05  NI-VIVB0002            REDEFINES  NI-VIVB0002-TAB.
           10 NI-GK-ST-NR          PIC S9(04) COMP.
           10 NI-GK-LAST-UPDATE    PIC S9(04) COMP.
           10 NI-GK-FUNKTION       PIC S9(04) COMP.
           10 NI-GK-DAT-VON        PIC S9(04) COMP.
           10 NI-GK-DAT-BIS        PIC S9(04) COMP.
           10 NI-GK-BEZ            PIC S9(04) COMP.
        05  DCL-VIVB0002.
           10 GK-ST-NR             PIC S9(9) USAGE COMP.
           10 GK-LAST-UPDATE       PIC X(26).
           10 GK-FUNKTION          PIC S9(4) USAGE COMP.
           10 GK-DAT-VON           PIC X(10).
           10 GK-DAT-BIS           PIC X(10).
           10 GK-BEZ               PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************

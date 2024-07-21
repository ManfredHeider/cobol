      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBA023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:09:20   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *      VERSION   1    18.07.94 17:25:06
      ******************************************************************
      * DCLGEN TABLE(VIVBA023)                                         *
      *        LIBRARY(T130003.CE010000.SRCDCL(CVVBA023))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA023 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      EDD_BEZUGS_NR                  INTEGER NOT NULL,
CCC004*      EDD_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      EDD_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EDD_DAT_BIS                    DATE NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_EDD_URSACHE                SMALLINT NOT NULL,
CCC004*      KNR_EDD_URSACHE                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA023                           *
      ******************************************************************
       01  VIVBA023-GESAMT.
        05  NI-VIVBA023-TAB.
           10  NI-VIVBA023-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-VIVBA023            REDEFINES   NI-VIVBA023-TAB.
           10 NI-ED-ST-NR          PIC S9(04)  COMP.
           10 NI-EDD-BEZUGS-NR     PIC S9(04)  COMP.
           10 NI-EDD-DAT-VON       PIC S9(04)  COMP.
           10 NI-EDD-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-EDD-DAT-BIS       PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KTY-EDD-URSACHE   PIC S9(04)  COMP.
           10 NI-KNR-EDD-URSACHE   PIC S9(04)  COMP.
        05  DCL-VIVBA023.
           10 ED-ST-NR             PIC S9(9) USAGE COMP.
           10 EDD-BEZUGS-NR        PIC S9(9) USAGE COMP.
           10 EDD-DAT-VON          PIC X(26).
           10 EDD-LAST-UPDATE      PIC X(26).
           10 EDD-DAT-BIS          PIC X(10).
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KTY-EDD-URSACHE      PIC S9(4) USAGE COMP.
           10 KNR-EDD-URSACHE      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

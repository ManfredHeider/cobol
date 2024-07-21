      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBB023.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVBB023.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Jun 1998 09:57:30   T13AE26
      *AVB00391 -  Anpassung BD-Struktur
      *
      *   Rev 1.0   18 Nov 1997 15:20:36   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBB023)                                         *
      *        LIBRARY(T130003.CE010000.SRCDCL(CVVBB023))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBB023 TABLE
CCC004*    ( ED_ST_NR                       INTEGER NOT NULL,
CCC004*      EDD_BEZUGS_NR                  INTEGER NOT NULL,
CCC004*      EDD_DAT_VON                    TIMESTAMP NOT NULL,
CCC004*      EDD_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      EDD_DAT_BIS                    DATE NOT NULL,
CCC004*      BVH_UNTST_NR                   INTEGER,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KTY_EDD_URSACHE                SMALLINT NOT NULL,
CCC004*      KNR_EDD_URSACHE                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBB023                           *
      ******************************************************************
       01  VIVBB023-GESAMT.
        05  NI-VIVBB023-TAB.
           10  NI-VIVBB023-ELE     PIC S9(04)  COMP   OCCURS 10  TIMES.
        05  NI-VIVBB023            REDEFINES   NI-VIVBB023-TAB.
           10 NI-ED-ST-NR          PIC S9(04)  COMP.
           10 NI-EDD-BEZUGS-NR     PIC S9(04)  COMP.
           10 NI-EDD-DAT-VON       PIC S9(04)  COMP.
           10 NI-EDD-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-EDD-DAT-BIS       PIC S9(04)  COMP.
200897     10 NI-BVH-UNTST-NR      PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KTY-EDD-URSACHE   PIC S9(04)  COMP.
           10 NI-KNR-EDD-URSACHE   PIC S9(04)  COMP.
        05  DCL-VIVBB023.
           10 ED-ST-NR             PIC S9(9) USAGE COMP.
           10 EDD-BEZUGS-NR        PIC S9(9) USAGE COMP.
           10 EDD-DAT-VON          PIC X(26).
           10 EDD-LAST-UPDATE      PIC X(26).
           10 EDD-DAT-BIS          PIC X(10).
200897     10 BVH-UNTST-NR         PIC S9(9) USAGE COMP.
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KTY-EDD-URSACHE      PIC S9(4) USAGE COMP.
           10 KNR-EDD-URSACHE      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:52:30   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0006)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0006))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0006 TABLE
CCC004*    ( DIR_NR                         SMALLINT NOT NULL,
CCC004*      DIR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      KTY_DIR_TYP                    SMALLINT NOT NULL,
CCC004*      KNR_DIR_TYP                    SMALLINT NOT NULL,
CCC004*      KTY_REGION                     SMALLINT NOT NULL,
CCC004*      KNR_REGION                     SMALLINT NOT NULL,
CCC004*      KTY_RESERV_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_RESERV_TYP                 SMALLINT NOT NULL,
CCC004*      MA_ST_NR                       INTEGER NOT NULL,
CCC004*      DIR_NAME                       CHAR(40) NOT NULL,
CCC004*      DIR_INFORMATION                CHAR(150) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0006                           *
      ******************************************************************
       01  TBVB0006-GESAMT.
        05  NI-TBVB0006-TAB.
           10  NI-TBVB0006-ELE       PIC S9(04)  COMP   OCCURS 11 TIMES.
        05  NI-TBVB0006              REDEFINES   NI-TBVB0006-TAB.
           10 NI-DIR-NR              PIC S9(04)  COMP.
           10 NI-DIR-LAST-UPDATE     PIC S9(04)  COMP.
           10 NI-KTY-DIR-TYP         PIC S9(04)  COMP.
           10 NI-KNR-DIR-TYP         PIC S9(04)  COMP.
           10 NI-KTY-REGION          PIC S9(04)  COMP.
           10 NI-KNR-REGION          PIC S9(04)  COMP.
           10 NI-KTY-RESERV-TYP      PIC S9(04)  COMP.
           10 NI-KNR-RESERV-TYP      PIC S9(04)  COMP.
           10 NI-MA-ST-NR            PIC S9(04)  COMP.
           10 NI-DIR-NAME            PIC S9(04)  COMP.
           10 NI-DIR-INFORMATION     PIC S9(04)  COMP.
        05  DCL-TBVB0006.
           10 DIR-NR               PIC S9(4) USAGE COMP.
           10 DIR-LAST-UPDATE      PIC X(26).
           10 KTY-DIR-TYP          PIC S9(4) USAGE COMP.
           10 KNR-DIR-TYP          PIC S9(4) USAGE COMP.
           10 KTY-REGION           PIC S9(4) USAGE COMP.
           10 KNR-REGION           PIC S9(4) USAGE COMP.
           10 KTY-RESERV-TYP       PIC S9(4) USAGE COMP.
           10 KNR-RESERV-TYP       PIC S9(4) USAGE COMP.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 DIR-NAME             PIC X(40).
           10 DIR-INFORMATION      PIC X(150).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

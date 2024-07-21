      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0013.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0013.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:54:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0013)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0013))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0013 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      AV_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      AV_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KTY_AV_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_AV_TYP                     SMALLINT NOT NULL,
CCC004*      AV_VERTRAGSDAT                 DATE,
CCC004*      AV_DAT_BIS                     DATE NOT NULL,
CCC004*      AV_UMSCHREIB_DAT               DATE,
CCC004*      AV_UMSCHREIB_PTN               CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0013                           *
      ******************************************************************
       01  TBVB0013-GESAMT.
        05  NI-TBVB0013-TAB.
           10  NI-TBVB0013-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-TBVB0013            REDEFINES   NI-TBVB0013-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-AV-DAT-VON        PIC S9(04)  COMP.
           10 NI-AV-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KTY-AV-TYP        PIC S9(04)  COMP.
           10 NI-KNR-AV-TYP        PIC S9(04)  COMP.
           10 NI-AV-VERTRAGSDAT    PIC S9(04)  COMP.
           10 NI-AV-DAT-BIS        PIC S9(04)  COMP.
           10 NI-AV-UMSCHREIB-DAT  PIC S9(04)  COMP.
           10 NI-AV-UMSCHREIB-PTN  PIC S9(04)  COMP.
        05  DCL-TBVB0013.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 AV-DAT-VON           PIC X(26).
           10 AV-LAST-UPDATE       PIC X(26).
           10 KTY-AV-TYP           PIC S9(4) USAGE COMP.
           10 KNR-AV-TYP           PIC S9(4) USAGE COMP.
           10 AV-VERTRAGSDAT       PIC X(10).
           10 AV-DAT-BIS           PIC X(10).
           10 AV-UMSCHREIB-DAT     PIC X(10).
           10 AV-UMSCHREIB-PTN     PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

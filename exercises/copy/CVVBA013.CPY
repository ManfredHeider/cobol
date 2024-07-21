      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.04.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA013.CPv  $
      *
      *   Rev 2.1   26 Apr 2007 15:44:22   t13ae30
      *AVB00908: Neuer VB-Vertrag 2007
      *
      *   Rev 2.0   24 Apr 2007 15:23:22   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVBA013)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA013))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA013 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      AV_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      AV_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KTY_AV_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_AV_TYP                     SMALLINT NOT NULL,
CCC004*      AV_VERTRAGSDAT                 DATE,
CCC004*      AV_DAT_BIS                     DATE NOT NULL,
CCC004*      AV_UMSCHREIB_DAT               DATE,
CCC004*      AV_UMSCHREIB_PTN               CHAR(40) NOT NULL,
CCC004*      AV_RUECKSCH_DAT                DATE
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA013                           *
      ******************************************************************
       01  VIVBA013-GESAMT.
        05  NI-VIVBA013-TAB.
           10  NI-VIVBA013-ELE     PIC S9(04)  COMP   OCCURS 10  TIMES.
        05  NI-VIVBA013            REDEFINES   NI-VIVBA013-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-AV-DAT-VON        PIC S9(04)  COMP.
           10 NI-AV-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KTY-AV-TYP        PIC S9(04)  COMP.
           10 NI-KNR-AV-TYP        PIC S9(04)  COMP.
           10 NI-AV-VERTRAGSDAT    PIC S9(04)  COMP.
           10 NI-AV-DAT-BIS        PIC S9(04)  COMP.
           10 NI-AV-UMSCHREIB-DAT  PIC S9(04)  COMP.
           10 NI-AV-UMSCHREIB-PTN  PIC S9(04)  COMP.
           10 NI-AV-RUECKSCH-DAT   PIC S9(04)  COMP.
        05  DCL-VIVBA013.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 AV-DAT-VON           PIC X(26).
           10 AV-LAST-UPDATE       PIC X(26).
           10 KTY-AV-TYP           PIC S9(4) USAGE COMP.
           10 KNR-AV-TYP           PIC S9(4) USAGE COMP.
           10 AV-VERTRAGSDAT       PIC X(10).
           10 AV-DAT-BIS           PIC X(10).
           10 AV-UMSCHREIB-DAT     PIC X(10).
           10 AV-UMSCHREIB-PTN     PIC X(40).
           10 AV-RUECKSCH-DAT      PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************

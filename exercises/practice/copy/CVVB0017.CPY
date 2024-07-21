      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0017.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0017.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:14:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0017)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0017))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0017 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      RF_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      RF_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      RF_DAT_BIS                     DATE NOT NULL,
CCC004*      RF_PROZ_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      RF_ABLAUF_DAT                  DATE NOT NULL,
CCC004*      KTY_RF_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_RF_TYP                     SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0017                           *
      ******************************************************************
       01  VIVB0017-GESAMT.
        05  NI-VIVB0017-TAB.
           10  NI-VIVB0017-ELE     PIC S9(04)  COMP   OCCURS  8  TIMES.
        05  NI-VIVB0017            REDEFINES   NI-VIVB0017-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-RF-DAT-VON        PIC S9(04)  COMP.
           10 NI-RF-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-RF-DAT-BIS        PIC S9(04)  COMP.
           10 NI-RF-PROZ-STZ       PIC S9(04)  COMP.
           10 NI-RF-ABLAUF-DAT     PIC S9(04)  COMP.
           10 NI-KTY-RF-TYP        PIC S9(04)  COMP.
           10 NI-KNR-RF-TYP        PIC S9(04)  COMP.
        05  DCL-VIVB0017.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 RF-DAT-VON           PIC X(26).
           10 RF-LAST-UPDATE       PIC X(26).
           10 RF-DAT-BIS           PIC X(10).
           10 RF-PROZ-STZ          PIC S999V99 USAGE COMP-3.
           10 RF-ABLAUF-DAT        PIC X(10).
           10 KTY-RF-TYP           PIC S9(4) USAGE COMP.
           10 KNR-RF-TYP           PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0005.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0005.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 11:52:16   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0005)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0005))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0005 TABLE
CCC004*    ( RFT_ST_NR                      INTEGER NOT NULL,
CCC004*      RFT_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      MA_ST_NR                       INTEGER NOT NULL,
CCC004*      RFT_PROV_STZ_AP                DECIMAL(5, 2) NOT NULL,
CCC004*      RFT_PROV_STZ_GP                DECIMAL(5, 2) NOT NULL,
CCC004*      RFT_PROZ_STZ                   DECIMAL(5, 2) NOT NULL,
CCC004*      RFT_RCK_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      RFT_RCK_STZ_DAT                DATE NOT NULL,
CCC004*      RFT_DAT_VON                    DATE NOT NULL,
CCC004*      RFT_DAT_BIS                    DATE,
CCC004*      RFT_ABLAUF_DAT                 DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0005                           *
      ******************************************************************
       01  TBVB0005-GESAMT.
        05  NI-TBVB0005-TAB.
           10  NI-TBVB0005-ELE     PIC S9(04)  COMP   OCCURS 11 TIMES.
        05  NI-TBVB0005            REDEFINES   NI-TBVB0005-TAB.
           10 NI-RFT-ST-NR         PIC S9(04)  COMP.
           10 NI-RFT-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-RFT-PROV-STZ-AP   PIC S9(04)  COMP.
           10 NI-RFT-PROV-STZ-GP   PIC S9(04)  COMP.
           10 NI-RFT-PROZ-STZ      PIC S9(04)  COMP.
           10 NI-RFT-RCK-STZ       PIC S9(04)  COMP.
           10 NI-RFT-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 NI-RFT-DAT-VON       PIC S9(04)  COMP.
           10 NI-RFT-DAT-BIS       PIC S9(04)  COMP.
           10 NI-RFT-ABLAUF-DAT    PIC S9(04)  COMP.
        05  DCL-TBVB0005.
           10 RFT-ST-NR            PIC S9(9) USAGE COMP.
           10 RFT-LAST-UPDATE      PIC X(26).
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 RFT-PROV-STZ-AP      PIC S999V99 USAGE COMP-3.
           10 RFT-PROV-STZ-GP      PIC S999V99 USAGE COMP-3.
           10 RFT-PROZ-STZ         PIC S999V99 USAGE COMP-3.
           10 RFT-RCK-STZ          PIC S999V99 USAGE COMP-3.
           10 RFT-RCK-STZ-DAT      PIC X(10).
           10 RFT-DAT-VON          PIC X(10).
           10 RFT-DAT-BIS          PIC X(10).
           10 RFT-ABLAUF-DAT       PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************

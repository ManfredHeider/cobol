      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTVB0051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CTVB0051.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Apr 09 1997 16:45:44   T13AE31
      *EINFUEHRUNG PEW
      *
      *   Rev 1.0   Nov 04 1995 12:02:02   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(TBVB0051)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CTVB0051))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE TBVB0051 TABLE
CCC004*    ( JUR_ST_NR                      INTEGER NOT NULL,
CCC004*      JUR_LAST_UPDATE                TIMESTAMP NOT NULL,
CCC004*      JUR_DAT_VON                    DATE NOT NULL,
CCC004*      JUR_DAT_BIS                    DATE,
CCC004*      JUR_PROV_STZ_AP                DECIMAL(5, 2) NOT NULL,
CCC004*      JUR_PROV_STZ_GP                DECIMAL(5, 2) NOT NULL,
CCC004*      JUR_PROZ_STZ                   DECIMAL(5, 2) NOT NULL,
CCC004*      JUR_RCK_STZ_DAT                DATE NOT NULL,
CCC004*      JUR_RCK_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      KTY_VERSAND                    SMALLINT NOT NULL,
CCC004*      KNR_VERSAND                    SMALLINT NOT NULL,
CCC004*      JUR_BEZ                        CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0051                           *
      ******************************************************************
       01  TBVB0051-GESAMT.
        05  NI-TBVB0051-TAB.
           10  NI-TBVB0051-ELE     PIC S9(04)  COMP   OCCURS  12 TIMES.
        05  NI-TBVB0051            REDEFINES   NI-TBVB0051-TAB.
           10 NI-JUR-ST-NR         PIC S9(04)  COMP.
           10 NI-JUR-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-JUR-DAT-VON       PIC S9(04)  COMP.
           10 NI-JUR-DAT-BIS       PIC S9(04)  COMP.
           10 NI-JUR-PROV-STZ-AP   PIC S9(04)  COMP.
           10 NI-JUR-PROV-STZ-GP   PIC S9(04)  COMP.
220297     10 NI-JUR-PROZ-STZ      PIC S9(04)  COMP.
           10 NI-JUR-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 NI-JUR-RCK-STZ       PIC S9(04)  COMP.
           10 NI-KTY-VERSAND       PIC S9(04)  COMP.
           10 NI-KNR-VERSAND       PIC S9(04)  COMP.
           10 NI-JUR-BEZ           PIC S9(04)  COMP.
        05  DCL-TBVB0051.
           10 JUR-ST-NR            PIC S9(9) USAGE COMP.
           10 JUR-LAST-UPDATE      PIC X(26).
           10 JUR-DAT-VON          PIC X(10).
           10 JUR-DAT-BIS          PIC X(10).
           10 JUR-PROV-STZ-AP      PIC S9(3)V99 USAGE COMP-3.
           10 JUR-PROV-STZ-GP      PIC S9(3)V99 USAGE COMP-3.
220297     10 JUR-PROZ-STZ         PIC S9(3)V99 USAGE COMP-3.
           10 JUR-RCK-STZ-DAT      PIC X(10).
           10 JUR-RCK-STZ          PIC S9(3)V99 USAGE COMP-3.
           10 KTY-VERSAND          PIC S9(4) USAGE COMP.
           10 KNR-VERSAND          PIC S9(4) USAGE COMP.
           10 JUR-BEZ              PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************

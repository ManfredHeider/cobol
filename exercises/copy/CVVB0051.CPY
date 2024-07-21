      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0051.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0051.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:21:32   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0051)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0051))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0051 TABLE
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
      * COBOL DECLARATION FOR TABLE VIVB0051                           *
      ******************************************************************
       01  VIVB0051-GESAMT.
        05  NI-VIVB0051-TAB.
           10  NI-VIVB0051-ELE     PIC S9(04)  COMP   OCCURS  12 TIMES.
        05  NI-VIVB0051            REDEFINES   NI-VIVB0051-TAB.
           10 NI-JUR-ST-NR         PIC S9(04)  COMP.
           10 NI-JUR-LAST-UPDATE   PIC S9(04)  COMP.
           10 NI-JUR-DAT-VON       PIC S9(04)  COMP.
           10 NI-JUR-DAT-BIS       PIC S9(04)  COMP.
           10 NI-JUR-PROV-STZ-AP   PIC S9(04)  COMP.
           10 NI-JUR-PROV-STZ-GP   PIC S9(04)  COMP.
           10 NI-JUR-PROZ-STZ      PIC S9(04)  COMP.
           10 NI-JUR-RCK-STZ-DAT   PIC S9(04)  COMP.
           10 NI-JUR-RCK-STZ       PIC S9(04)  COMP.
           10 NI-KTY-VERSAND       PIC S9(04)  COMP.
           10 NI-KNR-VERSAND       PIC S9(04)  COMP.
           10 NI-JUR-BEZ           PIC S9(04)  COMP.
        05  DCL-VIVB0051.
           10 JUR-ST-NR            PIC S9(9) USAGE COMP.
           10 JUR-LAST-UPDATE      PIC X(26).
           10 JUR-DAT-VON          PIC X(10).
           10 JUR-DAT-BIS          PIC X(10).
           10 JUR-PROV-STZ-AP      PIC S9(3)V99 USAGE COMP-3.
           10 JUR-PROV-STZ-GP      PIC S9(3)V99 USAGE COMP-3.
           10 JUR-PROZ-STZ         PIC S9(3)V99 USAGE COMP-3.
           10 JUR-RCK-STZ-DAT      PIC X(10).
           10 JUR-RCK-STZ          PIC S9(3)V99 USAGE COMP-3.
           10 KTY-VERSAND          PIC S9(4) USAGE COMP.
           10 KNR-VERSAND          PIC S9(4) USAGE COMP.
           10 JUR-BEZ              PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************

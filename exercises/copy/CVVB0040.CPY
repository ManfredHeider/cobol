      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0040.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0040.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:19:44   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0040)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0040))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0040 TABLE
CCC004*    ( KL_ST_NR                       INTEGER NOT NULL,
CCC004*      KL_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      KL_DAT_VON                     DATE NOT NULL,
CCC004*      KL_DAT_BIS                     DATE,
CCC004*      KL_PROV_STZ_AP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROV_STZ_GP                 DECIMAL(5, 2) NOT NULL,
CCC004*      KL_PROZ_STZ                    DECIMAL(5, 2) NOT NULL,
CCC004*      KL_RCK_STZ_DAT                 DATE NOT NULL,
CCC004*      KL_RCK_STZ                     DECIMAL(5, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0040                           *
      ******************************************************************
       01  VIVB0040-GESAMT.
        05  NI-VIVB0040-TAB.
           10  NI-VIVB0040-ELE     PIC S9(04)  COMP   OCCURS  9  TIMES.
        05  NI-VIVB0040            REDEFINES   NI-VIVB0040-TAB.
           10 NI-KL-ST-NR          PIC S9(04)  COMP.
           10 NI-KL-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-KL-DAT-VON        PIC S9(04)  COMP.
           10 NI-KL-DAT-BIS        PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-AP    PIC S9(04)  COMP.
           10 NI-KL-PROV-STZ-GP    PIC S9(04)  COMP.
           10 NI-KL-PROZ-STZ       PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ-DAT    PIC S9(04)  COMP.
           10 NI-KL-RCK-STZ        PIC S9(04)  COMP.
        05  DCL-VIVB0040.
           10 KL-ST-NR             PIC S9(9) USAGE COMP.
           10 KL-LAST-UPDATE       PIC X(26).
           10 KL-DAT-VON           PIC X(10).
           10 KL-DAT-BIS           PIC X(10).
           10 KL-PROV-STZ-AP       PIC S999V99 USAGE COMP-3.
           10 KL-PROV-STZ-GP       PIC S999V99 USAGE COMP-3.
           10 KL-PROZ-STZ          PIC S999V99 USAGE COMP-3.
           10 KL-RCK-STZ-DAT       PIC X(10).
           10 KL-RCK-STZ           PIC S999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************

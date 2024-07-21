      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0021.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0021.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:15:34   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0021)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0021))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0021 TABLE
CCC004*    ( ST_NR                          INTEGER NOT NULL,
CCC004*      KTY_BV_TYP                     SMALLINT NOT NULL,
CCC004*      KNR_BV_TYP                     SMALLINT NOT NULL,
CCC004*      BV_DAT_VON                     TIMESTAMP NOT NULL,
CCC004*      BV_LAST_UPDATE                 TIMESTAMP NOT NULL,
CCC004*      BV_DAT_BIS                     DATE NOT NULL,
CCC004*      KTY_SPERRE                     SMALLINT NOT NULL,
CCC004*      KNR_SPERRE                     SMALLINT NOT NULL,
CCC004*      BV_KTO_NR                      CHAR(20) NOT NULL,
CCC004*      BV_BLZ                         CHAR(11) NOT NULL,
CCC004*      BV_BANKNAME                    CHAR(40) NOT NULL,
CCC004*      BV_KTO_INHABER                 CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0021                           *
      ******************************************************************
       01  VIVB0021-GESAMT.
        05  NI-VIVB0021-TAB.
           10  NI-VIVB0021-ELE     PIC S9(04)  COMP   OCCURS  12 TIMES.
        05  NI-VIVB0021            REDEFINES   NI-VIVB0021-TAB.
           10 NI-ST-NR             PIC S9(04)  COMP.
           10 NI-KTY-BV-TYP        PIC S9(04)  COMP.
           10 NI-KNR-BV-TYP        PIC S9(04)  COMP.
           10 NI-BV-DAT-VON        PIC S9(04)  COMP.
           10 NI-BV-LAST-UPDATE    PIC S9(04)  COMP.
           10 NI-BV-DAT-BIS        PIC S9(04)  COMP.
           10 NI-KTY-SPERRE        PIC S9(04)  COMP.
           10 NI-KNR-SPERRE        PIC S9(04)  COMP.
           10 NI-BV-KTO-NR         PIC S9(04)  COMP.
           10 NI-BV-BLZ            PIC S9(04)  COMP.
           10 NI-BV-BANKNAME       PIC S9(04)  COMP.
           10 NI-BV-KTO-INHABER    PIC S9(04)  COMP.
        05  DCL-VIVB0021.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KTY-BV-TYP           PIC S9(4) USAGE COMP.
           10 KNR-BV-TYP           PIC S9(4) USAGE COMP.
           10 BV-DAT-VON           PIC X(26).
           10 BV-LAST-UPDATE       PIC X(26).
           10 BV-DAT-BIS           PIC X(10).
           10 KTY-SPERRE           PIC S9(4) USAGE COMP.
           10 KNR-SPERRE           PIC S9(4) USAGE COMP.
           10 BV-KTO-NR            PIC X(20).
           10 BV-BLZ               PIC X(11).
           10 BV-BANKNAME          PIC X(40).
           10 BV-KTO-INHABER       PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************

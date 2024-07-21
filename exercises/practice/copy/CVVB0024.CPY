      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0024.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0024.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:16:10   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0024)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0024))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0024 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      VSWZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      VSWZ_ZUSAGE_DAT                DATE NOT NULL,
CCC004*      VSWZ_A_IG_DAT                  DATE,
CCC004*      VSWZ_A_GESAMT                  DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_G_DAT_VON                 DATE,
CCC004*      VSWZ_G_STZ_JAHR                DATE,
CCC004*      VSWZ_G_VERS_SUM                DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_G_STZ_ANZ                 SMALLINT NOT NULL,
CCC004*      VSWZ_G_VERS_NR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VSWZ_Z_VERS_SUM                DECIMAL(11, 2) NOT NULL,
CCC004*      VSWZ_Z_VERS_NR                 DECIMAL(15, 0) NOT NULL,
CCC004*      VSWZ_Z_DAT_VON                 DATE,
CCC004*      KTY_A_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KNR_A_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KTY_G_BERECHTIG                SMALLINT NOT NULL,
CCC004*      KNR_G_BERECHTIG                SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0024                           *
      ******************************************************************
       01  VIVB0024-GESAMT.
        05  NI-VIVB0024-TAB.
           10  NI-VIVB0024-ELE     PIC S9(04)  COMP   OCCURS  17 TIMES.
        05  NI-VIVB0024            REDEFINES   NI-VIVB0024-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-VSWZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-VSWZ-ZUSAGE-DAT   PIC S9(04)  COMP.
           10 NI-VSWZ-A-IG-DAT     PIC S9(04)  COMP.
           10 NI-VSWZ-A-GESAMT     PIC S9(04)  COMP.
           10 NI-VSWZ-G-DAT-VON    PIC S9(04)  COMP.
           10 NI-VSWZ-G-STZ-JAHR   PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERS-SUM   PIC S9(04)  COMP.
           10 NI-VSWZ-G-STZ-ANZ    PIC S9(04)  COMP.
           10 NI-VSWZ-G-VERS-NR    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERS-SUM   PIC S9(04)  COMP.
           10 NI-VSWZ-Z-VERS-NR    PIC S9(04)  COMP.
           10 NI-VSWZ-Z-DAT-VON    PIC S9(04)  COMP.
           10 NI-KTY-A-BERECHTIG   PIC S9(04)  COMP.
           10 NI-KNR-A-BERECHTIG   PIC S9(04)  COMP.
           10 NI-KTY-G-BERECHTIG   PIC S9(04)  COMP.
           10 NI-KNR-G-BERECHTIG   PIC S9(04)  COMP.
        05  DCL-VIVB0024.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 VSWZ-LAST-UPDATE     PIC X(26).
           10 VSWZ-ZUSAGE-DAT      PIC X(10).
           10 VSWZ-A-IG-DAT        PIC X(10).
           10 VSWZ-A-GESAMT        PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-G-DAT-VON       PIC X(10).
           10 VSWZ-G-STZ-JAHR      PIC X(10).
           10 VSWZ-G-VERS-SUM      PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-G-STZ-ANZ       PIC S9(4) USAGE COMP.
           10 VSWZ-G-VERS-NR       PIC S999999999999999V USAGE COMP-3.
           10 VSWZ-Z-VERS-SUM      PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-Z-VERS-NR       PIC S999999999999999V USAGE COMP-3.
           10 VSWZ-Z-DAT-VON       PIC X(10).
           10 KTY-A-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KNR-A-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KTY-G-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KNR-G-BERECHTIG      PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 17      *
      ******************************************************************

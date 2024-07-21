      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0015.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0015.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:28   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:14:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0015)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0015))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0015 TABLE
CCC004*    ( MAAN_BEZUGS_NR                 INTEGER NOT NULL,
CCC004*      KTY_MAAN_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_MAAN_TYP                   SMALLINT NOT NULL,
CCC004*      MAAN_DAT_VON                   TIMESTAMP NOT NULL,
CCC004*      MAAN_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      MAAN_STRASSE                   CHAR(40) NOT NULL,
CCC004*      MAAN_PLZ_AKTUELL               CHAR(9) NOT NULL,
CCC004*      MAAN_PLZ                       CHAR(9) NOT NULL,
CCC004*      MAAN_ORT                       CHAR(40) NOT NULL,
CCC004*      MAAN_PLZ_PF                    CHAR(9) NOT NULL,
CCC004*      MAAN_ORT_PF                    CHAR(40) NOT NULL,
CCC004*      MAAN_POSTFACH                  CHAR(6) NOT NULL,
CCC004*      MAAN_WOHNT_BEI                 CHAR(40) NOT NULL,
CCC004*      MAAN_DAT_BIS                   DATE NOT NULL,
CCC004*      MAAN_TELEFON                   CHAR(14) NOT NULL,
CCC004*      MAAN_TELEFAX                   CHAR(14) NOT NULL,
CCC004*      KTY_AUSL                       SMALLINT NOT NULL,
CCC004*      KNR_AUSL                       SMALLINT NOT NULL,
CCC004*      KTY_GRND                       SMALLINT NOT NULL,
CCC004*      KNR_GRND                       SMALLINT NOT NULL,
CCC004*      KTY_ZUSTELLUNG                 SMALLINT NOT NULL,
CCC004*      KNR_ZUSTELLUNG                 SMALLINT NOT NULL,
CCC004*      KTY_BEZUGS_TYP                 SMALLINT NOT NULL,
CCC004*      KNR_BEZUGS_TYP                 SMALLINT NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0015                           *
      ******************************************************************
       01  VIVB0015-GESAMT.
        05  NI-VIVB0015-TAB.
           10  NI-VIVB0015-ELE     PIC S9(04)  COMP   OCCURS  24 TIMES.
        05  NI-VIVB0015            REDEFINES   NI-VIVB0015-TAB.
           10 NI-MAAN-BEZUGS-NR    PIC S9(04)  COMP.
           10 NI-KTY-MAAN-TYP      PIC S9(04)  COMP.
           10 NI-KNR-MAAN-TYP      PIC S9(04)  COMP.
           10 NI-MAAN-DAT-VON      PIC S9(04)  COMP.
           10 NI-MAAN-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-MAAN-STRASSE      PIC S9(04)  COMP.
           10 NI-MAAN-PLZ-AKTUELL  PIC S9(04)  COMP.
           10 NI-MAAN-PLZ          PIC S9(04)  COMP.
           10 NI-MAAN-ORT          PIC S9(04)  COMP.
           10 NI-MAAN-PLZ-PF       PIC S9(04)  COMP.
           10 NI-MAAN-ORT-PF       PIC S9(04)  COMP.
           10 NI-MAAN-POSTFACH     PIC S9(04)  COMP.
           10 NI-MAAN-WOHNT-BEI    PIC S9(04)  COMP.
           10 NI-MAAN-DAT-BIS      PIC S9(04)  COMP.
           10 NI-MAAN-TELEFON      PIC S9(04)  COMP.
           10 NI-MAAN-TELEFAX      PIC S9(04)  COMP.
           10 NI-KTY-AUSL          PIC S9(04)  COMP.
           10 NI-KNR-AUSL          PIC S9(04)  COMP.
           10 NI-KTY-GRND          PIC S9(04)  COMP.
           10 NI-KNR-GRND          PIC S9(04)  COMP.
           10 NI-KTY-ZUSTELLUNG    PIC S9(04)  COMP.
           10 NI-KNR-ZUSTELLUNG    PIC S9(04)  COMP.
           10 NI-KTY-BEZUGS-TYP    PIC S9(04)  COMP.
           10 NI-KNR-BEZUGS-TYP    PIC S9(04)  COMP.
        05  DCL-VIVB0015.
           10 MAAN-BEZUGS-NR       PIC S9(9) USAGE COMP.
           10 KTY-MAAN-TYP         PIC S9(4) USAGE COMP.
           10 KNR-MAAN-TYP         PIC S9(4) USAGE COMP.
           10 MAAN-DAT-VON         PIC X(26).
           10 MAAN-LAST-UPDATE     PIC X(26).
           10 MAAN-STRASSE         PIC X(40).
           10 MAAN-PLZ-AKTUELL     PIC X(9).
           10 MAAN-PLZ             PIC X(9).
           10 MAAN-ORT             PIC X(40).
           10 MAAN-PLZ-PF          PIC X(9).
           10 MAAN-ORT-PF          PIC X(40).
           10 MAAN-POSTFACH        PIC X(6).
           10 MAAN-WOHNT-BEI       PIC X(40).
           10 MAAN-DAT-BIS         PIC X(10).
           10 MAAN-TELEFON         PIC X(14).
           10 MAAN-TELEFAX         PIC X(14).
           10 KTY-AUSL             PIC S9(4) USAGE COMP.
           10 KNR-AUSL             PIC S9(4) USAGE COMP.
           10 KTY-GRND             PIC S9(4) USAGE COMP.
           10 KNR-GRND             PIC S9(4) USAGE COMP.
           10 KTY-ZUSTELLUNG       PIC S9(4) USAGE COMP.
           10 KNR-ZUSTELLUNG       PIC S9(4) USAGE COMP.
           10 KTY-BEZUGS-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEZUGS-TYP       PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 22      *
      ******************************************************************

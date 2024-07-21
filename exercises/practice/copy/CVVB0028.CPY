      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0028.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\CVVB0028.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:56:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 12:17:04   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE(VIVB0028)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0028))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0028 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KTY_SEMZ_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_SEMZ_TYP                   SMALLINT NOT NULL,
CCC004*      SEMZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      SEMZ_DAT_VON                   DATE NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0028                           *
      ******************************************************************
       01  VIVB0028-GESAMT.
        05  NI-VIVB0028-TAB.
           10  NI-VIVB0028-ELE     PIC S9(04)  COMP   OCCURS  5  TIMES.
        05  NI-VIVB0028            REDEFINES   NI-VIVB0028-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-KTY-SEMZ-TYP      PIC S9(04)  COMP.
           10 NI-KNR-SEMZ-TYP      PIC S9(04)  COMP.
           10 NI-SEMZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-SEMZ-DAT-VON      PIC S9(04)  COMP.
        05  DCL-VIVB0028.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KTY-SEMZ-TYP         PIC S9(4) USAGE COMP.
           10 KNR-SEMZ-TYP         PIC S9(4) USAGE COMP.
           10 SEMZ-LAST-UPDATE     PIC X(26).
           10 SEMZ-DAT-VON         PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************

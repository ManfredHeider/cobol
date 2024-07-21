      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   cvvba028.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\cvvba028.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:59:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Feb 15 1996 17:11:48   T13AE26
      *Versionszeile eingefügt
      *
      *   Rev 1.0   Feb 15 1996 09:40:34   T13AE24
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    VERSION                                                     *
      ******************************************************************
      * DCLGEN TABLE(VIVBA028)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA028))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVBA028 TABLE
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,
CCC004*      KTY_SEMZ_TYP                   SMALLINT NOT NULL,
CCC004*      KNR_SEMZ_TYP                   SMALLINT NOT NULL,
CCC004*      SEMZ_LAST_UPDATE               TIMESTAMP NOT NULL,
CCC004*      SEMZ_DAT_VON                   DATE NOT NULL,
CCC004*      SEMZ_TEILNAHME                 CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA028                           *
      ******************************************************************
       01  VIVBA028-GESAMT.
        05  NI-VIVBA028-TAB.
           10  NI-VIVBA028-ELE     PIC S9(04)  COMP   OCCURS  6  TIMES.
        05  NI-VIVBA028            REDEFINES   NI-VIVBA028-TAB.
           10 NI-MA-ST-NR          PIC S9(04)  COMP.
           10 NI-KTY-SEMZ-TYP      PIC S9(04)  COMP.
           10 NI-KNR-SEMZ-TYP      PIC S9(04)  COMP.
           10 NI-SEMZ-LAST-UPDATE  PIC S9(04)  COMP.
           10 NI-SEMZ-DAT-VON      PIC S9(04)  COMP.
           10 NI-SEMZ-TEILNAHME    PIC S9(04)  COMP.
        05  DCL-VIVBA028.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 KTY-SEMZ-TYP         PIC S9(4) USAGE COMP.
           10 KNR-SEMZ-TYP         PIC S9(4) USAGE COMP.
           10 SEMZ-LAST-UPDATE     PIC X(26).
           10 SEMZ-DAT-VON         PIC X(10).
           10 SEMZ-TEILNAHME       PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************

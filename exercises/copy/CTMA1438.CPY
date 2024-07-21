      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1438.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1438.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:18:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1438-T
CCC005*    EXEC SQL DECLARE TBOVSLM TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,DAT_VON              DATE                 NOT NULL
CCC004*     ,KNR_SLM_TYP          SMALLINT             NOT NULL
CCC004*     ,KNR_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,LAST_UPDATE          TIMESTAMP            NOT NULL
CCC004*     ,KTY_SLM_TYP          SMALLINT             NOT NULL
CCC004*     ,KTY_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,AUSZAHLUNG           DECIMAL(11, 2)       NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1438-T
        01 TBOVSLM-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 DAT-VON                           PIC X(10).
         10 KNR-SLM-TYP                       PIC S9(4) COMP.
         10 KNR-WAEHRUNG                      PIC S9(4) COMP.
         10 LAST-UPDATE                       PIC X(26).
         10 KTY-SLM-TYP                       PIC S9(4) COMP.
         10 KTY-WAEHRUNG                      PIC S9(4) COMP.
         10 AUSZAHLUNG                        PIC S9(9)V9(2) COMP-3.

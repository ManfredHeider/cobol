      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1434.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1434.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:17:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1434-T
CCC005*    EXEC SQL DECLARE TBOVKTO TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,MONAT                DATE                 NOT NULL
CCC004*     ,KNR_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,KNR_KTO_STAND        SMALLINT             NOT NULL
CCC004*     ,KTY_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,KTY_KTO_STAND        SMALLINT             NOT NULL
CCC004*     ,STAND_DISK           DECIMAL(11, 2)       NOT NULL
CCC004*     ,STAND_VORSCH         DECIMAL(11, 2)       NOT NULL
CCC004*     ,STAND_RUECK          DECIMAL(11, 2)       NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1434-T
        01 TBOVKTO-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 MONAT                             PIC X(10).
         10 KNR-WAEHRUNG                      PIC S9(4) COMP.
         10 KNR-KTO-STAND                     PIC S9(4) COMP.
         10 KTY-WAEHRUNG                      PIC S9(4) COMP.
         10 KTY-KTO-STAND                     PIC S9(4) COMP.
         10 STAND-DISK                        PIC S9(9)V9(2) COMP-3.
         10 STAND-VORSCH                      PIC S9(9)V9(2) COMP-3.
         10 STAND-RUECK                       PIC S9(9)V9(2) COMP-3.

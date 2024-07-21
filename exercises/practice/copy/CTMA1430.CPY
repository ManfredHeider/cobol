      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA1430.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA1430.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:17:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV1430-T
CCC005*    EXEC SQL DECLARE TBOVEV TABLE
CCC004*     (ST_NR                INTEGER              NOT NULL
CCC004*     ,MONAT                DATE                 NOT NULL
CCC004*     ,KNR_VERGUET_TYP      SMALLINT             NOT NULL
CCC004*     ,KNR_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,KTY_VERGUET_TYP      SMALLINT             NOT NULL
CCC004*     ,KTY_WAEHRUNG         SMALLINT             NOT NULL
CCC004*     ,EQF                  DECIMAL(5, 2)        NOT NULL
CCC004*     ,STORMIND_PROZ        DECIMAL(5, 2)        NOT NULL
CCC004*     ,AE_MONATS_DS         DECIMAL(10, 1)       NOT NULL
CCC004*     ,BRUTTO_BETRAG        DECIMAL(11, 2)       NOT NULL
CCC004*     ,RUECKBELASTG         DECIMAL(11, 2)       NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV1430-T
        01 TBOVEV-SATZ.
         10 ST-NR                             PIC S9(9) COMP.
         10 MONAT                             PIC X(10).
         10 KNR-VERGUET-TYP                   PIC S9(4) COMP.
         10 KNR-WAEHRUNG                      PIC S9(4) COMP.
         10 KTY-VERGUET-TYP                   PIC S9(4) COMP.
         10 KTY-WAEHRUNG                      PIC S9(4) COMP.
         10 EQF                               PIC S9(3)V9(2) COMP-3.
         10 STORMIND-PROZ                     PIC S9(3)V9(2) COMP-3.
         10 AE-MONATS-DS                      PIC S9(9)V9(1) COMP-3.
         10 BRUTTO-BETRAG                     PIC S9(9)V9(2) COMP-3.
         10 RUECKBELASTG                      PIC S9(9)V9(2) COMP-3.

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3610.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3610.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:20:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3610
CCC005*    EXEC SQL DECLARE OVANALYSEDATEN00 TABLE
CCC004*     (ANAL_LFDNR           SMALLINT             NOT NULL
CCC004*     ,SEL_DE_VG_WERT       SMALLINT             NOT NULL
CCC004*     ,SEL_DE_DAT_WERT      DATE                 NOT NULL
CCC004*     ,SEL_DE_1_WERT        CHAR(15)             NOT NULL
CCC004*     ,SEL_DE_2_WERT        CHAR(15)             NOT NULL
CCC004*     ,SEL_DE_3_WERT        CHAR(15)             NOT NULL
CCC004*     ,SEL_DE_4_WERT        CHAR(15)             NOT NULL
CCC004*     ,ERG_DE_0_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_1_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_2_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_3_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_4_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_5_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_6_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_7_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_8_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,ERG_DE_9_WERT        DECIMAL(17, 2)       NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3610
        01 OVANALYSEDATEN00-SATZ.
         10 ANAL-LFDNR                        PIC S9(4) COMP.
         10 SEL-DE-VG-WERT                    PIC S9(4) COMP.
         10 SEL-DE-DAT-WERT                   PIC X(10).
         10 SEL-DE-1-WERT                     PIC X(15).
         10 SEL-DE-2-WERT                     PIC X(15).
         10 SEL-DE-3-WERT                     PIC X(15).
         10 SEL-DE-4-WERT                     PIC X(15).
         10 ERG-DE-0-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-1-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-2-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-3-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-4-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-5-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-6-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-7-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-8-WERT                     PIC S9(15)V9(2) COMP-3.
         10 ERG-DE-9-WERT                     PIC S9(15)V9(2) COMP-3.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

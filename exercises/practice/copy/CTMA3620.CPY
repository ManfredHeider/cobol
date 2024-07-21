      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3620.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3620.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:21:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3620
CCC005*    EXEC SQL DECLARE OVAUSWERTUNGVB00 TABLE
CCC004*     (AUSW_LFDNR           SMALLINT             NOT NULL
CCC004*     ,ANAL_LFDNR           SMALLINT             NOT NULL
CCC004*     ,GUELT_AB             DATE                 NOT NULL
CCC004*     ,GUELT_BIS            DATE                 NOT NULL
CCC004*     ,SEL_DE_VG_LFDNR      SMALLINT             NOT NULL
CCC004*     ,SEL_DE_DAT_LFDNR     SMALLINT             NOT NULL
CCC004*     ,SEL_DE_VBNR_LFDNR    SMALLINT             NOT NULL
CCC004*     ,SEL_DE_DIR_LFDNR     SMALLINT             NOT NULL
CCC004*     ,SEL_DE_STUFE_LFDNR   SMALLINT             NOT NULL
CCC004*     ,SEL_DE_1_LFDNR       SMALLINT             NOT NULL
CCC004*     ,SEL_DE_2_LFDNR       SMALLINT             NOT NULL
CCC004*     ,SEL_DE_3_LFDNR       SMALLINT             NOT NULL
CCC004*     ,SEL_DE_4_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_0_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_1_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_2_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_3_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_4_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_5_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_6_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_7_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_8_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_9_LFDNR       SMALLINT             NOT NULL
CCC004*     ,ERG_DE_10_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_11_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_12_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_13_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_14_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_15_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_16_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_17_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_18_LFDNR      SMALLINT             NOT NULL
CCC004*     ,ERG_DE_19_LFDNR      SMALLINT             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3620
        01 OVAUSWERTUNGVB00-SATZ.
         10 AUSW-LFDNR                        PIC S9(4) COMP.
         10 ANAL-LFDNR                        PIC S9(4) COMP.
         10 GUELT-AB                          PIC X(10).
         10 GUELT-BIS                         PIC X(10).
         10 SEL-DE-VG-LFDNR                   PIC S9(4) COMP.
         10 SEL-DE-DAT-LFDNR                  PIC S9(4) COMP.
         10 SEL-DE-VBNR-LFDNR                 PIC S9(4) COMP.
         10 SEL-DE-DIR-LFDNR                  PIC S9(4) COMP.
         10 SEL-DE-STUFE-LFDNR                PIC S9(4) COMP.
         10 SEL-DE-1-LFDNR                    PIC S9(4) COMP.
         10 SEL-DE-2-LFDNR                    PIC S9(4) COMP.
         10 SEL-DE-3-LFDNR                    PIC S9(4) COMP.
         10 SEL-DE-4-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-0-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-1-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-2-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-3-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-4-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-5-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-6-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-7-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-8-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-9-LFDNR                    PIC S9(4) COMP.
         10 ERG-DE-10-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-11-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-12-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-13-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-14-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-15-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-16-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-17-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-18-LFDNR                   PIC S9(4) COMP.
         10 ERG-DE-19-LFDNR                   PIC S9(4) COMP.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

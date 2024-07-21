      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3646.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3646.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:23:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3646
CCC005*    EXEC SQL DECLARE OVNOTIZ00 TABLE
CCC004*     (NOTIZ_ID             DECIMAL(15)          NOT NULL
CCC004*     ,USERID               CHAR(8)              NOT NULL
CCC004*     ,ERSTELL_DAT          DATE                 NOT NULL
CCC004*     ,SEL_DE_VG_WERT       SMALLINT             NOT NULL
CCC004*     ,SEL_DE_DAT_WERT      DATE                 NOT NULL
CCC004*     ,DE_LFDNR             SMALLINT             NOT NULL
CCC004*     ,SGWT_LFDNR           SMALLINT             NOT NULL
CCC004*     ,NOAR_LFDNR           SMALLINT             NOT NULL
CCC004*     ,NOZY_LFDNR           SMALLINT             NOT NULL
CCC004*     ,NOTIZ_BESCHR         CHAR(20)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3646
        01 OVNOTIZ00-SATZ.
         10 NOTIZ-ID                          PIC S9(15) COMP-3.
         10 USERID                            PIC X(8).
         10 ERSTELL-DAT                       PIC X(10).
         10 SEL-DE-VG-WERT                    PIC S9(4) COMP.
         10 SEL-DE-DAT-WERT                   PIC X(10).
         10 DE-LFDNR                          PIC S9(4) COMP.
         10 SGWT-LFDNR                        PIC S9(4) COMP.
         10 NOAR-LFDNR                        PIC S9(4) COMP.
         10 NOZY-LFDNR                        PIC S9(4) COMP.
         10 NOTIZ-BESCHR                      PIC X(20).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

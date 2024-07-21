      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3640.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3640.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:22:44   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3640
CCC005*    EXEC SQL DECLARE OVDRUCK00 TABLE
CCC004*     (DRUCK_ID             DECIMAL(15)          NOT NULL
CCC004*     ,AUSW_LFDNR           SMALLINT             NOT NULL
CCC004*     ,AUDR_LFDNR           SMALLINT             NOT NULL
CCC004*     ,USERID               CHAR(8)              NOT NULL
CCC004*     ,DRCK_LFDNR           SMALLINT             NOT NULL
CCC004*     ,HALT_KZN             CHAR(1)              NOT NULL
CCC004*     ,ERSTELL_DAT          DATE                 NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3640
        01 OVDRUCK00-SATZ.
         10 DRUCK-ID                          PIC S9(15) COMP-3.
         10 AUSW-LFDNR                        PIC S9(4) COMP.
         10 AUDR-LFDNR                        PIC S9(4) COMP.
         10 USERID                            PIC X(8).
         10 DRCK-LFDNR                        PIC S9(4) COMP.
         10 HALT-KZN                          PIC X(1).
         10 ERSTELL-DAT                       PIC X(10).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

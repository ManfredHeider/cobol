      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3629.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3629.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:22:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3629.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/COBOL/CTMA3629.CBv  $
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3629
CCC005*    EXEC SQL DECLARE OVBEZEICH_VERKN00 TABLE
CCC004*     (BEZEICH_GRP_ID_1     SMALLINT             NOT NULL
CCC004*     ,BEZEICH_LFDNR_1      SMALLINT             NOT NULL
CCC004*     ,BEZEICH_GRP_ID_2     SMALLINT             NOT NULL
CCC004*     ,BEZEICH_LFDNR_2      SMALLINT             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3629
        01 OVBEZEICH-VERKN00-SATZ.
         10 BEZEICH-GRP-ID-1                  PIC S9(4) COMP.
         10 BEZEICH-LFDNR-1                   PIC S9(4) COMP.
         10 BEZEICH-GRP-ID-2                  PIC S9(4) COMP.
         10 BEZEICH-LFDNR-2                   PIC S9(4) COMP.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

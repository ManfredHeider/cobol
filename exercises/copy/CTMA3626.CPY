      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3626.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3626.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:21:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3626
CCC005*    EXEC SQL DECLARE OVBEZEICH00 TABLE
CCC004*     (BEZEICH_GRP_ID       SMALLINT             NOT NULL
CCC004*     ,BEZEICH_LFDNR        SMALLINT             NOT NULL
CCC004*     ,BEZEICH_KURZ         CHAR(4)              NOT NULL
CCC004*     ,BEZEICH_MITTEL       CHAR(10)             NOT NULL
CCC004*     ,BEZEICH_LANG         CHAR(40)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3626
        01 OVBEZEICH00-SATZ.
         10 BEZEICH-GRP-ID                    PIC S9(4) COMP.
         10 BEZEICH-LFDNR                     PIC S9(4) COMP.
         10 BEZEICH-KURZ                      PIC X(4).
         10 BEZEICH-MITTEL                    PIC X(10).
         10 BEZEICH-LANG                      PIC X(40).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

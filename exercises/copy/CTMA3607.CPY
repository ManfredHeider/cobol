      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3607.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3607.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:20:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3607
CCC005*    EXEC SQL DECLARE OVWORK100 TABLE
CCC004*     (USERID               CHAR(8)              NOT NULL
CCC004*     ,TRANS                CHAR(8)              NOT NULL
CCC004*     ,SEQUENZNUMMER        SMALLINT             NOT NULL
CCC004*     ,ENDE_INDIKATOR       SMALLINT             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL
CCC004*     ,DATENBEREICH         VARCHAR(3822)        NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3607
        01 OVWORK100-SATZ.
         10 USERID                            PIC X(8).
         10 TRANS                             PIC X(8).
         10 SEQUENZNUMMER                     PIC S9(4) COMP.
         10 ENDE-INDIKATOR                    PIC S9(4) COMP.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
         10 DATENBEREICH.
           49 DATENBEREICH-LEN                PIC S9(4) COMP.
           49 DATENBEREICH-TEXT               PIC X(3822).

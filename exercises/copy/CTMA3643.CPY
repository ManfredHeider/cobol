      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3643.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3643.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:22:56   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3643
CCC005*    EXEC SQL DECLARE OVDRUCKZEILE00 TABLE
CCC004*     (DRUCK_ID             DECIMAL(15)          NOT NULL
CCC004*     ,DRUCK_LFDNR          SMALLINT             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL
CCC004*     ,DRUCK_ZEILE          VARCHAR(1000)        NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3643
        01 OVDRUCKZEILE00-SATZ.
         10 DRUCK-ID                          PIC S9(15) COMP-3.
         10 DRUCK-LFDNR                       PIC S9(4) COMP.
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).
         10 DRUCK-ZEILE.
           49 DRUCK-ZEILE-LEN                 PIC S9(4) COMP.
           49 DRUCK-ZEILE-TEXT                PIC X(1000).

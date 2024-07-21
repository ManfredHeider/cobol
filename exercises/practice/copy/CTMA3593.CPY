      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTMA3593.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTMA3593.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 11:19:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * GENERATION OF SQL DECLARE FROM MEMBER TB-OV3593
CCC005*    EXEC SQL DECLARE OVBUCHDAT00 TABLE
CCC004*     (VERSION              CHAR(1)              NOT NULL
CCC004*     ,VG                   SMALLINT             NOT NULL
CCC004*     ,VERARB_KZN           CHAR(1)              NOT NULL
CCC004*     ,FACH_KENNUNG         CHAR(10)             NOT NULL
CCC004*     ,BUCH_DAT             DATE                 NOT NULL
CCC004*     ,DV_KENNUNG           CHAR(10)             NOT NULL
CCC004*     ,AENDERUNGSSYSZEIT    TIMESTAMP            NOT NULL
CCC004*     ,AENDERUNGSKENNUNG    CHAR(8)              NOT NULL)
CCC003*    END-EXEC.
      * GENERATION OF COBOL HOST STRUCTURE FROM TB-OV3593
        01 OVBUCHDAT00-SATZ.
         10 VERSION                           PIC X(1).
         10 VG                                PIC S9(4) COMP.
         10 VERARB-KZN                        PIC X(1).
         10 FACH-KENNUNG                      PIC X(10).
         10 BUCH-DAT                          PIC X(10).
         10 DV-KENNUNG                        PIC X(10).
         10 AENDERUNGSSYSZEIT                 PIC X(26).
         10 AENDERUNGSKENNUNG                 PIC X(8).

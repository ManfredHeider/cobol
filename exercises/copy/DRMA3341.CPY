      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3341.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3341.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:46:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVADRESSEN00
      *
      *     SOURCE-NAME      :  DRMA3341
      *     DMR-NAME         :  DG-OV3341
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  14.05.08/14:37
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 ADRESSEN.
              07 VERSION                PIC X.
              07 VG                     PIC S9(4)      BINARY.
              07 VBNR                   PIC S9(9)      BINARY.
              07 HIST-KZN               PIC X.
              07 GUELTIG-AB             PIC X(10).
              07 GUELTIG-BIS            PIC X(10).
              07 ANREDE                 PIC X.
              07 ZUNAME                 PIC X(40).
              07 VORNAME                PIC X(20).
              07 TITEL-AKAD             PIC X(10).
              07 TITEL-ADEL             PIC X(10).
              07 AUSL-KZ                PIC X(4).
              07 PLZ                    PIC X(9).
              07 ORT                    PIC X(40).
              07 STR                    PIC X(40).
              07 TELEFON-P              PIC X(26).
              07 FAX-NR-P               PIC X(26).
              07 TELEFON-B              PIC X(26).
              07 FAX-NR-B               PIC X(26).
              07 HANDY                  PIC X(26).
              07 EMAIL                  PIC X(83).
              07 URL                    PIC X(83).
              07 AENDERUNGSSYSZEIT      PIC X(26).
              07 AENDERUNGSKENNUNG      PIC X(8).
      *
      **************************************************************

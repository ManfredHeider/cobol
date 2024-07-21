      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3344.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3344.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 11:47:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA3335 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA3344
      *     DMR-NAME         :  DG-OV3344
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  14.05.08/10:16
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA3344.
              07 FUNKTION               PIC X(8).
              07 ADRESSEN.
                 09 VERSION             PIC X.
                 09 VG                  PIC S9(4)      BINARY.
                 09 VBNR                PIC S9(9)      BINARY.
                 09 HIST-KZN            PIC X.
                 09 GUELTIG-AB          PIC X(10).
                 09 GUELTIG-BIS         PIC X(10).
                 09 ANREDE              PIC X.
                 09 ZUNAME              PIC X(40).
                 09 VORNAME             PIC X(20).
                 09 TITEL-AKAD          PIC X(10).
                 09 TITEL-ADEL          PIC X(10).
                 09 AUSL-KZ             PIC X(4).
                 09 PLZ                 PIC X(9).
                 09 ORT                 PIC X(40).
                 09 STR                 PIC X(40).
                 09 TELEFON-P           PIC X(26).
                 09 FAX-NR-P            PIC X(26).
                 09 TELEFON-B           PIC X(26).
                 09 FAX-NR-B            PIC X(26).
                 09 HANDY               PIC X(26).
                 09 EMAIL               PIC X(83).
                 09 URL                 PIC X(83).
                 09 AENDERUNGSSYSZEIT   PIC X(26).
                 09 AENDERUNGSKENNUNG   PIC X(8).
      *
      **************************************************************

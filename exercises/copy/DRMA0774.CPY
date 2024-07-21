      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0774.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA0774.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:03:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVADRESSEN00
      *
      *     SOURCE-NAME      :  DRMA0774
      *     DMR-NAME         :  DR-OV0774
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  13.05.08/14:09
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA0774.
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

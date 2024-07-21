      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1364.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1364.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 08:52:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1363 - Ermitteln Daten KISS
      *
      *     SOURCE-NAME      :  DRMA1364
      *     DMR-NAME         :  DG-OV1364
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.09.10/10:54
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA1364.
              07 DGOV4047.
                 09 FUNKTION            PIC X(8).
                 09 AKTION              PIC XXX.
                 09 ZEILE-AB            PIC 9(4).
                 09 DGOV4049.
                    11 AUSW-LFDNR       PIC S9(4)      BINARY.
                    11 ANZ-NR           PIC S9(4)      BINARY.
                    11 ANAN-LFDNR       PIC S9(4)      BINARY.
                    11 VGAN-LFDNR       PIC S9(4)      BINARY.
                    11 ANALZEIT-VON     PIC X(6).
                    11 ANALZEIT-BIS     PIC X(6).
                    11 VGLZEIT-VON      PIC X(6).
                    11 VGLZEIT-BIS      PIC X(6).
                    11 VG               PIC S9(4)      BINARY.
                    11 VBNR             PIC S9(9)      BINARY.
                    11 STRUK-DIR        PIC X(4).
                    11 STRUK-STUFE      PIC X(4).
                    11 SEL-DE-1-WERT    PIC X(15).
                    11 SEL-DE-2-WERT    PIC X(15).
                    11 SEL-DE-3-WERT    PIC X(15).
                    11 SEL-DE-4-WERT    PIC X(15).
                    11 ERG-DE-LFDNR1    PIC S9(4)      BINARY.
                    11 ERG-DE-LFDNR2    PIC S9(4)      BINARY.
                    11 ERG-DE-LFDNR3    PIC S9(4)      BINARY.
                    11 ERG-DE-LFDNR4    PIC S9(4)      BINARY.
              07 DGOV0001.
                 09 SICH-TABELLE-1      PIC X(424).
                 09 SICH-TABELLE-2      PIC X(8000).
                 09 SICH-ZW-FELDER      PIC X(238).
                 09 SICH-DRMA1143.
                    11 BUFFER-EINGABE-BEREICH.
                       13 PROGRAMMNAME  PIC X(8).
                       13 LAENGE        PIC S9(4)      BINARY.
                       13 MAXIMALE-ANZAHL
                                        PIC S9(4)      BINARY.
                 09 SICH-DRMA1145.
                    11 STEUER-BEREICH-BLAETTERN.
                       13 USERID        PIC X(8).
                       13 TRANSACTION-CODE
                                        PIC X(8).
                       13 OPERATION     PIC XX.
                       13 GEFORDERTE-ANZAHL
                                        PIC 9(4).
                       13 GELIEFERTE-ANZAHL
                                        PIC 9(4).
                       13 LETZTE-POSITION
                                        PIC 9(4).
                       13 GESAMT-ANZAHL PIC 9(4).
      *
      **************************************************************

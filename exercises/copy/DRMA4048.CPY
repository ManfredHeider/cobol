      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4048.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4048.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 17:19:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Eingabebestand für KISS-Auswertung
      *
      *     SOURCE-NAME      :  DRMA4048
      *     DMR-NAME         :  DR-OV4048
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  23.09.10/10:55
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 DRMA4048.
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
      *
      **************************************************************

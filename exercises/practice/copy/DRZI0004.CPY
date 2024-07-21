      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRZI0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRZI0004.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 16:11:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                             **
      *   Vermögensberater-Ergebnissatz                             **
      *                                                             **
      *   Name ALT:    OM450S                                       **
      *   Name NEU:   DRZI0004                                      **
      *                                                             **
      *   Satzlaenge: 111 - 246 Byte's variabel                     **
      *                                                             **
      *                                                             **
      *   Stand:      28.09.88                                      **
      *                                                             **
      ****************************************************************
           05 VB-ERGEBNIS.
              10 SA              PIC X(6).
              10 LAENGE          PIC S9(4) COMP.
              10 MA-NR           PIC X(10).
              10 SATZ-KZ         PIC X(5).
              10 BUCHDAT         PIC S9(7) PACKED-DECIMAL.
              10 BUCHTEXT        PIC X(40).
              10 BUCHBER-1.
                 15 KTOKZ-1      PIC X.
                 15 GKTO-1       PIC S9(15)   PACKED-DECIMAL.
                 15 BETRAG-1     PIC S9(9)V99 PACKED-DECIMAL.
              10 BUCHBER-2.
                 15 KTOKZ-2      PIC X.
                 15 GKTO-2       PIC S9(15)   PACKED-DECIMAL.
                 15 BETRAG-2     PIC S9(9)V99 PACKED-DECIMAL.
              10 RES-SORT.
                 15 RES-GES      PIC 99.
                 15 RES-PRODSCHL PIC 99.
              10 RES-VSNR        PIC 9(10).
              10 RES-VSNRBFSX REDEFINES RES-VSNR.
                 15 FILLERBFS    PIC 99.
                 15 RES-VSNRBFS  PIC 9(15) PACKED-DECIMAL.
              10 INFO            PIC X(135).
      **                                                           ***
      **             1. Redefinition / Kontroll-Vorsatz            ***
      **                                                           ***
           05 KONTRVOR-BER REDEFINES VB-ERGEBNIS.
              10 KONTR-SA-V         PIC X(6).
              10 KONTR-LAENGE-V     PIC S9(4) COMP.
              10 KONTR-MA-NR-V      PIC 9(10).
              10 KONTR-MANRVX REDEFINES KONTR-MA-NR-V  PIC X(10).
              10 KONTR-VB-NR  REDEFINES KONTR-MA-NR-V  PIC X(10).
      *  Hier werden folgende Kontroll-Daten gespeichert:
              10 KONTR-DATEN-V.
      *  Den Programmnamen des Programms, das die Datei erstellt oder
      *  zuletzt fortgeschrieben hat.
                 15 KONTR-PGM         PIC X(8).
      *  Den Jobnamen des Jobs in dem die Datei erstellt oder zuletzt
      *  fortgeschrieben wurde.
                 15 KONTR-JOB         PIC X(8).
      *  Das Tagesdatum an dem die Datei erstellt oder zuletzt fortge-
      *  schrieben wurde.
                 15 KONTR-AENDDAT     PIC S9(7) PACKED-DECIMAL.
      *  Das Buchungsdatum unter dem die Datei erstellt wurde.
                 15 KONTR-BUCHDAT     PIC S9(7) PACKED-DECIMAL.
              10 KONTR-FI-V     PIC X(204).
      **                                                           ***
      **             2. Redefinition / Kontroll-Nachsatz           ***
      **                                                           ***
           05 KONTRNACH-BER REDEFINES VB-ERGEBNIS.
               10 KONTR-SA-N         PIC X(6).
               10 KONTR-LAENGE-N     PIC S9(4) COMP.
               10 KONTR-MA-NR-N      PIC 9(10).
               10 KONTR-MANRNX REDEFINES KONTR-MA-NR-N  PIC X(10).
               10 KONTR-VB-NR  REDEFINES KONTR-MA-NR-N  PIC X(10).
      *  Hier werden folgende Kontroll-Daten gespeichert:
               10 KONTR-DATEN-N.
      *  Die Anzahl der Saetze der Datei.
                  15 KONTR-ANZ-SAETZE  PIC S9(9) PACKED-DECIMAL.
               10 KONTR-FI-N     PIC X(223).
      ****
      ****  Ende Definition - VB-Ergebnissatz (OM450S) *************
      ****
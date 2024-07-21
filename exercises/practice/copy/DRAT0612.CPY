      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.12.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0612.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRAT0612.CPv  $
      *
      *   Rev 2.6   Jun 23 2020 15:21:24   t13ac20
      *AAT00055: neue Fehlervariablen Z-FEHLER-INHALT zur Initialis
      *ierung
      *
      *   Rev 2.5   Sep 19 2017 11:35:02   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren-DVM
      *
      *   Rev 2.4   May 15 2017 13:39:14   t13ac20
      *AAT00055: Anpassungen Fehlerverfahren
      *
      *   Rev 2.3   May 15 2017 11:07:38   t13ac20
      *AAT00055: Anpassungen Fehlerverfahren
      *
      *   Rev 2.2   Mar 01 2017 17:26:32   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren-Wien
      *
      *   Rev 2.1   Jan 25 2017 10:12:02   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren
      *
      *   Rev 2.0   Dec 23 2016 16:54:22   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *    COPY-MEMBER: DRAT0612
      *    --------------------------------------------------------
      *    Daten-Copy für Fehlerverfahren
      *    --------------------------------------------------------
      ******************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * -------------------------------------------------------------
      * AAT00055!03.01.17!2.1 ! neu                       ! IFR
      *         !24.02.17!2.2 ! Ergänzung Reaktionen      ! IFR
120517* AAT00055!12.05.17!2.3 ! Reaktion vom 24.02.17     ! SWO
      *         !        !    ! wieder rückgängig;        !
      *         !        !    ! Ergänzung Reaktion "E"    !
150517* AAT00055!15.05.17!2.4 ! Konstanten für Fehler-    ! SWO
      *         !        !    ! nummern 6 und 8 eingefügt !
150817* AAT00055!15.05.17!2.5 ! Anpass. Z-FEHLER-FELDNAME ! IFR
      *         !        !    ! an TBAT0033               !
      * AAT00055!23.06.20!2.6 ! NEUE FELDER               ! T13AC20
      *         !        !    ! Z-FEHLER-INHALT-xxx       !
      *         !        !    !                           !
      *
      * ENDE-LOGBUCH
      ******************************************************************
      *
      *    Dieses Copy enthält die Anweisungen zur Durchführung
      *    eines Programmabbruchs falls er durch eine Fehleraktion
      *    erforderlich ist.
      *
      *    Voraussetzung: Copy DRAT0612
      *
      ******************************************************************
      *
       01  FILLER.
           05  FILLER               PIC X(032)
                                    VALUE '*** KONSTANTEN ***'.
 
           05  K-UPAT0030                PIC  X(008) VALUE 'UPAT0030'.
 
           05  K-KEINE-REAKTION                  PIC X(01) VALUE ' '.
           05  K-INITIALISIEREN                  PIC X(01) VALUE 'I'.
           05  K-SATZ-WEITERVERARBEITEN-N        PIC X(01) VALUE 'S'.
           05  K-VERTRAG-WEITERVERARBEITEN-N     PIC X(01) VALUE 'V'.
           05  K-PROGRAMM-ABBRECHEN              PIC X(01) VALUE 'A'.
110517     05  K-PROGRAMM-ABBRECHEN-SOFORT       PIC X(01) VALUE 'E'.
240217*    05  K-KEIN-ABBRUCHFEHLER              PIC X(01) VALUE 'K'.
 
           05  K-NACHSATZ-FEHLT                  PIC 9(04) VALUE 1.
           05  K-VORSATZ-FALSCH                  PIC 9(04) VALUE 2.
           05  K-FELD-UNGUELTIG                  PIC 9(04) VALUE 3.
           05  K-FELD-N-NUMERISCH                PIC 9(04) VALUE 4.
           05  K-FELD-LEER                       PIC 9(04) VALUE 5.
150517     05  K-DOPPEL-LOESCH                   PIC 9(04) VALUE 6.
           05  K-HINWEIS                         PIC 9(04) VALUE 7.
150517     05  K-FEHLER                          PIC 9(04) VALUE 8.
 
       01  FILLER.
           05  FILLER               PIC  X(032)
                                VALUE '*** SCHALTER /STEUERFELDER ***'.
 
      * Schalter um wg. Fehlern beim Fehlerhandling abzubrechen
           05  S-VOR-PROGRAMMENDE-ABBR-KZ     PIC X VALUE SPACE.
               88 S-VOR-PROGRAMMENDE-ABBR-J         VALUE 'J'.
               88 S-VOR-PROGRAMMENDE-ABBR-N         VALUE 'N'.
 
      * Schalter um Sätze wg. Fehlerhandling nicht
      * weiter zu verarbeiten
           05  S-SATZ-WEITERVERARBEITEN-KZ    PIC X VALUE SPACE.
               88 S-SATZ-WEITERVERARBEITEN-J        VALUE 'J'.
               88 S-SATZ-WEITERVERARBEITEN-N        VALUE 'N'.
 
      * Schalter um Verträge wg. Fehlerhandling nicht
      * weiter zu verarbeiten
           05  S-VERTRAG-WEITERVERARBEITEN-KZ PIC X VALUE SPACE.
               88 S-VERTRAG-WEITERVERARBEITEN-J     VALUE 'J'.
               88 S-VERTRAG-WEITERVERARBEITEN-N     VALUE 'N'.
 
      * Schalter Vertrag in Vertrags-Fehlerdatei vorhanden
           05  S-VERT-NR-IN-VERTRERR          PIC X VALUE SPACE.
               88 S-VERT-NR-IN-VERTRERR-J           VALUE 'J'.
               88 S-VERT-NR-IN-VERTRERR-N           VALUE 'N'.
 
      * Schalter für Fehlerroutine
           05  S-ELEMENT-KZ                   PIC 9 VALUE ZERO.
               88 S-ELEMENT-GEFUNDEN-J              VALUE 0.
               88 S-ELEMENT-GEFUNDEN-N              VALUE 1.
 
       01  FILLER.
           05  FILLER               PIC X(032)
                                    VALUE '*** DRUCKBEREICH ***'.
           05  D-ANZ-FEHL-STAT            PIC 9(009)  VALUE 0.
 
       01  FILLER.
           05  FILLER                        PIC X(032)
                                    VALUE '*** ZWISCHENFELDER ETC ***'.
 
       01  FILLER.
           05  Z-FEHLER-VAR-1                PIC X(15).
           05  Z-FEHLER-VAR-2                PIC X(15).
           05  Z-FEHLER-VAR-3                PIC X(15).
           05  Z-FEHLER-VAR-4                PIC X(15).
           05  Z-FEHLER-VAR-5                PIC X(15).
           05  Z-FEHLER-SCHL.
               10  Z-FEHLER-NUMMER           PIC 9(4).
150817*        10  Z-FEHLER-FELDNAME         PIC X(15).
150817         10  Z-FEHLER-FELDNAME         PIC X(25).
           05  Z-FEHLER-VERTRAG              PIC X(015) JUSTIFIED RIGHT.
           05  Z-FEHLER-INHALT-X             PIC X(15).
           05  Z-FEHLER-INHALT-9             PIC 9(15).
           05  Z-FEHLER-INHALT-P             PIC 9(15) COMP-3.
           05  Z-FEHLER-INHALT-D-SQL         PIC  X(10).
           05  Z-FEHLER-INHALT-D-ISO         PIC  X(10).
           05  Z-FEHLER-INHALT-9V9           PIC  9(14)V9.
           05  Z-FEHLER-INHALT-9V99          PIC  9(13)V99.
           05  Z-FEHLER-INHALT-9V999         PIC  9(12)V999.
           05  Z-FEHLER-INHALT-S9            PIC S9(15).
           05  Z-FEHLER-INHALT-S9V9          PIC S9(14)V9.
           05  Z-FEHLER-INHALT-S9V99         PIC S9(13)V99.
           05  Z-FEHLER-INHALT-S9V999        PIC S9(12)V999.
           05  Z-FEHLER-INHALT-SP            PIC S9(15)     COMP-3.
           05  Z-FEHLER-INHALT-SPV9          PIC S9(14)V9   COMP-3.
           05  Z-FEHLER-INHALT-SPV99         PIC S9(13)V99  COMP-3.
           05  Z-FEHLER-INHALT-SPV999        PIC S9(12)V999 COMP-3.
           05  Z-VNR-NUM                     PIC X(015).
           05  FILLER                        REDEFINES Z-VNR-NUM.
               10 Z-VNR-NUM-R                PIC 9(015).
           05  Z-FS-FEHLER-ANZAHL            PIC 9(09).
 
           05  Z-INDEX                       PIC S9(04) COMP.
 
 
 
 
 
 
 




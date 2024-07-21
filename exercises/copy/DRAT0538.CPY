      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0538.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0538.CPv  $
      *
      *   Rev 2.2   Sep 19 2017 11:34:48   t13ex27
      *AAT00055: Überarbeitung Fehlerverfahren-DVM
      *
      *   Rev 2.1   Apr 09 2014 11:59:32   t13ex77
      *FKI01459: Fehlerverfahren - Vergrößern der Fehlerzähler
      *
      *   Rev 2.0   Feb 12 2013 13:24:42   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *   Dieses Copy beschreibt den Aufbau für die Tabelle
      *   zur internen Ermittlung der Fehleranzahl in einem Batch-
      *   programm
      *
      *
      *--------------------------------------------------------------*
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FKI01459!??.04.14!    ! Neuerstellung             ! abn      *
      *         !        !    ! Fehlerstatistik           !          *
150817* AAT00055!15.05.17!2.2 ! Anpass. TB-FEHLER-FELDNAME! IFR      *
      *         !        !    ! an TBAT0033               !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *--------------------------------------------------------------*
      *
      * Tabelle Anzahl Fehler pro Feld
       01 TB-FEHLER-STATISTIK.
      *    Achtung - wer den OCCURS-Wert ändert muss auch den
      *              MAX-Wert ändern!
           05  TB-Fehler-ID OCCURS 100.
              15 TB-Fehler-IDX.
                 20  TB-Fehler-Nummer      PIC 9(4).
150817*          20  TB-Fehler-Feldname    PIC X(15).
150817           20  TB-Fehler-Feldname    PIC X(25).
              15 TB-Job-Name               PIC X(08).
              15 TB-Fehler-Relevanz        PIC X(01).
              15 TB-Fehler-Reaktion        PIC X(01).
              15 TB-Fehler-Anzahl          PIC 9(09).
      *
       01  TB-Fehler-MAX                   PIC 9(03) COMP-3 VALUE 100.
      *
      *---------------------------------------------------------------*


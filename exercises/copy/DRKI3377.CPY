      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  17.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3377.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3377.CPv  $
      *
      *   Rev 2.1   17 Aug 2018 11:06:50   t13ex78
      *AKI01622: G79 - Rück-DTA Betreuerwechsel
      *
      *   Rev 2.0   17 Aug 2018 07:18:30   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *  Copy-Member: DRKI3377 Betreuerwechsel G79 Geiger Edelmetalle *
      *****************************************************************
      *    VERSION   01                                               *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * -------------------------------------------------------------**
      * AKI01622!17.08.18!2.0 ! Neu                       ! tfl       *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * Der Rück-DTA Betreuerwechsel für Geiger Edelmetalle geht wenn *
      * man so will am Quasi-Standard vorbei.                         *
      * Die Vertragsnummer wird lediglich 7-stellig bereitgestellt,   *
      * die Direktionsnummer des neuen VB ist irrelevant und die Da-  *
      * tenfelder sind durch ';' getrennt, da sie auf dem Helios-     *
      * server als csv-Files bereitgestellt werden.                   *
      * Zusätzlich gibt es rudimentärste VB-Stammdaten in Form von    *
      * Vor- und Nachname des neuen Betreuers.                        *
      *****************************************************************
      *
       01  DRKI3377-SATZ.
           10 DRKI3377-VERTRNUMMER            PIC  9(007).
           10 DRKI3377-SEMIKOLON1             PIC  X(001).
           10 DRKI3377-VBNUMMER               PIC  9(007).
           10 DRKI3377-SEMIKOLON2             PIC  X(001).
           10 DRKI3377-NACHNAME               PIC  X(040).
           10 DRKI3377-SEMIKOLON3             PIC  X(001).
           10 DRKI3377-VORNAME                PIC  X(020).


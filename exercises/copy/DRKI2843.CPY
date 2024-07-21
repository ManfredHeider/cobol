      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2843.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2843.CPv  $
      *
      *   Rev 2.1   Feb 18 2014 14:22:46   t13ex77
      *AKI01274: Prüfung der DeuBa-DTAs
      *
      *   Rev 2.0   Jan 27 2014 13:34:50   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * Eingabedaten
      *
       01 PI-DRKI2843.
          05 PI-DRKI2843-DATEN.
      *      Kennzeichen, um welche Art des Auruf es sich handelt
             10 PI-DRKI2843-AUFRUF-KZ           PIC  X(001).
      *         Initialisierungsaufruf
                88 PI-DRKI2843-AUFRUF-INIT          VALUE 'I'.
      *         Verarbeitungsaufruf
                88 PI-DRKI2843-AUFRUF-VERARB        VALUE 'V'.
      *         Abschlussaufruf zum Beenden der aktuellen Verarb.
                88 PI-DRKI2843-AUFRUF-ENDE          VALUE 'E'.
      *      Kennzeichen, welche Daten verarbeitet werden sollen
             10 PI-DRKI2843-DEUBA-DWS-KZ        PIC  X(003).
      *         Verarbeitung der Deuba-Daten
                88 PI-DRKI2843-DEUBA                VALUE 'DEU'.
      *         Verarbeitung der DWS-Daten
                88 PI-DRKI2843-DWS                  VALUE 'DWS'.
      *      Kennzeichen, ob alte Daten gelöscht werden sollen
             10 PI-DRKI2843-LOESCH-KZ           PIC  X(001).
      *         Löschen durchführen
                88 PI-DRKI2843-LOESCH-JA            VALUE 'J'.
      *         Löschen nicht durchführen
                88 PI-DRKI2843-LOESCH-NEIN          VALUE 'N'.
      *      VertragsID für die Verarbeitung DWS
             10 PI-DRKI2843-VERTRAGSID          PIC  X(013).
      *
      * Ausgabedaten
      *
       01 PO-DRKI2843.
      * Rückgabe-Returncode
          05 PO-DRKI2843-RC                     PIC  X(003).
      *      Verarbeitung war erfolgreich
             88 PO-DRKI2843-RC-OK                   VALUE 'OK '.
      *      Es wurden keine zu verarbeitende Daten gefunden
             88 PO-DRKI2843-RC-NFD                  VALUE 'NFD'.
      *      Die Steuerdatei war beim Erstaufruf leer
             88 PO-DRKI2843-RC-STL                  VALUE 'STL'.
      *      Schwerer Fehler aus UPRO
             88 PO-DRKI2843-RC-ERR                  VALUE 'ERR'.

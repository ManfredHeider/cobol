      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3773.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI3773.CPv  $
      *
      *   Rev 2.3   Oct 08 2020 14:41:20   u130684
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.2   Oct 06 2020 17:09:48   u130684
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.1   Jun 23 2020 13:15:06   t13ex27
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.0   Apr 09 2020 16:05:56   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                          PIC  9(002).
      *
      *    Die VB-Nummer des Abschlussvermittlers
           05 'XX'-VB-NR                        PIC  9(007).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                          PIC  9(015).
           05 'XX'-VERTRAGSZUSTAND              PIC S9(02).
           05 'XX'-VERTRAGSZUSTAND-DATUM        PIC  X(10).
           05 'XX'-VERTRAGSZUSTAND-KZ           PIC  X(01).
      *        J WENN VERTRAGSZUSTAND GEAENDERT
      *
      *    Vertragsbeginn- und Endedatum im DB2 Format
           05 'XX'-VERTRAGSBEGINN               PIC  X(010).
           05 'XX'-VERTRAGSBEGINN-KZ            PIC  X(001).
           05 'XX'-VERTRAGSABLAUF               PIC  X(010).
           05 'XX'-VERTRAGSABLAUF-KZ            PIC  X(001).
      *
           05 'XX'-VEREIGENVERMKZ               PIC  X(001).
           05 FILLER                            PIC  X(050).



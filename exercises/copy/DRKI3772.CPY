      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.04.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3772.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI3772.CPv  $
      *
      *   Rev 2.4   Oct 08 2020 14:41:14   u130684
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.3   Oct 06 2020 17:09:42   u130684
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.2   Oct 06 2020 13:19:46   u130684
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.1   Jun 23 2020 13:14:18   t13ex27
      *AKI01773: Anbindung Compeon an DV - Verarbeitung
      *
      *   Rev 2.0   Apr 09 2020 16:05:04   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    Verarbeitungskennzeichen  I = Insert U = Update
           05 'XX'-VERARB-KZ                    PIC  X(001).
      *        Die Gesellschaftsnummer, sollte immer die 90 sein
           05 'XX'-GES                        PIC  9(002).
      *
      *    Die Vertragsnummer mit fuehrenden Nullen
           05 'XX'-VNR                        PIC  9(015).
      *
      *    Die VB-Nummer des Abschlussvermittlers
           05 'XX'-VB-NR                      PIC  9(007).
           05 'XX'-VB-NR-KZ                   PIC  X(001).
      *
      *    Produktart
           05 'XX'-PRODUKT-ART                PIC  X(001).
           05 'XX'-PRODUKT-ART-KZ             PIC  X(001).

      *    Vertragsöffnung
           05 'XX'-OEFFNUNG                   PIC  9(008).
           05 'XX'-OEFFNUNG-KZ                PIC  X(001).
      *
      *    Vertragsschliessung
           05 'XX'-SCHLIESSUNG                PIC  9(008).
           05 'XX'-SCHLIESSUNG-KZ             PIC  X(001).
      *
      *    Finanzierungsbetrag
           05 'XX'-FINBETR                    PIC  9(12)V99.
           05 'XX'-FINBETR-KZ                 PIC  X(001).
      *
      *    Zusage
           05 'XX'-ZUSAGE                     PIC  9(008).
           05 'XX'-ZUSAGE-KZ                  PIC  X(001).
      *
      *    Auszahlung
           05 'XX'-AUSZAHLUNG                 PIC  9(008).
           05 'XX'-AUSZAHLUNG-KZ              PIC  X(001).

           05 'XX'-PRODUKTGEBER               PIC  X(030).
           05 'XX'-PRODUKTGEBER-KZ            PIC  X(001).

           05 'XX'-AKTSALDO                   PIC S9(12)V9(2).
           05 'XX'-AKTSALDO-KZ                PIC  X(001).

           05 'XX'-SOLLZINS                   PIC S9(2)V9(4).
           05 'XX'-SOLLZINS-KZ                PIC  X(001).

           05 'XX'-RATE                       PIC S9(12)V9(2).
           05 'XX'-RATE-KZ                    PIC  X(001).

           05 'XX'-FAELLIG                    PIC  9(008).
           05 'XX'-FAELLIG-KZ                 PIC  X(001).

           05 'XX'-RESTLAUF                   PIC  9(003).
           05 'XX'-RESTLAUF-KZ                PIC  X(001).

           05 'XX'-IBAN                       PIC  X(034).
           05 'XX'-IBAN-KZ                    PIC  X(001).

           05 'XX'-BIC                        PIC  X(011).
           05 'XX'-BIC-KZ                     PIC  X(001).

           05 'XX'-MANDAT                     PIC  X(035).
           05 'XX'-MANDAT-KZ                  PIC  X(001).

           05 'XX'-INHKTONNAME                PIC  X(030).
           05 'XX'-INHKTONNAME-KZ             PIC  X(001).

           05 'XX'-INHKTOVNAME                PIC  X(030).
           05 'XX'-INHKTOVNAME-KZ             PIC  X(001).

           05 'XX'-DELETEKZ                   PIC  X(001).
           05 'XX'-DELETEKZ-KZ                PIC  X(001).

           05 'XX'-IBANKREDIT                 PIC  X(034).
           05 'XX'-IBANKREDIT-KZ              PIC  X(001).





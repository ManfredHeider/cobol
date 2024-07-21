      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.12.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0027.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0027.CPv  $
      *
      *   Rev 2.2   Dec 20 2012 08:15:14   t13ex15
      *FKI01396: DVAG Wien Ersetzen PMKI0722 durch Funktion in Aktu
      *alisierung
      *
      *   Rev 2.1   Dec 18 2012 12:40:56   t13ex15
      *FKI01396: DVAG Wien Ersetzen PMKI0722 durch Funktion in Aktu
      *alisierung
      *
      *   Rev 2.0   Dec 18 2012 07:43:32   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K W 0 0 2 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE   280 BYTE - 23. 09. 2002                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *18.12.12!NEU aus DRKI1446                       !SWZ      ! 01  *
      *        !                                       !         !     *
      ******************************************************************

       01  DRKW0027.
           05  BA-GES                        PIC  9(002).
           05  BA-VNR                        PIC  9(015).
           05  BA-ANTEIL-LFDNR               PIC  9(002).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER

           05  BA-ISIN-NR                    PIC  X(012).
      *        ISIN-Nummer

           05  BA-GUTHABEN                   PIC  9(011)V99.
           05  BA-ANTEILE                    PIC  9(006)V999.
           05  BA-EINZ-KUM                   PIC  9(013).
      *        KUMULIERTE EINZAHLUNGEN

           05  BA-AUSZ-KUM                   PIC  9(013).
      *        KUMULIERTE AUSZAHLUNGEN

           05  BA-WERT-GES                   PIC  9(003)V99.
      *        WERTENTWICKLUNG GESAMT

           05  BA-WERT-JAHR                  PIC  9(003)V99.
      *        WERTENTWICKLUNG FUER DAS LFD JAHR

           05  BA-BUCH-BETRAG                PIC  9(007)V99.
           05  BA-BUCH-DATUM                 PIC  X(010).
      *    05  BA-BUCH-SCHLUESSEL            PIC  9(02).
           05  BA-BUCH-SCHLUESSEL            PIC S9(002).
           05  BA-BUCH-STEUERBETRAG          PIC  9(007)V99.
           05  BA-BUCH-ANTEILE               PIC  9(006)V999.
      *        Anteile der aktuellen Buchung

           05  BA-FREI-VERBRAUCHT            PIC  9(005)V99.
      *        VERBRAUCHTER FREISTELLUNGSBETRAG
           05  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(001).
      *        J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT

           05  BA-ZINS-AB-ST                 PIC  9(006)V99.
      *        ZINSABSCHLAGSTEUER
           05  BA-AEND-ZINS-AB-ST-KZ         PIC  X(001).
      *        J WENN ZINABSCHLAGSTEUER GEAENDERT
           05  BA-VL-KZ                      PIC  X(001).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)

           05  BA-ZW-GEWINN                  PIC S9(005)V99.
      *        Zwischengewinn Guthaben
           05  BA-AEND-ZW-GEWINN-KZ          PIC  X(001).
      *        J WENN Zwischengewinn GEAENDERT
           05  BA-UMSATZ-NETTO               PIC  9(007)V99.
      *        Nettobetrag = Rücknahmepreis x Anteile

           05  BA-AEND-ANTEIL-KZ             PIC  X(001).
      *        J wenn eins der Felder in GuthabenAnteil geaendert

      *    05  BA-ANTEILE-AKT                PIC  9(006)V999.
      *        Anteile der aktuellen Buchung
           05  FILLER                        PIC  X(009).

           05  BA-WP-KENN-NR                 PIC  X(006).
      *        FUER DIT: WERTPAPIERKENNNUMMER
      *        FUER III UND RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL

      *    05  FILLER                        PIC  X(28).
      *    05  FILLER                        PIC  X(10).

           05  BA-GUTHABEN-DATUM-VON         PIC  X(010).
      *    05  FILLER                        PIC  X(100).
           05  BA-ANTEILE-4-NK               PIC  9(006)V9999.
      *        DEUTSCHE BANK Anteile mit 4 Nachkommastellen
           05  BA-KUM-AUT-WIEDER-ANLAGE     PIC  9(11)V99.
      *        Kumulierte automatische Wiederanlage
      *    05  FILLER                        PIC  X(090).
      *    05  FILLER                        PIC  X(080).
           05  FILLER                        PIC  X(067).

           05  BA-AKTUAL-KZ                  PIC  X(001).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

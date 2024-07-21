      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.06.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2324.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2324.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:50:54   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Aug 13 2010 09:24:02   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB PGM-Änderung wg. Zusä
      *tzliche Felder
      *
      *   Rev 2.1   Jun 18 2009 10:38:22   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Jun 15 2009 16:27:02   t13ex51
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 3 2 4    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG GUTHABENPRODUKT                                *
      *        - AUSGABE: ALLGEMEIN DBPRODUKT FUER SEB-BANK            *
      *                                                                *
160606*    SATZLÄNGE   443                                             *
140610*    SATZLÄNGE   444                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *15.06.09!NEUERSTELLUNG                          !HRO      ! 01  *
      *KI-NEU  !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
140610*14.06.10!Neue Felder Nominalzins, Darlehensart  !SWZ      ! 01.1*
140610*        !und TILGUNGANF                         !         !     *
      *        !                                       !         !     *
      *----------------------------------------------------------------*
      ******************************************************************
       01  DRKI2324.

           05  DB-GES                             PIC  9(02).
           05  DB-VNR                             PIC  9(15).
           05  DB-GHP-LFDNR                       PIC  9(04).
      *        NUR Unterkontonummer
           05  DB-GHP-PRODUKT-ID                  PIC  S9(04).
      *        Produkt-ID
           05  DB-DBPROD-LFDNR                    PIC  S9(04).
      *        lfd. Nr. von DBPRODUKT

           05  DB-WAEHRUNGS-SCHL                  PIC S9(04).
      *        Währungsschlüssel von DB
           05  DB-AEND-WAERHUNGS-SCHL-KZ          PIC  X(01).
      *        J WENN Währungsschlüssel GEAENDERT

           05  DB-BILANZ-SCHL                     PIC S9(04).
      *        Bilanzschlüssel von DB
           05  DB-AEND-BILANZ-SCHL-KZ             PIC  X(01).
      *        J WENN Bilanzschlüssel GEAENDERT

           05  DB-LIMIT-EXTERN                    PIC  S9(12)V99.
      *        Externes Limit
           05  DB-AEND-LIMIT-EXTERN-KZ            PIC  X(01).
      *        J WENN Externes Limit GEAENDERT

           05  DB-SALDO                           PIC  S9(12)V99.
      *        Saldo
           05  DB-AEND-SALDO-KZ                   PIC  X(01).
      *        J WENN Saldo GEAENDERT

           05  DB-WKZ                             PIC  X(01).
      *        WKZ
           05  DB-AEND-WKZ-KZ                     PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-KARTEN-ART                      PIC  X(03).
      *        Kartenart
           05  DB-AEND-KARTEN-ART-KZ              PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-ANZAHL-KARTEN-AKTIV             PIC S9(04).
      *        Anzahl der aktiven Karten je Kartenart
           05  DB-AEND-ANZAHL-KARTEN-AKTIV-KZ     PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-KREDIT-KARTEN-NR                PIC  X(16).
      *        Kreditkartennummer
           05  DB-AEND-KREDIT-KARTEN-NR-KZ        PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-HAUPT-KARTEN-NR                 PIC  X(16).
      *        Hauptkartennummer
           05  DB-AEND-HAUPT-KARTEN-NR-KZ         PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-VNR-HAUPT-KARTEN-NR             PIC  X(15).
      *        Vertragsnummer der Hauptkartennummer
           05  DB-AEND-VNR-HAUPT-KARTEN-NR-KZ     PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-ABRECH-KONTO-NR                 PIC  X(10).
      *        Abrechnungskonto-Nummer
           05  DB-AEND-ABRECH-KONTO-NR-KZ         PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-BLZ-ABRECH-KONTO                PIC  X(08).
      *        BLZ der Abrechnungskonto-Nummer
           05  DB-AEND-BLZ-ABRECH-KONTO-KZ        PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-FILIAL-NR                       PIC S9(04).
      *        BLZ der Abrechnungskonto-Nummer
           05  DB-AEND-FILIAL-NR-KZ               PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-KONTO-ART                       PIC  X(03).
      *        Kontoart
           05  DB-AEND-KONTO-ART-KZ               PIC  X(01).
      *        J WENN WKZ GEAENDERT

           05  DB-VERTRAGS-ENDE                   PIC  X(10).
           05  DB-AEND-VERTRAGS-ENDE-KZ           PIC  X(01).
      *        J WENN VERTRAGSENDE GEAENDERT

           05  DB-DARLEHENS-BETRAG                PIC  S9(12)V99.
      *        Darlehensbetrag
           05  DB-AEND-DARLEHENS-BETRAG-KZ        PIC  X(01).
      *        J WENN Saldo GEAENDERT

           05  DB-WKZ-DARLEHEN                    PIC  X(01).
           05  DB-AEND-WKZ-DARLEHENKZ             PIC  X(01).
      *        J WENN WÄHRUNGSKENNZEICHEN GEAENDERT

           05  DB-BLZ-KUNDE                       PIC  X(08).
      *        Bankleitzahl  des Kundens
           05  DB-AEND-BLZ-KUNDE-KZ               PIC  X(01).
      *        J WENN WKZ GEAENDERT
           05  DB-KOND-SCHL                       PIC  X(02).
      *        Konditionsschlüssel
           05  DB-AEND-KOND-SCHL-KZ               PIC  X(01).
      *        J WENN WKZ GEAENDERT

160606     05  DB-SOLLZINS                        PIC  S9(02)V9999.
>>         05  DB-AEND-SOLLZINS-KZ                PIC  X(001).
      *        J = Änderung Sollzinssatz
           05  DB-UEBERZZINS                      PIC  S9(02)V9999.
           05  DB-AEND-UEBERZZINS-KZ              PIC  X(001).
      *        J = Änderung Überziehungszinssatz
           05  DB-HABENZINS                       PIC  S9(02)V9999.
           05  DB-AEND-HABENZINS-KZ               PIC  X(001).
      *        J = Änderung Habenzinssatz
           05  DB-MAHNSTUFE                       PIC  X(001).
    <<     05  DB-AEND-MAHNSTUFE-KZ               PIC  X(001).
160606*        J = Änderung Mahnstufe
290808*    05  DB-ZAHLUNGSWEISE                   PIC  9(001).
121108     05  DB-ZAHLUNGSWEISE                   PIC S9(002).
>>>>>>*            ZAHLUNGSWEISE-ID  0-FEHLT 1-JÄHRLICH
      *            2-HALBJ 4-VIERTELJ 5-2MON 6-MON 8-EINMAL
           05  DB-AEND-ZAHLUNGSWEISE-KZ           PIC  X(001).
      *            J = ÄNDERUNG ZAHLUNGSWEISE
           05  DB-RATENHOEHE                      PIC S9(012)V99.
      *            SPARRATE
<<<<<<     05  DB-AEND-RATENHOEHE-KZ              PIC  X(001).
290808*            J = ÄNDERUNG RATENHOEHE
      *
090908     05  DB-DATUM-KREDITZUSAGE              PIC  X(10).
>>>>>>*            DATUM DER KREDITZUSAGE
           05  DB-AEND-DAT-KREDZUS-KZ             PIC  X(01).
      *        J WENN DATUM GEÄNDERT
      *
           05  DB-DATUM-KREDITAUSZAHLUNG          PIC  X(10).
      *            DATUM DER KREDITAUSZAHLUNG
           05  DB-AEND-DAT-KREDAUS-KZ             PIC  X(01).
      *        J WENN DATUM GEÄNDERT
      *
           05  DB-DATUM-ENDEZINSFS                PIC  X(10).
      *            ENDEDATUM DER ZINSFESTSCHREIGUNG
           05  DB-AEND-DAT-ENDEZFS-KZ             PIC  X(01).
      *        J WENN DATUM GEÄNDERT
      *
           05  DB-AKTUELLER-SALDO                 PIC S9(012)V99.
      *            AKTUELLER SALDOHENSBETRAG
           05  DB-AEND-AKT-SALDO-KZ               PIC  X(001).
      *            J = ÄNDERUNG RATENHOEHE
      *
           05  DB-DATUM-ENDEZRFREI                PIC  X(10).
      *            ENDEDATUM ZEITRAUM FREIJAHRE
           05  DB-AEND-DAT-ZRFREI-KZ              PIC  X(01).
      *        J WENN DATUM GEÄNDERT
      *
           05  DB-TILGUNGSART                     PIC  X(001).
      *            TILGUNGSART
           05  DB-AEND-TILGUNGSART-KZ             PIC  X(001).
      *        J = ÄNDERUNG DER TILGUNGSART
      *
           05  DB-TILGUNGSPROZENT                 PIC  S9(02)V9999.
      *            TILGUNG IN PROZENT
           05  DB-AEND-TILG-PROZ-KZ               PIC  X(001).
      *        J = ÄNDERUNG TILGUNGSPROZ-SATZES
      *
           05  DB-TILGUNGSPRODUKT                 PIC  X(10).
      *            TILGUNGSPRODUKT
           05  DB-AEND-TILG-PROD-KZ               PIC  X(01).
      *        J WENN SICH DAS TILGUNGSPRODUKT ÄNDERT
      *
           05  DB-BEITRAG-TILGSATZ                PIC S9(012)V99.
      *            BEITRAGSTILGUNG
           05  DB-AEND-TILG-SATZ-KZ               PIC  X(001).
      *            J = ÄNDERUNG BEITRAGSTILGUNG
      *
           05  DB-ANNUITAET                       PIC S9(012)V99.
      *            AKTUELLE RATE IN EURO
           05  DB-AEND-ANNUITAET-KZ               PIC  X(001).
      *            J = ÄNDERUNG DER RATE
      *
           05  DB-ANTEIL-RATE                     PIC S9(012)V99.
      *            TILGUNGSANTEIL PRO RATE
           05  DB-AEND-ANT-RATE-KZ                PIC  X(001).
      *            J = ÄNDERUNG DES TIL-ANTEIL PRO RATE
      *
           05  DB-ANTEIL-ZINS                     PIC S9(012)V99.
      *            ZINSANTEIL PRO RATE
090908     05  DB-AEND-ANT-ZINS-KZ                PIC  X(001).
<<<<<<*            J = ÄNDERUNG DES ZINSANTEIL PRO RATE
221008*
>>>>>>     05  DB-KREDITART                       PIC  X(006).
      *            KREDITART  PK ODER TK
           05  DB-AEND-KREDITART-KZ               PIC  X(001).
      *            J = ÄNDERUNG DER KREDITART
      *
           05  DB-DATUM-FAELLIGKEIT               PIC  X(010).
      *            DATUM DER KREDITFALLIGKEIT
      *
<<<<<<     05  DB-AEND-DAT-KREDFAEL-KZ            PIC  X(001).
221008*            J WENN DATUM GEÄNDERT
      *
           05  DB-AKTUAL-KZ                       PIC  X(01).

      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

140610     05  DB-TILGUNGANF                      PIC  S9(02)V9999.
>>         05  DB-AEND-TILGUNGANF-KZ              PIC  X(001).
      *        J = Änderung TILGUNGANF

140610     05  DB-NOMINALZINS                     PIC  S9(02)V9999.
>>         05  DB-AEND-NOMINALZINS-KZ             PIC  X(001).
      *        J = Änderung NOMINALZINS

140610     05  DB-DARLEHENSART                    PIC  X(001).
>>         05  DB-AEND-DARLEHENSART-KZ            PIC  X(001).
      *        J = Änderung NOMINALZINS

290808*    05  FILLER                             PIC  X(46).
140610     05  FILLER                             PIC  X(30).
      *        RESERVE
      ******************************************************************


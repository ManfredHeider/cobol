      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 4 3 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *    SATZLÄNGE   704 BYTE - 23. 10. 2010                         *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *AKI00892! NEU                                   !WHO      !     *
      *        !                                       !         !     *
150411*AKI01040! NEUES ATTRIBUT                        !MDI      ! 001 *
150411*        ! BA-KUM-BRUTTO-ERTRAGSGUTSCHR          !         !     *
150411*--------!---------------------------------------!---------!-----*
231012*AKI01165!23.10.12! 2.0! zusätzliche Attribute zu! WHO           *
      *        !        !    ! VL-Sperrfristen         !               *
130514*----------------------------------------------------------------*
130514*AKI01227!13.05.14!    ! zusätzliche Attribute zu! WHO           *
      *        !        !    ! OIF                     !               *
      *        !        !    !                         !               *
      *----------------------------------------------------------------*
061014*AKI01327!06.10.14! Erweiterungen für            ! WHO           *
      *        !        ! Wertentwicklung              !               *
      *----------------------------------------------------------------*
      *DBO10080!10.08.22!Felderweiterung für           !               *
      *        !        !BA-ANTEILE-4-NK 9(007)V9999   !               *
      *        !        !um 1 B                        !u131774        *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  DRKI2437.
           05  BA-VERT-ID                    PIC  X(13).
         02  BA-GESAMT.
           05  BA-AKTUAL-KZ                  PIC  X(001).
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

           05  BA-VL-KZ                      PIC  X(001).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)

           05  BA-AEND-ANTEIL-KZ             PIC  X(001).
      *        J wenn eins der Felder in GuthabenAnteil geaendert

           05  BA-WP-KENN-NR                 PIC  X(006).

           05  BA-GUTHABEN-DATUM-VON         PIC  X(010).

           05  BA-ANTEILE-4-NK               PIC  9(007)V9999.
      *        DEUTSCHE BANK Anteile mit 4 Nachkommastellen
           05  BA-KUM-AUT-WIEDER-ANLAGE     PIC  9(11)V99.
      *        Kumulierte automatische Wiederanlage
           05  BA-INV-KAPITAL-AKT           PIC 9(11)V99.
      *            inverstiertes Kapital aktuell
           05  BA-INV-KAPITAL-VOR-JAHR      PIC 9(11)V99.
      *            inverstiertes Kapital Vorjahr
           05  BA-KUM-UEBERTRAEGE           PIC S9(11)V99.
      *            Kumulierte Übertraäge
           05  BA-KUM-AUTOM-WIEDER-VORJ     PIC S9(11)V99.
      *            Kumulierte automatische Wiederanlage 31.12. des
      *            Vorjahres
           05  BA-DURCH-EINSTANDSKURS       PIC 9(05)V99.
      *            durchschnittlicher Einstandskurs
           05  BA-QUALITETSFLAG             PIC 9(01).
      *            Qualitätsflag zum Einstandskurs
           05  BA-REALISIERTE-GEW-VERL      PIC S9(11)V99.
      *            Realisierte Gewinne/Verlust
150411     05  BA-KUM-BRUTTO-ERTRAGSGUTSCHR PIC 9(11)V99.
150411*            kumulierter Bruttobetrag Ertragsgutschriften
231012     05  BA-ANTEILE-FREI              PIC 9(07)V9(04).
>>    *            Anzahl der Anteile ohne Sperrschlüssel 26xx
           05  BA-VL-SPERRE-DAT1            pic X(10).
      *            Ende der Sperrfrist für die unten angegebenen Anteile
           05  BA-ANTEILE-GESPERRT1         PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g Termin gesperrten Anteile
           05  BA-VL-SPERRE-DAT2            pic X(10).
      *            Ende der zweiten Sperrfrist für die angegebenen. Ante
           05  BA-ANTEILE-GESPERRT2         PIC 9(07)V9(04).
      *            Anzahl der bis zum o.g. 2. Termin gesperrten Anteile
           05  BA-VL-SPERRE-DAT3            pic X(10).
      *            Ende der dritten Sperrfrist für die angegebenen. Ante
<<         05  BA-ANTEILE-GESPERRT3         PIC 9(07)V9(04).
231212*            Anzahl der bis zum o.g. 3. Termin gesperrten Anteile
140613     05  BA-ANZ-ANTEILE-GEKUE-OIF           PIC 9(07)V9(04).
>>    *            Anzahl wegen unwiderruflicher Rückgabe
      *            gekündigte Anteile bei offenen Immobilienfonds
           05  BA-KUM-AUS-AKT-HALB-JAHR           PIC 9(11)V99.
      *            Kumulierte Auszahlungen aktuelles Kalenderhalbjahr
      *            (ohne Abgänge aus Überträgen)
           05  BA-KUM-BRUTTO-BETRAG-OIF           PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften in
      *            EUR bei offenen Immobilienfonds in Liquidation ab
      *            Zeitpunkt der Kündigung - gesamt
           05  BA-KUM-BRUTTO-BETRAG-OIF-VJ       PIC 9(11)V99.
      *            Kumulierter Bruttobetrag der Ertragsgutschriften
      *            in EUR bei offenen Immobilienfonds in Liquidation
      *            - per 31.12. des Vorjahres
           05  BA-KUM-BRUTTO-BETRAG-VJ            PIC 9(11)V99.
      *            Kumulierter Bruttobetrag Ertragsgutschriften in
      *            EUR gesamt - per 31.12. des Vorjahres
           05  BA-ANZ-ANTEILE-VERFUE-OIF          PIC 9(07)V9(04).
      *            Verfügbarer / kündbarer Anteilsbestand
      *            bei offenen Immobilienfonds
           05  BA-ANZ-ANTEILE-GESPER-OIF          PIC 9(07)V9(04).
<<    *            Noch gesperrter Anteilbestand
140613*            aufgrund Haltefristen bei offenen Immobilienfonds
130514     05  BA-ANT-VORJU13-OIF                 PIC 9(07)V9(04).
130514*            Antahl Anteile mit erwerb vor 22.07.2013
130514     05  BA-BER-AUSGE-KONTINGENT            PIC 9(06)V9(02).
130514*            Bereits ausgenutzes Halbjahres-Kontingent WKKN
130514     05  BA-ANLEGER-ID-OIF                  PIC X(014).
061014     05  BA-WERT-PAPIER-BEZ            PIC X(050).
>>    *            Wertpapierbezeichnung
           05  BA-RUECKNAHME-PREIS           PIC S9(005)V9999.
      *            Rücknahmepreis
           05  BA-N-REAL-GW-VL-EUR-INKL      PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  BA-N-REAL-GW-VL-VH-INKL       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent  inkl.
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  BA-N-REAL-GW-VL-EUR-OHNE      PIC S9(11)V99.
      *            nicht realisierter Gewinn/ Verlust in Euro ohne .
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
           05  BA-N-REAL-GW-VL-VH-OHNE       PIC S9(06)V99.
      *            nicht realisierter Gewinn/ Verlust in Prozent ohne
      *           Barausschüttungen  aller Bestände in diesem Unterdepot
<<
061014     05  FILLER                             PIC X(180).

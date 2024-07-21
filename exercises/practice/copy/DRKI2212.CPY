      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.11.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2212.CPY  $                                 0001000
      *    $Revision:   2.17  $
      *
      *    $Log:   L:/mf/CPY/DRKI2212.CPv  $
      *
      *   Rev 2.17   Mar 27 2017 09:02:08   t13ae31
      *FKI01544: III alten Code entfernen
      *
      *   Rev 2.16   Jun 10 2014 15:21:20   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.15   Jun 10 2014 14:45:38   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.14   Jun 10 2014 12:08:44   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.13   Jun 05 2014 13:58:08   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.12   Jun 05 2014 13:51:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.11   May 30 2014 11:11:00   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.10   May 30 2014 10:58:56   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.9   May 28 2014 10:34:14   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.8   May 28 2014 10:19:48   t13ex15
      *AKI01227: OnlineKI: Darstellung Anteilrückgabe bei OIF G40 B
      *estand PGM Cpy
      *
      *   Rev 2.7   Feb 26 2014 16:53:58   t13ex15
      *FKI01447: AGI Bestands-DTA Ergänzung ISIN
      *
      *   Rev 2.6   Jun 17 2009 14:28:32   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik
      *
      *   Rev 2.5   Jun 15 2009 16:33:42   t13ex39
      *AKI00740: AllianzGI/FodB: Verarbeitung Unterkonten-Systematik
      *
      *   Rev 2.4   Dec 22 2008 11:18:22   t13ex50
      *AKI00760: AGI/FodB (ProdPartner 40): Anpassung von Buchungss
      *chlüsseln
      *
      *   Rev 2.3   Jan 14 2008 12:06:40   t13ex50
      *AKI00693: Masch. sichern + Verarb. AGI Komplett- und Tagesbe
      *stände
      *
      *   Rev 2.2   Jan 14 2008 09:48:14   t13ex50
      *AKI00693: Korrektur Akt. Bestände (Buchungsschlüssel)
      *
      *   Rev 2.1   Dec 19 2007 13:55:54   t13ex50
      *AKI00639: PremiumDepot: Datenspiegel und Programmanpassungen
      *
      *   Rev 2.0   Nov 15 2007 13:57:20   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 2 2 1 2    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUFBEREITUNGSPROGRAMME DER BANKEN,           *
      *    AUFBEREITUNG DER BESTANDSVERAENDERUNGEN.                    *
      *        - AUSGABE: BESTANDSVERAENDERUNGEN                       *
      *                                                                *
      *   DIES IST EINE KOPIE DES DRKI0173 PLUS 3 NEUE FELDER UND      *
      *      EINEM FILLER FÜR KÜNFTIGE ERWEITERUNGEN                   *
      *                                                                *
      *                                                                *
      *                                                                *
      *    SATZLÄNGE   400 STELLEN                                     *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *14.11.07!NEUERSTELLUNG  (AUS DRKI0173) + NEUE   !SCHLEICHER 2.1 *
      *AKI00639!                FELDER                 !         !     *
      *        !ACHTUNG: BUCHUNGSSCHLÜSSEL HAT JETZT   !         !     *
      *        !         3 ANSTATT 2 STELLEN           !         !     *
      *--------!---------------------------------------!---------!-----*
140108*14.01.08!KORREKTUR BUCHUNGSSCHLÜSSEL VORZEICHEN !SCHLEICHER 2.2 *
      *AKI00693!   WIEDER EINGEFÜGT.                   !         !     *
      *--------!---------------------------------------!---------!-----*
140108*14.01.08!FILLER UM 1 ERHÖHT UND AUF 300 STELLEN !SCHLEICHER 2.3 *
      *AKI00693!   ZU KOMMEN                           !         !     *
      *--------!---------------------------------------!---------!-----*
171208*17.12.08!BUCHUNGSSCHLÜSSEL 1 STELLE VERLÄNGERT  !SCHLEICHER 2.4 *
      *AKI00760!  FILLER 1 STELLE VERKÜRZT             !         !     *
      *--------!----------------------------------------!---------!----*
160109*16.01.09!NEUE AUFBAU PRIMARYKEY                  !WHO      !2.5 *
      *--------!----------------------------------------!---------!----*
180214*18.02.14!Umstellung wegen zusätzlicher ISIN      !SWZ      !2.7 *
      *--------!----------------------------------------!---------!----*
260514*26.05.14!Erweiterung wegen OIF, von 300 auf 400  !SWZ      !2.8 *
      *--------!----------------------------------------!---------!----*
      *22.03.17! III entfernt                           !HHU      !2.9 *
      *        !                                        !         !    *
      *--------!---------------------------------------!---------!-----*
      ******************************************************************
       01  DRKI2212.
           05  BA-GES                        PIC  9(02).
160109     05  BA-VNR                        PIC  9(13).
160109     05  BA-GHP-LFDNR                  PIC  9(02).
      *        LFDNR VON ANTEIL = VERTRAGSUNTERNUMMER
160109     05  BA-ANTEIL-LFDNR               PIC  9(02).
 
           05  BA-WP-KENN-NR                 PIC  X(06).
      *        FUER DIT: WERTPAPIERKENNNUMMER
      *        FUER RESTLICHE BANKEN: DVAG-PRODUKTSCHLUSSEL
 
           05  BA-GUTHABEN                   PIC  9(11)V99.
           05  BA-ANTEILE                    PIC  9(06)V999.
           05  BA-EINZ-KUM                   PIC  9(13).
      *        KUMULIERTE EINZAHLUNGEN
 
           05  BA-AUSZ-KUM                   PIC  9(13).
      *        KUMULIERTE AUSZAHLUNGEN
 
           05  BA-WERT-GES                   PIC  9(03)V99.
      *        WERTENTWICKLUNG GESAMT
 
           05  BA-WERT-JAHR                  PIC  9(03)V99.
      *        WERTENTWICKLUNG FUER DAS LFD JAHR
 
           05  BA-BUCH-BETRAG                PIC  9(07)V99.
           05  BA-BUCH-DATUM                 PIC  X(10).
140108*    05  BA-BUCH-SCHLUESSEL            PIC  9(03).
140108*    05  BA-BUCH-SCHLUESSEL            PIC S9(02).
171208     05  BA-BUCH-SCHLUESSEL            PIC S9(03).
           05  BA-BUCH-STEUERBETRAG          PIC  9(07)V99.
           05  BA-BUCH-ANTEILE               PIC  9(06)V999.
 
180214*    05  BA-FREI-VERBRAUCHT            PIC  9(05)V99.
      *        VERBRAUCHTER FREISTELLUNGSBETRAG
180214*    05  BA-AEND-FREI-VERBRAUCHT-KZ    PIC  X(01).
      *        J WENN VERBRAUCHTER FREISTELLUNGAUFTRAG GEAENDERT
 
180214*    05  BA-ZINS-AB-ST                 PIC  9(06)V99.
      *        ZINSABSCHLAGSTEUER
180214*    05  BA-AEND-ZINS-AB-ST-KZ         PIC  X(01).
      *        J WENN ZINABSCHLAGSTEUER GEAENDERT
           05  BA-VL-KZ                      PIC  X(01).
      *        KZ, OB PRODUKT VERMOEGENSWIRKSAMELEISTUNG (J oder leer)
 
180214*    05  BA-ZW-GEWINN                  PIC S9(005)V99.
      *        Zwischengewinn Guthaben
180214*    05  BA-AEND-ZW-GEWINN-KZ          PIC  X(01).
      *        J WENN Zwischengewinn GEAENDERT
           05  BA-UMSATZ-NETTO               PIC  9(007)V99.
      *        Nettobetrag = Rücknahmepreis x Anteile
 
           05  BA-AEND-ANTEIL-KZ             PIC  X(01).
      *        J wenn eins der Felder in GuthabenAnteil geaendert
 
 
           05  BA-GUTHABEN-DATUM-VON         PIC  X(10).
           05  BA-AKTUAL-KZ                  PIC  X(01).
 
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
 
 
141107     05  BA-GEB-BETRAG                 PIC  9(007)V99.
..!   *        GEBÜHRENBETRAG
..!        05  BA-AEND-GEB-BETRAG-KZ         PIC  X(001).
..!   *
..!        05  BA-GEB-SCHLUESSEL             PIC  9(002).
..!   *        GEBÜHRENSCHLÜSSEL
..!        05  BA-AEND-GEB-SCHLUESSEL-KZ     PIC  X(001).
..!        05  BA-GEB-TEXT                   PIC  X(020).
..!   *        GEBÜHRENTEXT
..!        05  BA-AEND-GEB-TEXT-KZ           PIC  X(001).
 
180214     05  BA-ISIN                       PIC  X(012).
180214*        ISIN
      *        *****************************************************
      *        *   OIF/ANSFUG
      *        *****************************************************
140314     05  BA-ANZ-ANT-UN-RCK-GEK  PIC  9(07)V9999.
      *        Anzahl wegen unwiderruflicher Rückgabe gekündigte
      *        Anteile bei offenen Immobilienfonds
140314     05  BA-ANZ-ANT-ABL-MHF-ERR PIC  9(07)V9999.
      *        Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *        erreicht (12 Monate oder länger im Bestand des
      *        Kunden)
140314     05  BA-ANZ-ANT-ABL-MHF-N-E PIC  9(07)V9999.
      *        Anzahl Anteile OIF, Ablauf der MHF für Kündigung
      *        noch nicht erreicht (kürzer als 12 Monate im
      *        Bestand des Kunden)
140314     05  BA-ANZ-ANT-ERW-VOR-JU13 PIC  9(07)V9999.
      *        Anzahl Anteile OIF mit Erwerb vor dem 22.07.2013
      *        (Anzahl der Anteile, die im Rahmen des halbjährl.
      *        Kontingentes für vor dem 22.07.2013 erworbene
      *        Anteile ohne Kündigung zurückgegeben werden können)
140314     05  BA-EUR-BER-VERK-JU13 PIC  9(06)V99.
      *        Bereits ausgenutztes Halbjahres-Kontingent dieser
      *        WKN, zur Rückgabe von OIF ( Volumen in Euro, das im
      *        Rahmen des halbjährlichen Kontingentes dieser WKN
      *        für die Rückgabe von vor dem 22.07.2013 erworbene
      *        Anteile im aktuellen Kalenderhalbjahr bereits in
      *        Anspruch genommen wurdech genommen wurde)
140314     05  BA-ANLEGER-ID        PIC  X(14).
      *        Anleger-ID zu bereits ausgenutztem
      *        Halbjahres-Kontingent dieser WKN, zur Rückgabe
      *        von OIF
      *        *****************************************************
      *        *   Reserve
      *        *****************************************************
260514     05  FILLER                        PIC  X(143).


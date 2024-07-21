      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2331.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2331.CPv  $
      *
      *   Rev 2.6   Jan 26 2016 12:49:54   t13ex39
      *AKI01411: Namensänderung DWS Altersvorsorgeprodukte 
      *
      *   Rev 2.5   Sep 02 2013 12:22:28   t13ex14
      *AKI01204: SEPA - DB-Online - DWS Sparplan
      *
      *   Rev 2.4   Jul 02 2012 10:35:52   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.3   Jun 29 2011 07:55:20   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.2   Oct 27 2009 12:21:02   t13ex50
      *
      *   Rev 2.1   Aug 20 2009 15:24:30   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 04 2009 15:33:24   t13ex50
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                  ###############                               *
      *         #######  D R K I 2 3 3 1  #######                      *
      *                  ###############                               *
      *                                                                *
      *    COPY-MEMBER FÜR AUSGABEDATEI VERäNDRUNGSBESTAND             *
      *          Investmentsparpläne von DWS                           *
      *                                                                *
      *    LAENGE =  300 BYTES       ERSTELLT  AM 03.08.09             *
010813*    LAENGE =  390 BYTES       ERSTELLT  AM 03.08.09             *
      *                                                                *
      *                                                                *
      *                      DATEILÄNGE = 300                          *
010813*                      DATEILÄNGE = 390                          *
      *                      DATEITYP   = QSAM                         *
      *                                                                *
      *                                                                *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!--------!----!-----------------------------!-----------*
      *AUFTRAG !Datum   !Ver !Text                         !Name       *
      *--------!--------!----!-----------------------------!-----------*
      *AKI00844!03.08.09!2.00! Neuerstellung               !Schleicher *
      *--------!--------!----!-----------------------------!-----------*
      *AKI01040!25.05.11!3.00! Neue Felder Dauerzulagenantr!P. Hübner  *
      *--------!--------!----!-----------------------------!-----------*
100612*AKI01149!11.06.12!5.0 !PIN verarbeiten              !WHO        *
      *----------------------------------------------------------------*
010813*AKI01204!01.08.13!    !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
010813*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
110116*AKI01411!11.01.16!neue Vertragstypen für DWS-Alt-   ! WHO      *
      *        !        !vorsorge                          !          *
      *        !        !                                  !           *
      *--------!--------!----!-----------------------------!-----------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI2331.
           05  BNK-SA                                 PIC  X(002).
      *            SATZART        = '16'
           05  BNK-GES                                PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER                PIC  9(003).
                   15 BNK-KUNDENNUMMER                PIC  9(007).
               10  BNK-ORDNUNGSNUMMER                 PIC  9(002).
      *                UNTERDEPOTNUMMER
           05  BNK-PORTFOLIONUMMER                    PIC  X(010).
           05  BNK-AEND-PORTFOLIO-KZ                  PIC  X(001).

           05  BNK-VERARB-KZ                          PIC  X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *                D = DELETE

           05  BNK-DATUM-EROEFFNUNG.
               10  JAHR-EROEFFNUNG                    PIC  9(004).
               10  MONAT-EROEFFNUNG                   PIC  9(002).
               10  TAG-EROEFFNUNG                     PIC  9(002).
           05  BNK-AEND-DAT-E-KZ                      PIC  X(001).
      *            J = Änderung Datum Eroeffnung

           05  BNK-DATUM-AUSZAHLTERMIN.
               10  JAHR-AUSZAHLTERMIN                 PIC  9(004).
               10  MONAT-AUSZAHLTERMIN                PIC  9(002).
               10  TAG-AUSZAHLTERMIN                  PIC  9(002).
           05  BNK-AEND-DAT-A-KZ                      PIC  X(001).
      *            J = ÄNDERUNG DATUM AUSZAHLTERMIN

           05  BNK-BANKVERBINDUNG                     PIC  X(020).
           05  BNK-AEND-BANKVERB-KZ                   PIC  X(001).
      *            J = Änderung Bankverbindung

           05  BNK-BLZ                                PIC  9(011).
           05  BNK-AEND-BLZ-KZ                        PIC  X(001).
      *            J = Änderung BLZ

           05  BNK-KONTONUMMER                        PIC  9(018).
           05  BNK-AEND-KTONR-KZ                      PIC  X(001).
      *            J = Änderung Kontonummer

           05  BNK-ABW-KONTOINHABER                   PIC  X(027).
           05  BNK-AEND-KTOINH-KZ                     PIC  X(001).
      *            J = Änderung Kontoinhaber

           05  BNK-SPARRATE                           PIC  9(007)V99.
           05  BNK-AEND-SPARRATE-KZ                   PIC  X(001).
      *            J = Änderung Sparrate

           05  BNK-DYNAM                              PIC  9(002)V99.
           05  BNK-AEND-DYNAM-KZ                      PIC  X(001).
      *           J = Änderung Dynamisierung

           05  BNK-ZAHLWEISE                          PIC  9(001).
           05  BNK-AEND-ZW-KZ                         PIC  X(001).
      *            J = Änderung Zahlweise

           05  BNK-TAG                                PIC  9(002).
           05  BNK-AEND-TAG-KZ                        PIC  X(001).
      *            J = ÄNDERUNG EINZAHLUNGSTAG

           05  BNK-VERTRAGSTYP                        PIC  9(002).
           05  BNK-AEND-VERTRTYP-KZ                   PIC  X(001).
      *            J = ÄNDERUNG VRTRAGSTYP

           05  BNK-ENDE-RATENAUSSETZUNG               PIC  9(008).
           05  BNK-AEND-ENDE-RATENAUS-KZ              PIC  X(001).
      *            J = ÄNDERUNG ENDE RATENAUSSETZUNG

           05  BNK-MAHNSTATUS                         PIC  X(002).
           05  BNK-AEND-MAHNSTAT-KZ                   PIC  X(001).
      *            J = ÄNDERUNG DES MAHNSTATUS

           05  BNK-DAUERZULAGENANTRAG                 PIC  X(001).
           05  BNK-AEND-DAUERZULANTRAG-KZ             PIC  X(001).
      *            J = ÄNDERUNG DES DAUERZULAGENANTRAGES

           05  BNK-DAT-VERTR-AUFLOESUNG.
               10  JAHR-VERTR-AUFLOESUNG              PIC  9(004).
               10  MONAT-VERTR-AUFLOESUNG             PIC  9(002).
               10  TAG-VERTR-AUFLOESUNG               PIC  9(002).
           05  BNK-AEND-DAT-VERTR-AUFL-KZ             PIC  X(001).
      *            J = ÄNDERUNG DATUM vertrag auflösung

           05  BNK-LETZT-AENDDAT                      PIC  9(008).
      *            Buchungsdatum aus Vorsatz

           05  BNK-WKZ                                PIC  X(003).
      *            Währungskennzeichen

           05  BNK-DEL-KZ                             PIC  X(001).
           05  BNK-AEND-DEL-KZ                        PIC  X(001).
      *            J = ÄNDERUNG DES DEL-KZ

250511     05 BNK-KZ-UNM-ZULAGEBER                PIC X(001).
      *              KZ UNMITTELBARE ZULAGENBERECHTIGUNG
           05 BNK-AEND-KZ-UNM-ZULAGEBER-KZ        PIC X(001).

           05  BNK-KZ-UEBWEG-KAPUEB                PIC X(001).
      *              KZ ÜBERTRAGUNGSWEG KAPITALÜBERTRAGUNG
           05 BNK-AEND-KZ-UEBWEG-KAPUEB-KZ         PIC X(001).

           05  BNK-KZ-UEBART                       PIC X(001).
      *              KZ ÜBERTRAGUNGSART
           05 BNK-AEND-KZ-UEBART-KZ                PIC X(001).

           05  BNK-DAT-KAPITAL-UEBERTRAG.
               10  JAHR-KAP-UEBERTAG               PIC 9(004).
               10  MONAT-KAP-UEBERTRAG             PIC 9(002).
               10  TAG-KAP-UEBERTRAG               PIC 9(002).
      *              DATUM KAPITAL ÜBERTRAG
           05 BNK-AEND-DAT-KAP-UEBERTRAG-KZ        PIC X(001).

           05  BNK-DAT-GELD-BEWEG.
               10  JAHR-GELD-BEWEG                 PIC 9(004).
               10  MONAT-GELD-BEWEG                PIC 9(002).
               10  TAG-GELD-BEWEG                  PIC 9(002).
      *              DATUM GELD BEWEGUNG
           05 BNK-AEND-DAT-GELD-BEWEG-KZ           PIC X(001).

           05  BNK-ANZAHL-KINDZULAGE               PIC 9(002).
      *               ANZAHL KINDERZULAGEN IM ANTRAG
           05 BNK-AEND-ANZAHL-KINDZULAGE-KZ        PIC X(001).

           05  BNK-GARANTIESUMME                   PIC 9(11)V99.
           05  BNK-GARANTIESUMME-KZ                PIC X(001).
      *               GARANTIESUMME

           05  BNK-DAT-BEGINN-ABLSTAB.
               10  JAHR-BEGINN-ABLSTAB             PIC 9(004).
               10  MONAT-BEGINN-ABLSTAB            PIC 9(002).
               10  TAG-BEGINN-ABLSTAB              PIC 9(002).
           05  BNK-DAT-BEGINN-ABLSTAB-KZ           PIC X(001).
      *              DATUM BEGINN ABLAUFSTABILISIERUNG

           05  BNK-DAT-BEGINN-HOECHSICH.
               10  JAHR-BEGINN-HOECHSICH           PIC 9(004).
               10  MONAT-BEGINN-HOECHSICH          PIC 9(002).
               10  TAG-BEGINN-HOECHSICH            PIC 9(002).
           05  BNK-DAT-BEGINN-HOECHSICH-KZ         PIC X(001).
      *              DATUM BEGINN HOECHSTSTANDSICHERUNG

           05  BNK-DAT-LETZTHOECHST.
               10  JAHR-LETZTHOECHST               PIC 9(004).
               10  MONAT-LETZTHOECHST              PIC 9(002).
               10  TAG-LETZTHOECHST                PIC 9(002).
           05  BNK-DAT-LETZTHOECHST-KZ             PIC X(001).
      *              DATUM LETZTE HOECHSTSTANDANPASSUNG

           05  BNK-BETRHOECHST                     PIC 9(11)V99.
           05  BNK-BETRHOECHST-KZ                  PIC X(001).
      *              HÖCHSTSTAND

           05  BNK-DAT-KUENDIGUNG.
               10  JAHR-KUENDIGUNG                 PIC 9(004).
               10  MONAT-KUENDIGUNG                PIC 9(002).
250511         10  TAG-KUENDIGUNG                  PIC 9(002).
           05  BNK-DAT-KUENDIGUNG-KZ               PIC X(001).
      *              DATUM KUENDIGUNGSTERMIN
100612     05  BNK-PIN                             PIC 9(009).
110612     05  BNK-AEND-PIN-KZ                     PIC X(01).
010813     05  BNK-IBAN                            PIC X(34).
>>    *        IBAN des Belastungskontos zur Lastschrift für
      *        den Rateneinzug
           05  BNK-AEND-IBAN-KZ                    PIC X(01).
      *        J WENN IBAN GEÄNDERT
           05  BNK-BIC                             PIC X(11).
      *        BIC des Belastungskontos zur Lastschrift für
      *        den Rateneinzug
           05  BNK-AEND-BIC-KZ                     PIC X(01).
      *        J WENN BIC GEÄNDERT
           05  BNK-MANDREF                         PIC X(35).
      *        Mandatsreferenz
    <<     05  BNK-AEND-MANDREF-KZ                 PIC X(01).
010813*        J WENN MANDATSREFERENZ GEÄNDERT
110116     05  BNK-PRODUKT-PROV                    pic S9(03)V99.
110116     05  BNK-AEND-PRODUKT-PROV-KZ            PIC X(01).
110116*        J WENN EÄNDERT
110116     05  BNK-TARIF                           pic X(02).
110116     05  BNK-AEND-TARIF-KZ                   PIC X(01).
110116     05  BNK-RESERVE                         PIC X(012).

      *---------------------------------------------------------------*


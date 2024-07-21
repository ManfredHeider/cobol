      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2355.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2355.CPv  $
      *
      *   Rev 2.6   Feb 01 2016 06:10:16   t13ex39
      *AKI01411: Namensänderung DWS Altersvorsorgeprodukte 
      *
      *   Rev 2.5   Sep 02 2013 12:22:46   t13ex14
      *AKI01204: SEPA - DB-Online - DWS Sparplan
      *
      *   Rev 2.4   Jul 18 2011 10:12:44   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.3   Jul 15 2011 09:37:38   t13ae31
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertra
      *g Dauerzulagenantrag
      *
      *   Rev 2.2   Oct 27 2009 12:22:12   t13ex50
      *
      *   Rev 2.1   Aug 20 2009 15:25:06   t13ex50
      *AKI00844: DWS-Investmentsparpläne
      *
      *   Rev 2.0   Aug 19 2009 09:43:16   t13ex50
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS DWS-Investmentsparpläne              *
      *                                                                *
      *    SATZLÄNGE   302                                             *
010813*    SATZLÄNGE   382                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------+--------+----+-----------------------------!-----------*
      *AKI00844!03.08.09!2.00! Neuerstellung               !Schleicher *
      *--------+--------+----+-----------------------------+-----------*
      *AKI01040!25.05.11!3.00! Neue Felder Dauerzulagenantr!P. Hübner  *
      *----------------------------------------------------------------*
010813*AKI01204!01.08.13! NEUE FELDER EINGEFÜGT f. SEPA    !WMI        *
010813*        !        !   UND DEN FILLER ANGEPASST       !           *
      *--------!--------!----------------------------------!-----------*
110116*AKI01411!11.01.16!neue Vertragstypen für DWS-Alt-   ! WHO      *
      *        !        !vorsorge                          !          *
      *        !        !                                  !           *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI2355.

      *
           05  KSDS-SA                                PIC  X(002).
      *            SATZART        = '16'
           05  KSDS-GES                               PIC  9(002).
      *            GESELLSCHAFT   = '48'
      *
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 KSDS-FILIAL-NR                  PIC  9(003).
                   15 KSDS-KDNR                       PIC  9(007).
                   15 KSDS-VERT-ORDNUNGSNR            PIC  9(002).
      *        UNTERDEPOTNUMMER
               10 FILLER REDEFINES KSDS-VNR.
      *        VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   15 KSDS-KEY-1                      PIC  9(010).
                   15 FILLER                          PIC  9(002).
      *                UNTERDEPOTNUMMER

               10  KSDS-LFDNR                         PIC  9(002).
      *                EINE LAUFENDE NUMMER

           05  KSDS-PORTFOLIONUMMER                   PIC  X(010).

           05  KSDS-DATUM-EROEFFNUNG.
               10 JAHR-EROEFFNUNG                     PIC  9(004).
               10 MONAT-EROEFFNUNG                    PIC  9(002).
               10 TAG-EROEFFNUNG                      PIC  9(002).

           05  KSDS-DATUM-AUSZAHLTERMIN.
               10 JAHR-AUSZAHLTERMIN                  PIC  9(004).
               10 MONAT-AUSZAHLTERMIN                 PIC  9(002).
               10 TAG-AUSZAHLTERMIN                   PIC  9(002).

           05  KSDS-BANKVERBINDUNG                    PIC  X(020).
           05  KSDS-BLZ                               PIC  9(011).
           05  KSDS-KONTONUMMER                       PIC  9(018).
           05  KSDS-ABW-KONTOINHABER                  PIC  X(027).
           05  KSDS-SPARRATE                          PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
           05  KSDS-DYNAM                             PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
           05  KSDS-ZAHLWEISE                         PIC  9(001).
      *               ZAHLWEISE.........0 : ZAHLWEISE FEHLT
      *               ZAHLWEISE.........1 : JAEHRLICH
      *               ZAHLWEISE.........2 : HALBJAEHRLICH
      *               ZAHLWEISE.........4 : VIERTELJAEHRLICH
      *               ZAHLWEISE.........5 : 2-MONATLICH
      *               ZAHLWEISE.........6 : MONATLICH
      *               ZAHLWEISE.........8 : EINMALPRAEMIE
           05  KSDS-TAG                               PIC  9(002).
      *               TAG IM MONAT FÜR DIE EINZAHLUNG (RATENTERMIN)
           05  KSDS-VERTRAGSTYP                       PIC  9(002).
      *               51 DWS RIESTERRENTE PREMIUM
      *               52 DWS RIESTERRENTE PREMIUM
      *               54 DWS RIESTERRENTE PREMIUM
      *               56 DWS VERMÖGENSSPARPLAN PREMIUM
      *               58 DWS BASISRENTE
      *               81 DWS TOPRENTE DYNAMIK
      *               82 DWS TOPRENTE DYNAMIK
      *               83 DWS TOPRENTE BALANCE
      *               84 DWS TOPRENTE BALANCE
      *               85 DWS TOPRENTE PLUS
      *               86 DWS TOPRENTE PLUS
      *               87 DWS TOPRENTE DYNAMIK
      *               88 DWS TOPRENTE BALANCE
           05  KSDS-ENDE-RATENAUSSETZUNG              PIC  9(008).
      *               ENDEDATUM DER RATENAUSSETZUNG
           05  KSDS-MAHNSTATUS                        PIC  X(002).
      *            10  LASTSCHRIFT WURDE NICHT EINGELÖST
      *            20  ZWEITE LASTSCHRIFT IN FOLGE WURDE NICHT EINGEL.
      *            40  DRITTE LASTSCHRIFT IN FOLGE WURDE NICHT EINGEL.
           05  KSDS-DAUERZULAGENANTRAG                PIC  X(001).
      *            ' '
      *            'J' DAUERZULAGENANTRAG EINGREICHT
      *            'N' DAUERZULAGENANTRAG NICHT EINGREICHT

           05  KSDS-DAT-VERTR-AUFLOESUNG.
               10  JAHR-VERTR-AUFLOESUNG              PIC  9(004).
               10  MONAT-VERTR-AUFLOESUNG             PIC  9(002).
               10  TAG-VERTR-AUFLOESUNG               PIC  9(002).
      *               DATUM VERTRAGSAUFLÖSUNG  BZW. VORZEITIGE
      *               VERTRAGSKÜNDIGUNG
           05  KSDS-DEL-KZ                            PIC  X(001).
      *               LÖSCHKENNZEICHEN
      *                D = SATZ IST ZU LÖSCHEN
      *                S = DATENSCHUTZSPERRE AKTIVIERT
250511     05  KSDS-KZ-UNM-ZULAGEBER                  PIC X(001).
      *              KZ UNMITTELBARE ZULAGENBERECHTIGUNG
           05  KSDS-KZ-UEBWEG-KAPUEB                  PIC X(001).
      *              KZ ÜBERTRAGUNGSWEG KAPITALÜBERTRAGUNG
           05  KSDS-KZ-UEBART                         PIC X(001).
      *              KZ ÜBERTRAGUNGSART
           05  KSDS-DAT-KAPITAL-UEBERTRAG.
               10  JAHR-KAP-UEBERTAG                  PIC 9(004).
               10  MONAT-KAP-UEBERTRAG                PIC 9(002).
               10  TAG-KAP-UEBERTRAG                  PIC 9(002).
      *              DATUM KAPITAL ÜBERTRAG
           05  KSDS-DAT-GELD-BEWEG.
               10  JAHR-GELD-BEWEG                    PIC 9(004).
               10  MONAT-GELD-BEWEG                   PIC 9(002).
               10  TAG-GELD-BEWEG                     PIC 9(002).
      *              DATUM GELD BEWEGUNG
           05  KSDS-ANZAHL-KINDZULAGE                 PIC 9(002).
      *               ANZAHL KINDERZULAGEN IM ANTRAG
           05  KSDS-GARANTIESUMME                     PIC 9(11)V9.
      *               GARANTIESUMME
           05  KSDS-DAT-BEGINN-ABLSTAB.
               10  JAHR-BEGINN-ABLSTAB                PIC 9(004).
               10  MONAT-BEGINN-ABLSTAB               PIC 9(002).
               10  TAG-BEGINN-ABLSTAB                 PIC 9(002).
      *              DATUM BEGINN ABLAUFSTABILISIERUNG
           05  KSDS-DAT-BEGINN-HOECHSICH.
               10  JAHR-BEGINN-HOECHSICH              PIC 9(004).
                10  MONAT-BEGINN-HOECHSICH             PIC 9(002).
                10  TAG-BEGINN-HOECHSICH               PIC 9(002).
      *              DATUM BEGINN HOECHSTSTANDSICHERUNG
            05  KSDS-DAT-LETZTHOECHST.
                10  JAHR-LETZTHOECHST                  PIC 9(004).
                10  MONAT-LETZTHOECHST                 PIC 9(002).
                10  TAG-LETZTHOECHST                   PIC 9(002).
      *              DATUM LETZTE HOECHSTSTANDANPASSUNG
            05  KSDS-BETRHOECHST                       PIC 9(11)V99.
      *              HÖCHSTSTAND
            05  KSDS-DAT-KUENDIGUNG.
                10  JAHR-KUENDIGUNG                    PIC 9(004).
                10  MONAT-KUENDIGUNG                   PIC 9(002).
250511          10  TAG-KUENDIGUNG                     PIC 9(002).
      *              DATUM KUENDIGUNGSTERMIN
           05  KSDS-AEND-DAT                          PIC  X(010).
      *               DATUM, WANN DER SATZ IN DIE HIST
      *                 GESCHRIEBEN WURDE
010813     05  KSDS-IBAN                              PIC X(34).
>>    *        IBAN des Belastungskontos zur Lastschrift für
      *        den Rateneinzug
           05  KSDS-BIC                               PIC X(11).
      *        BIC des Belastungskontos zur Lastschrift für
      *        den Rateneinzug
    <<     05  KSDS-MANDREF                           PIC X(35).
010813*        Mandatsreferenz
110116     05  KSDS-PRODUKT-PROV                      pic S9(003)V99.

110116     05  KSDS-TARIF                             pic X(002).
110116**   05  KSDS-RESERVE                           PIC  X(056).
110116     05  KSDS-RESERVE                           PIC  X(049).
      *            RESERVE
      *---------------------------------------------------------------*



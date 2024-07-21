      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1342.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRKI1342.CPv  $
      *
      *   Rev 2.11   Feb 28 2021 10:43:54   t13ex39
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT
      *
      *   Rev 2.10   Jul 12 2016 15:28:56   t13ex39
      *AKI01433: Verringerung der DeuBa-Ermittlungsfehler auf das W
      *esentliche
      *
      *   Rev 2.9   Sep 02 2013 10:29:42   t13ex14
      *AKI01204: SEPA - 'XX'-Online - DeuBa Sparpläne
      *
      *   Rev 2.8   Jul 02 2012 10:34:28   t13ex39
      *AKI01149: PIN bei DWS und DeuBa
      *
      *   Rev 2.7   22 Apr 2008 16:58:26   t13ex39
      *AKI00708: WHO  - OnlineKI - Deutsche Bank Depot - angezeigte
      * Daten ZAHLUNG
      *
      *   Rev 2.6   Jan 16 2006 17:52:36   t13ex39
      *AKI00494: Änderung der Lieferung DeuBa-Sparpläne
      *
      *   Rev 2.2   Jan 19 2005 14:30:00   t13ex39
      *AKI00249: SPARPLÄNE
      *
      *   Rev 2.1   Jul 30 2002 16:48:36   t13ex12
      *AKI00274 Einführung 'XX'24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:21:44   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für Investmentsparpläne von 'XX'24               
      *                                                                *
      *    SATZLÄNGE   250                                             *
      *    SATZLÄNGE   400                                           *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!16.03.02! Neuerstellung                    !v.d.Goltz  *
      *--------!-------------------------------------------!-----------*
      *AKI00249!18.10.04! Neue Felder                      !Hornung    *
      *        !        ! Begin/Ende der Ratenaussetzung   !           *
      *        !        !                                  !           *
      * -------!--------!----------------------------------!-----------*
      *AKI00512!27.07.05! zus. Attribute für Vermögens-    !Hornung    *
      *        !        ! sparplan                         !           *
      *        !        !                                  !           *
      * -------!--------!----------------------------------!-----------*
      *AKI00494!11.11.05! neuer Aufbaudes Primary-Keys     !Hornung    *
      *        !        !                                  !           *
      *--------!--------!----------------------------------!-----------*
      *        !        !                                  !           *
      *AKI00708!14.04.08! neues Attribut                   ! WHO       *
      *--------!--------!----------------------------------!-----------*
      *AKI01149!11.06.12! PIN verarbeiten                  !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI01204!19.06.13! NEUE FELDER EINGEFÜGT f. SEPA    !WMI        *
      *        !        !   UND DEN FILLER ANGEPASST       !           *
      *----------------------------------------------------------------*
      *AKI01433!01.05.16! Ermittlungsfehler auf das We-    ! WHO      *
      *        !        ! sentliche reduzieren             !          *
      *        !        ! BANKVERBINDUNG                   !          *
      *        !        ! BLZ                              !          *
      *        !        ! KONTONUMMER                      !          *
      *        !        ! auf FILLER gesetzt               !          *
      *        !        !                                  !          *
      *        !        !                                  !          *
      *        !        !                                  !          *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI1342.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC X(001).
      *            SATZART = '1'
               15  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = ''XX' AG'
               15  'XX'-V-DATUM         PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            Tägliche Lieferung 'PLANT'
      *            Komplettbestand    'PLANK'

               15  'XX'-V-WKZ           PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
      *        15  FILLER             PIC X(268).
               15  FILLER             PIC X(368).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                        PIC  X(001).
      *            SATZART        = '7'
               15  'XX'-GES                       PIC  9(002).
      *            GESELLSCHAFT   = '48'
      *        15  'XX'-KEY.
>>    *            20  'XX'-VNR.
      *                25 FILIALNUMMER          PIC  9(003).
      *                25 KUNDENNUMMER          PIC  9(007).
      *            20  'XX'-VNR-1 REDEFINES 'XX'-VNR PIC 9(10).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
      *            20 'XX'-UNTERDEPOTNUMMER       PIC  9(002).
      *            20 'XX'-WAEHRUNG               PIC  9(003).
      *
      *            20  'XX'-PROD-ID.
      *                25  'XX'-PLAN-NR           PIC  9(002).
      *                25  'XX'-PROD-ID-REST      PIC  X(018).

               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIALNUMMER          PIC  9(003).
                       25 'XX'-KUNDENNUMMER          PIC  9(007).
                   20  'XX'-VNR-1 REDEFINES 'XX'-VNR PIC 9(10).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   20  'XX'-PLAN-NR               PIC  9(002).
                   20  'XX'-WAEHRUNG              PIC  9(003).
      *
               15  'XX'-UNTERDEPOTNUMMER          PIC  9(002).
<<             15  'XX'-PROD-ID.
                   25  'XX'-PROD-ID-REST          PIC  X(018).

               15  'XX'-DATUM-EROEFFNUNG.
                   20  'XX'-JAHR-EROEFFNUNG          PIC  9(004).
                   20  'XX'-MONAT-EROEFFNUNG         PIC  9(002).
                   20  'XX'-TAG-EROEFFNUNG           PIC  9(002).
               15  'XX'-DATUM-SCHLIESSUNG.
                   20  'XX'-JAHR-SCHLIESSUNG         PIC  9(004).
                   20  'XX'-MONAT-SCHLIESSUNG        PIC  9(002).
                   20  'XX'-TAG-SCHLIESSUNG          PIC  9(002).
      *        15  'XX'-BANKVERBINDUNG            PIC  X(020).
      *        15  'XX'-BLZ                       PIC  9(011).
      *        15  'XX'-KONTONUMMER               PIC  9(018).
               15  FILLER                       PIC  X(020).
               15  FILLER                       PIC  9(011).
               15  FILLER                       PIC  9(018).
               15  'XX'-ABW-KONTOINHABER          PIC  X(027).
               15  'XX'-WKN                       PIC  X(006).
      *               WERTPAPIERKENN-NUMMER
               15  'XX'-ISIN                      PIC  X(012).
               15  'XX'-SPARRATE                  PIC  9(007)V99.
      *               SPARLEISTUNG FUER SPARPLAENE
               15  'XX'-DYNAM                     PIC  9(002)V99.
      *               DYNAMISIERUNG FUER SPARPLAENE
               15  'XX'-ZAHLWEISE                 PIC  X(001).
      *               ZAHLWEISE......***0 : ZAHLWEISE FEHLT
      *               ZAHLWEISE........*1 : JAEHRLICH
      *               ZAHLWEISE........*2 : HALBJAEHRLICH
      *               ZAHLWEISE........*4 : VIERTELJAEHRLICH
      *               ZAHLWEISE........*5 : 2-MONATLICH
      *               ZAHLWEISE........*6 : MONATLICH
      *               ZAHLWEISE.........8 : EINMALPRAEMIE
               15  'XX'-TAG                       PIC  9(002).
      *               Tag im Monat für die Einzahlung
               15  'XX'-DATUM-VERTRAGSENDE.
                   20  'XX'-JAHR-VERTRAGSENDE        PIC  9(004).
                   20  'XX'-MONAT-VERTRAGSENDE       PIC  9(002).
                   20  'XX'-TAG-VERTRAGSENDE         PIC  9(002).
      *               Ende der Einzahlungsfrist bei VL-Verträgen
               15  'XX'-VERTRAGSTYP               PIC  X(002).
      *               Vertragstyp......0 : wenn Einmalanlage
      *               Vertragstyp......1 : wenn VL
      *               Vertragstyp......2 : wenn Sparplan
      *               Vertragstyp......3 : Entnahmeplan
      *               Vertragstyp......4 : Lebensphasenmodell
      *               Vertragstyp......5 : VermögensSparplan
               15  'XX'-BEGINN-RATENAUSSETZUNG    PIC  9(008).
      *               Beginndatum der Ratenaussetzung
               15  'XX'-ENDE-RATENAUSSETZUNG      PIC  9(008).
      *               Endedatum der Ratenaussetzung
               15  'XX'-DEL-KZ                    PIC  X(001).
      *               Löschkennzeichen
               15  'XX'-KOSTEN-PROZ               PIC 9(01)V99.
>>    *               Kosten in %
               15  'XX'-URSPR-GES-SPAR-LEIST      PIC 9(06)V99.
      *               Ursprüngliche Gesamtsparleistung in EUR
               15  'XX'-URSPR-SPAR-RATE           PIC 9(04)V99.
      *               Ursprüngliche Sparrate
               15  'XX'-AUSG-AUFSCHL-URSPR        PIC 9(02)V99.
      *               Ausgabeaufschlag des ursprünglichen Fonds
               15  'XX'-AUSG-AUFSCHL-AKTUELL      PIC 9(02)V99.
      *               Ausgabeaufschlag des aktuellen Fonds
               15  'XX'-VERTR-LAUFZEIT            PIC 9(02).
      *               Vertragslaufzeit
               15  'XX'-BEG-DYNAM                 PIC 9(08).
      *               Beginn Dynamisierung
               15  'XX'-ZAEHLER-LASTSCHR-RUECK    PIC 9(02).
      *               Zähler Lastschriftrückgaben
               15  'XX'-KND-MERKMAL               PIC X(01).
      *               Kündigungsmerkmal = 'K' bzw. ' '
               15  'XX'-DAT-VERTR-AUFLOESUNG      PIC 9(08).
      *               Datum Vertragsauflösung  bzw. vorzeitige
<<    *               Vertragskündigung
               15  'XX'-BONIFIKATION              PIC  9(003)V9(04).
               15  'XX'-URSPR-VERT-NR             PIC  X(012).
               15  'XX'-PIN                       PIC  9(009).
               15  'XX'-IBAN                      PIC  X(034).
>>    *            IBAN zur Lastschrift für den Rateneinzug

               15  'XX'-BIC                       PIC  X(011).
      *            BIC zur Lastschrift für den Rateneinzug

               15  'XX'-MANDREF                   PIC  X(035).
      *            Mandatsreferenz
    <<
               15  'XX'-RESERVE                   PIC  X(055).
      *        15  'XX'-RESERVE                   PIC  X(044).
      *        15  'XX'-RESERVE                   PIC  X(033).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(001).
      *            SATZART             'E'
               15  'XX'-N-ANZ              PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
      *        15  FILLER                PIC  X(292).
               15  FILLER                PIC  X(392).
      *            RESERVE




      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1508.CPY  $
      *    $Revision:   2.13  $
      *
      *    $Log:   L:/mf/CPY/DRKI1508.CPv  $
      *
      *   Rev 2.13   Mar 02 2020 12:52:14   t13ae52
      *AKI01578: ADBTool-Test
      *
      *   Rev 2.12   Oct 01 2018 09:34:12   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.11   Sep 24 2018 12:57:42   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.10   Sep 13 2018 09:56:40   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.9   Sep 10 2018 09:46:22   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.8   Sep 19 2017 10:04:00   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.7   Sep 15 2017 09:56:58   t13ex39
      *AKI01542: Erweitern Deuba Kreditkarten-DTA
      *
      *   Rev 2.6   Mar 09 2016 14:44:06   t13ex73
      *AAT00049: Fehlerverfahren mit Prozentkriterien - G48 PGMs
      *
      *   Rev 2.5   Sep 02 2013 11:52:56   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Kreditkarten
      *
      *   Rev 2.4   Nov 13 2009 09:30:26   t13ex14
      *AKI00881: Deutsche Bank: Kreditkarten-Datenlieferung Filial-
      *Nr.
      *
      *   Rev 2.3   Jul 31 2006 13:48:02   t13ex14
      *AKI00587: Deutsche Bank. Mahnstatus
      *
      *   Rev 2.2   Aug 18 2004 10:50:20   t13ex39
      *AKI00249: Änderungen/Ergänzungen zum Datenspiegel der Deutsc
      *hen Bank
      *
      *   Rev 2.1   Jul 13 2003 23:31:56   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:14   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Eingabedatei Änderungsbestand der       *
      *                Kreditkartendaten von der Deutschen Bank        *
      *                                                                *
170713*    SATZLÄNGE 200                                               *
010917*    SATZLÄNGE 250                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00249!14.07.04!      Löschkennzeichen 'D' ver-   !WHO        *
      *        !        !      arbeiten                    !           *
      *        !        !                                  !           *
      *AKI00587!01.08.06!      Mahnstatus aufgenommen      !Misgeld    *
      *        !        !                                  !           *
091009*AKI00881!09.10.09!      Filiale aufgenommen         !Misgeld    *
091009*        !        !                                  !           *
      *----------------------------------------------------------------*
170713*AKI01204!17.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
170713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
      *AAT00049!03.03.16! Umbau auf Replacing by XX        !DDY        *
      *        !        ! - ohne Vermerk in AREA A -       !           *
      *----------------------------------------------------------------*
010917*AKI01542!01.09.17! Aufhname PIN neue LREC=250       !WHO        *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI01601!03.07.18! Aufnahme Kreditkartenlimit       !Schumacher *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI01632!24.09.18! NEUE KREDITKARTEN                !SCHUMACHER *
      *----------------------------------------------------------------*
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1508.
      ******************************************************************
      *                                                                *
      *         HAUPTSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-SATZ.
      *
               15  'XX'-SA                        PIC X(002).
      *            SATZART        = '12'
               15  'XX'-GES                       PIC 9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-KEY.
                   20  'XX'-VNR.
                       25 'XX'-FILIALNUMMER       PIC 9(003).
                       25 'XX'-KUNDENNUMMER       PIC 9(007).
                   20  'XX'-KARTENART             PIC X(003).
      *                ECH - EUROCARD Hauptkarte
      *                VCH - VISA Hauptkarte
      *                ECZ - EUROCARD Zusatzkarte
      *                VCZ - VISA Zusatzkarte
      *                BBH - Schumikarte
      *         ***AB 24.09.2018***
      *                BCS - KREDITKARTE BUSINESSCARD
      *                DBU - DEUTSCHE BANK BUSINESSCARD DIRECT
      *                MGH - MASTERCARD GOLD HAUPTKARTE
      *                MPH - MASTERCARD PLATIN HAUPTKARTE
      *                TRV - MASTERCARD TRAVEL
      *                VGH - VISA GOLD HAUPTKARTE
 
                   20  'XX'-KREDITKARTEN-NR       PIC 9(016).
      *
               15  'XX'-HAUPTKARTEN-NR            PIC 9(016).
      *        Ist grundsätzlich bei Zusatzkarten belegt (Wird nicht
      *        übertragen sofern es sich um Firmen-Dummy-Karten handelt,
      *        Kartenart CTE und CTV)
      *
               15  'XX'-HAUPTKARTEN-VTN           PIC 9(010).
      *        Kann bei Haupt- und Zusatzkarten belegt sein, sofern die
      *        Karte einer anderen Kundenverbindung zugeordnet ist (z.B.
      *        bei Bevollmächtigten oder Firmenkarten) , ggf. ist bei
      *        Firmenkonten die Nummer mit Feld 3 identisch.
      *
               15  'XX'-ABRECHNUNGSKONTO-NR       PIC 9(010).
      *        Dieses Feld wird für alle Karten belegt.
      *        Kontonummer, über die die Abrechnung der Kreditkarte
      *        belastet wird, bei 'XX'-Kontonummer Darstellung mit
      *        führender Null, 7-stellige Kontonummer und 2-stellige
      *        Unterkontonummer. Hierüber kann ggf. ein Bezug zwischen
      *        Karte und Kontoverbindung innerhalb der DB hergestellt
      *        werden.
      *
               15  'XX'-ABRECHNUNGSKONTO-BLZ      PIC 9(008).
      *        Bankleitzahl der Abrechnungskonto-Nummer.
      *        Dieses Feld wird für alle Karten rechtsbündig mit der
      *        Filialnummer zu Feld 8 bei 'XX'-Verbindungen gefüllt,
091009****     !!! Das gilt nicht mehr. Siehe neues Feld FilialNr !!!
091009****
      *        ansonsten mit der Blz der Abrechnungskonto-Nummer
      *
               15  'XX'-DATUM-EROEFFNUNG.
                   20  'XX'-JAHR-EROEFFNUNG       PIC 9(004).
                   20  'XX'-MONAT-EROEFFNUNG      PIC 9(002).
                   20  'XX'-TAG-EROEFFNUNG        PIC 9(002).
      *
140704         15  'XX'-DEL-KZ                    PIC X(001).
140704*            Delete-Kennzeichen
 
               15  'XX'-MAHNSTATUS                PIC X(001).
 
091009         15  'XX'-FILIALNR                  PIC 9(003).
091009
170713         15  'XX'-IBAN                      PIC X(034).
>>    *            IBAN des Belastungskontos zur Lastschrift für
      *            den Einzug der Kartenabrechnung
 
               15  'XX'-BIC                       PIC X(011).
      *            BIC des Belastungskontos zur Lastschrift für
      *            den Einzug der Kartenabrechnung
 
               15  'XX'-MANDREF                   PIC X(035).
      *            Mandatsreferenz
 
               15  'XX'-NACHNAME-BZ               PIC X(028).
<<    *            Kontoinhaber des Belastungskontos zur Lastschrift
170713*            für den Einzug der Kartenabrechnung
010917         15  'XX'-PIN                       PIC 9(009).
 
      *            Kreditkartenlimit
030718         15  'XX'-LIMIT                     PIC 9(006)V99.
 
      *            Reserve
030718         15  'XX'-RESERVE                   PIC X(035).
      *
      ******************************************************************
      *                                                                *
      *        VORSATZ                                                 *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-VORSATZ REDEFINES 'XX'-SATZ.
               15  'XX'-V-SA                      PIC X(002).
      *            SATZART = '01'
               15  'XX'-V-ABSENDER                PIC X(008).
      *            ABSENDER = 'DB AG'
               15  'XX'-V-DATUM                   PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER              PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT                    PIC X(008).
      *            INHALTSANGABE:
      *            Komplettlieferung 'KKARTEK'
               15  'XX'-V-WKZ                     PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
010917         15  FILLER                         PIC X(217).
      *
      ******************************************************************
      *                                                                *
      *          NACHSATZ                                              *
      *                                                                *
      ******************************************************************
      *
           10  'XX'-NACHSATZ REDEFINES 'XX'-SATZ.
               15  'XX'-N-SA                      PIC  X(002).
      *            SATZART '99'
               15  'XX'-N-ANZ                     PIC  9(007).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
010917         15  FILLER                         PIC  X(241).
 
      *----------------------------------------------------------------*
 
 
 





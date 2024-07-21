      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1506.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI1506.CPv  $
      *
      *   Rev 2.6   Jan 26 2018 14:55:26   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.5   Feb 10 2016 08:31:50   t13ex73
      *AKI01417: Infosperre Deutsche Bank Kunden (DBO-1138)
      *
      *   Rev 2.4   Sep 02 2013 11:52:46   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Kreditkarten
      *
      *   Rev 2.3   Nov 13 2009 09:30:14   t13ex14
      *AKI00881: Deutsche Bank: Kreditkarten-Datenlieferung Filial-
      *Nr.
      *
      *   Rev 2.2   Jul 31 2006 13:47:48   t13ex14
      *AKI00587: Deutsche Bank. Mahnstatus
      *
      *   Rev 2.1   Jul 13 2003 23:33:12   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die KSDS-Datei Kreditkartendaten von der    *
      *                Deutschen Bank                                  *
      *                                                                *
      *    SATZLÄNGE 100                                               *
170713*    SATZLÄNGE 250                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.02.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00587!01.08.06 ! 1.2! Mahnstatus aufgenommen     !Misgeld    *
      *        !        !     !                            !           *
091009*AKI00881!09.10.09!     ! Filiale aufgenommen        !Misgeld    *
091009*        !        !     !                            !           *
      *----------------------------------------------------------------*
170713*AKI01204!17.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
170713*        !        !    !  UND DEN FILLER ANGEPASST   !           *
      *----------------------------------------------------------------*
010216*AKI01417!01.02.16! KSDS-DEL-KZ Feld einfuegen       ! DDY       *
      *----------------------------------------------------------------*
101217* AKI01556!10.12.17! PIN verarbeiten                ! WHO      *
      *         !        !                                !          *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1506.
      *
           05  KSDS-SA                        PIC X(002).
      *            SATZART        = '12'
           05  KSDS-GES                       PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-KEY.
               10  KSDS-VNR.
                   15 KSDS-FILIALNUMMER       PIC 9(003).
                   15 KSDS-KUNDENNUMMER       PIC 9(007).
               10  KSDS-KARTENART             PIC X(003).
      *            ECH - EUROCARD Hauptkarte
      *            VCH - VISA Hauptkarte
      *            ECZ - EUROCARD Zusatzkarte
      *            VCZ - VISA Zusatzkarte
      *            BBH - Schumikarte
      *
               10  KSDS-KREDITKARTEN-NR       PIC 9(016).
      *
           05  KSDS-HAUPTKARTEN-NR            PIC 9(016).
      *        Ist grundsätzlich bei Zusatzkarten belegt (Wird nicht
      *        übertragen sofern es sich um Firmen-Dummy-Karten handelt,
      *        Kartenart CTE und CTV)
      *
           05  KSDS-HAUPTKARTEN-VTN           PIC 9(010).
      *        Kann bei Haupt- und Zusatzkarten belegt sein, sofern die
      *        Karte einer anderen Kundenverbindung zugeordnet ist (z.B.
      *        bei Bevollmächtigten oder Firmenkarten) , ggf. ist bei
      *        Firmenkonten die Nummer mit Feld 3 identisch.
      *
           05  KSDS-ABRECHNUNGSKONTO-NR       PIC 9(010).
      *        Dieses Feld wird für alle Karten belegt.
      *        Kontonummer, über die die Abrechnung der Kreditkarte
      *        belastet wird, bei DB-Kontonummer Darstellung mit
      *        führender Null, 7-stellige Kontonummer und 2-stellige
      *        Unterkontonummer. Hierüber kann ggf. ein Bezug zwischen
      *        Karte und Kontoverbindung innerhalb der DB hergestellt
      *        werden.
      *
           05  KSDS-ABRECHNUNGSKONTO-BLZ      PIC 9(008).
      *        Bankleitzahl der Abrechnungskonto-Nummer.
      *        Dieses Feld wird für alle Karten rechtsbündig mit der
      *        Filialnummer zu Feld 8 bei DB-Verbindungen gefüllt,
091009****     !!! Das gilt nicht mehr. Siehe neues Feld FilialNr !!!
091009****
      *        ansonsten mit der Blz der Abrechnungskonto-Nummer
      *
           05  KSDS-DATUM-EROEFFNUNG.
               10  KSDS-JAHR-EROEFFNUNG       PIC  9(004).
               10  KSDS-MONAT-EROEFFNUNG      PIC  9(002).
               10  KSDS-TAG-EROEFFNUNG        PIC  9(002).
      *
      *-----------------------------------------------------------------
      *    Vertrag aus KSDS ist in dem geleiferten Bestand nicht mehr
      *    vorhanden. Das Schliessungsdatum wird auf das Tagesdatum
      *    gesetzt.
      *-----------------------------------------------------------------
           05  KSDS-DATUM-SCHLIESSUNG.
               10  KSDS-JAHR-SCHLIESSUNG      PIC  9(004).
               10  KSDS-MONAT-SCHLIESSUNG     PIC  9(002).
               10  KSDS-TAG-SCHLIESSUNG       PIC  9(002).
      *
010806     05  KSDS-MAHNSTATUS                PIC  X(001).
010806
091009     05  KSDS-FILIALNR                  PIC  9(003).
091009
170713     05  KSDS-IBAN                      PIC X(034).
>>    *        IBAN des Belastungskontos zur Lastschrift für
      *        den Einzug der Kartenabrechnung
 
           05  KSDS-BIC                       PIC X(011).
      *        BIC des Belastungskontos zur Lastschrift für
      *        den Einzug der Kartenabrechnung
 
           05  KSDS-MANDREF                   PIC X(035).
      *        Mandatsreferenz
 
           05  KSDS-NACHNAME-BZ               PIC X(028).
      *        Kontoinhaber des Belastungskontos zur Lastschrift
      *        für den Einzug der Kartenabrechnung
    <<
010216     05  KSDS-DEL-KZ                    PIC  X(001).
101217     05  KSDS-PIN                       PIC 9(009).
101217*    05  KSDS-RESERVE                   PIC X(044).
101217     05  KSDS-RESERVE                   PIC X(035).
      *            RESERVE


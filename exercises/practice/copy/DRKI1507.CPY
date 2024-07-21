      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1507.CPY  $
      *    $Revision:   2.10  $
      *
      *    $Log:   L:/mf/CPY/DRKI1507.CPv  $
      *
      *   Rev 2.10   Mar 02 2020 12:52:02   t13ae52
      *AKI01578: ADBTool-Test
      *
      *   Rev 2.9   Oct 01 2018 09:33:46   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.8   Sep 24 2018 12:57:28   t13ae52
      *AKI01632: G48 - Hinzufügen neuer Kreditkarten
      *
      *   Rev 2.7   Sep 13 2018 09:56:50   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.6   Sep 10 2018 09:46:56   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.5   Jan 25 2018 07:03:08   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs  KARTEN
      *
      *   Rev 2.4   Sep 02 2013 11:52:50   t13ex14
      *AKI01204: SEPA - DB-Online - DeuBa Kreditkarten
      *
      *   Rev 2.3   Nov 13 2009 09:30:20   t13ex14
      *AKI00881: Deutsche Bank: Kreditkarten-Datenlieferung Filial-
      *Nr.
      *
      *   Rev 2.2   Jul 31 2006 13:47:54   t13ex14
      *AKI00587: Deutsche Bank. Mahnstatus
      *
      *   Rev 2.1   Jul 13 2003 23:32:34   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.0   Jan 13 2003 15:48:08   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *    COPY-MEMBER für die Ausgabedatei Änderungsbestand der       *
      *                Kreditkartendaten von der Deutschen Bank        *
      *                                                                *
      *    SATZLÄNGE 100                                               *
010806*    SATZLÄNGE 120                                               *
170713*    SATZLÄNGE 260                                               *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00327!12.01.03! Neuerstellung                    !Golücke    *
      *----------------------------------------------------------------*
      *AKI00587 !01.08.06! 1.2! Mahnstatus aufgenommen     !Misgeld    *
      *         !        !    !                            !           *
091009*AKI00881 !09.10.09!    ! Filiale aufgenommen        !Misgeld    *
091009*         !        !    !                            !           *
      *----------------------------------------------------------------*
170713*AKI01204 !17.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI       *
170713*         !        !    !  UND DEN FILLER ANGEPASST  !           *
      *----------------------------------------------------------------*
101217* AKI01556!10.12.17!    !PIN verarbeiten             ! WHO       *
      *         !        !    !                            !           *
      *----------------------------------------------------------------*
      *AKI01601 !03.07.18!2.6 !Neues Feld:                 !Schumacher *
      *         !        !    !Kreditkartenlimit           !           *
      *----------------------------------------------------------------*
      *AKI01632 !24.09.18!2.8 !NEUE KREDITKARTEN           !SCHUMACHER *
      *----------------------------------------------------------------*
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  DRKI1507.
      *
           05  BNK-SA                                  PIC X(002).
      *            SATZART        = '12'
           05  BNK-GES                                 PIC 9(002).
      *            GESELLSCHAFT   = '48'
           05  BNK-KEY.
               10  BNK-VNR.
                   15 BNK-FILIALNUMMER                 PIC 9(003).
                   15 BNK-KUNDENNUMMER                 PIC 9(007).
               10  BNK-KARTENART                       PIC X(003).
      *            ECH - EUROCARD Hauptkarte
010806*            MCH - MASTERCARD Hauptkarte
      *            VCH - VISA Hauptkarte
      *            ECZ - EUROCARD Zusatzkarte
010806*            MCZ - MASTERCARD Zusatzkarte
      *            VCZ - VISA Zusatzkarte
      *            BBH - Schumikarte
010806*            MSK - Schumikarte
010806*            DVR - DVAG-Karte
      *        ***AB 24.09.2018***
      *            BCS - KREDITKARTE BUSINESSCARD
      *            DBU - DEUTSCHE BANK BUSINESSCARD DIRECT
      *            MGH - MASTERCARD GOLD HAUPTKARTE
      *            MPH - MASTERCARD PLATIN HAUPTKARTE
      *            TRV - MASTERCARD TRAVEL
      *            VGH - VISA GOLD HAUPTKARTE
      *
               10  BNK-KREDITKARTEN-NR                 PIC 9(016).
 
           05  BNK-VERARB-KZ                           PIC X(001).
      *            VERARBEITUNGSKENNZEICHEN
      *                I = INSERT
      *                U = UPDATE
      *                D = DELETE
      *
           05  BNK-HAUPTKARTEN-NR                      PIC 9(016).
      *        Ist grundsätzlich bei Zusatzkarten belegt (Wird nicht
      *        übertragen sofern es sich um Firmen-Dummy-Karten handelt,
      *        Kartenart CTE und CTV)
      *
           05  BNK-AEND-HAUPTKARTEN-NR-KZ              PIC X(001).
      *            J = Änderung Hauptkartennummer
      *
           05  BNK-HAUPTKARTEN-VTN                     PIC 9(010).
      *        Kann bei Haupt- und Zusatzkarten belegt sein, sofern die
      *        Karte einer anderen Kundenverbindung zugeordnet ist (z.B.
      *        bei Bevollmächtigten oder Firmenkarten) , ggf. ist bei
      *        Firmenkonten die Nummer mit Feld 3 identisch.
      *
           05  BNK-AEND-HAUPTKARTEN-VTN-KZ             PIC X(001).
      *            J = Änderung Hauptkartennummer
      *
           05  BNK-ABRECHNUNGSKONTO-NR                 PIC 9(010).
      *        Dieses Feld wird für alle Karten belegt.
      *        Kontonummer, über die die Abrechnung der Kreditkarte
      *        belastet wird, bei DB-Kontonummer Darstellung mit
      *        führender Null, 7-stellige Kontonummer und 2-stellige
      *        Unterkontonummer. Hierüber kann ggf. ein Bezug zwischen
      *        Karte und Kontoverbindung innerhalb der DB hergestellt
      *        werden.
      *
           05  BNK-AEND-ABBRECHKONTO-NR-KZ             PIC X(001).
      *            J = Änderung Abrechnungskontonummer
      *
           05  BNK-ABRECHNUNGSKONTO-BLZ                PIC 9(008).
      *        Bankleitzahl der Abrechnungskonto-Nummer.
      *        Dieses Feld wird für alle Karten rechtsbündig mit der
      *        Filialnummer zu Feld 8 bei DB-Verbindungen gefüllt,
091009****     !!! Das gilt nicht mehr. Siehe neues Feld FilialNr !!!
091009****
      *        ansonsten mit der Blz der Abrechnungskonto-Nummer
      *
           05  BNK-AEND-ABBRECHKONTO-BLZ-KZ            PIC X(001).
      *            J = Änderung Hauptkartennummer
      *
           05  BNK-DATUM-EROEFFNUNG.
               10 BNK-JAHR-EROEFNNUNG                  PIC 9(004).
               10 BNK-MONAT-EROEFFNUNG                 PIC 9(002).
               10 BNK-TAG-EROEFFNUNG                   PIC 9(002).
           05  BNK-AEND-DAT-E-KZ                       PIC X(001).
      *        J = Änderung Datum Eröffnung
      *
           05  BNK-DATUM-SCHLIESSUNG.
               10 BNK-JAHR-SCHLIESSUNG                 PIC 9(004).
               10 BNK-MONAT-SCHLIESSUNG                PIC 9(002).
               10 BNK-TAG-SCHLIESSUNG                  PIC 9(002).
           05  BNK-AEND-DAT-S-KZ                       PIC X(001).
      *        J = Änderung Datum Schliessung
      *
           05  BNK-LFDNR                               PIC 9(004).
010806
010806     05  BNK-MAHNSTATUS                          PIC X(001).
010806*        Belegung je nach Kartenstatus:
010806*        AKT - leer
010806*        KUK - leer
010806*        SUS - "S" (suspendiert)
010806*        MA1 - "1" (1. Mahnung)
010806*        MA2 - "2" (2. Mahnung)
010806     05  BNK-AEND-MAHNSTATUS-KZ                  PIC X(001).
010806*        J = Änderung Mahnstatus
 
091009     05  BNK-FILIALNR                            PIC 9(003).
091009     05  BNK-AEND-FILIALNR-KZ                    PIC X(001).
091009*        J = Änderung FilialNr
091009
170713     05  BNK-IBAN                                PIC X(034).
>>    *        IBAN des Belastungskontos zur Lastschrift für
      *        den Einzug der Kartenabrechnung
 
           05  BNK-AEND-IBAN-KZ                        PIC X(001).
      *        J WENN IBAN GEÄNDERT
 
           05  BNK-BIC                                 PIC X(011).
      *        BIC des Belastungskontos zur Lastschrift für
      *        den Einzug der Kartenabrechnung
 
           05  BNK-AEND-BIC-KZ                         PIC X(001).
      *        J WENN BIC GEÄNDERT
 
           05  BNK-MANDREF                             PIC X(035).
      *        Mandatsreferenz
 
           05  BNK-AEND-MANDREF-KZ                     PIC X(001).
      *        J WENN MANDATSREFERENZ GEÄNDERT
 
           05  BNK-NACHNAME-BZ                         PIC X(028).
      *        Kontoinhaber des Belastungskontos zur Lastschrift
      *        für den Einzug der Kartenabrechnung
 
           05  BNK-AEND-NACHNAME-BZ-KZ                 PIC X(001).
      *        J WENN KONTOINHABER GEÄNDERT
    <<
101217     05  BNK-PIN                       PIC  9(009).
101217     05  BNK-AEND-PIN-KZ               PIC  X(001).
 
030718*    Kreditkartenlimit
030718     05  BNK-LIMIT                     PIC  9(006)V99.
030718*    J wenn Kreditkartenlimit sich ändert
030718     05  BNK-AEND-LIMIT-KZ             PIC  X(001).
 
101217*    05  BNK-RESERVE                   PIC X(038).
101217*    05  BNK-RESERVE                   PIC X(028).
030718     05  BNK-RESERVE                   PIC X(019).
      *            RESERVE
 
 





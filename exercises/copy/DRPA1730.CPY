      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.01.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1730.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1730.CPv  $
      *
      *   Rev 2.2   Apr 25 2013 15:09:38   t13ae34
      *APA01464: SEPA - Erstellung SEPA-Überweisungs-DTA für MA-Abr
      *echnung
      *
      *   Rev 2.1   Mar 13 2013 17:42:04   t13ae34
      *APA01464: SEPA - Erstellung SEPA-Überweisungs-DTA für MA-Abr
      *echnung
      *
      *   Rev 2.0   Jan 28 2013 08:44:22   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *  DATENFORMAT DTAUS
      ****************************************************************
      *    COPY-MEMBER: DRPA1730                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01464!28.01.13!2.1 !NEU                        ! HEIDER   *
      * APA01464!22.04.13!2.2 !DETAILLIERTER VERW.ZWECK VB! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DATENTRÄGERVORSATZ
       01  DRPA1730-SATZART-A.
      *       SATZART = 'A'
           05 DRPA1730-A-SATZART             PIC  X(01) VALUE 'A'.
      *       KENNZEICHEN
           05 DRPA1730-A-KENNZEICHEN.
      *          'G'=GUTSCHRIFT, 'L'=LASTSCHRIFT
              10 DRPA1730-A-KENNZEICHEN-GL   PIC  X(01).
      *          'K'=KUNDENDATEI, 'B'=BANKDATEI
              10 DRPA1730-A-KENNZEICHEN-KB   PIC  X(01).
      *       BANKLEITZAHL DES KREDITINSTITUTS (DATEIEMPFÄNGERS)
           05 DRPA1730-A-BLZ-E               PIC  9(09) COMP-3.
      *       NUR BELEGT, WENN DATEIABSENDER KREDITINSTITUT IST
           05 FILLER                         PIC  9(09) COMP-3.
      *       DATEIABSENDER
           05 DRPA1730-A-KUNDENNAME          PIC  X(27).
      *       DATEIERSTELLUNGSDATUM FORMAT TTMMJJ
           05 DRPA1730-A-DATUM               PIC  9(07) COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(04) VALUE SPACE.
      *       KONTONUMMER. ÜBER DIESES KONTO WIRD GEGENWERT VERRECHNET.
      *       RECHTSBÜNDIG, NICHT BELEGTE STELLEN MIT 0 BELEGEN
           05 DRPA1730-A-KTO-E               PIC  9(11) COMP-3.
      *       REFERNZNUMMER, OPTINAL
           05 DRPA1730-A-REFERENZNUMMER      PIC  9(10).
      *       RESERVE
           05 FILLER                         PIC  X(15).
      *       AUSFÜHRUNGSDATUM FORMAT TTMMJHJJ, OPTIONAL
      *       WENN GEFÜLLT, MUSS ES JÜNGER ALS DATEIERSTELLUNG SEIN
           05 DRPA1730-A-DATUM-AUSFUEHRUNG   PIC  X(08).
      *       RESERVER
           05 FILLER                         PIC  X(58) VALUE SPACE.
      *       WÄHRUNGSKENNZEICHEN, KONSTANT = '1' ('EURO')
           05 DRPA1730-A-WKZ                 PIC  X(01) VALUE '1'.
      *
      *    ZAHLUNGSAUSTAUSCHSATZ
       01  DRPA1730-SATZART-C.
      *       SATZART = 'C'
           05 DRPA1730-C-SATZART             PIC  X(01) VALUE 'C'.
      *       BANKLEITZHL DES ERSTBETEILIGTE KREDITINSITUS, OPTIONAL
           05 DRPA1730-C-BLZ-ERSTBETEILIGT   PIC  9(09) COMP-3.
      *       BANKLEITZHL DES BEGÜNSTIGTEN/ZAHLSTELLE
           05 DRPA1730-C-BLZ                 PIC  9(09) COMP-3.
      *       KONTONUMMER DES BEGÜNSTIGTEN
           05 DRPA1730-C-KTO                 PIC  9(11) COMP-3.
      *       INTERNE KUNDENNUMMER
           05 DRPA1730-C-INTERNE-KDNR        PIC  9(11) COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  9(13) COMP-3.
      *       TEXTSCHLÜSEL
           05 DRPA1730-C-TEXTSCHLUESSEL      PIC  9(05) COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(01).
      *       BANKINTERNES FELD
           05 FILLER                         PIC  9(11) COMP-3.
      *       BANKLEITZAHL DES ÜBERWEISENDEN/ZAHLSTELLE
           05 DRPA1730-C-BLZ-UEB             PIC  9(09) COMP-3.
      *       KONTONUMMER DES ÜBERWEISENDEN
           05 DRPA1730-C-KTO-UEB             PIC  9(11) COMP-3.
      *       BETRAG
           05 DRPA1730-C-BETRAG              PIC S9(09)V99 COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(03).
      *       ZAHLUNGSEMPFÄNGER/BEGÜNSTIGTER
           05 DRPA1730-C-EMP                 PIC  X(27).
      *       ÜBERWEISENDER
           05 DRPA1730-C-UEB                 PIC  X(27).
      *       VERWENDUNGSZWECK
           05 DRPA1730-C-VERWENDUNG.
      *          VB-NR
              10 DRPA1730-C-VERWENDUNG-VB-NR PIC  X(07).
      *          VB-PZ
              10 DRPA1730-C-VERWENDUNG-VB-PZ PIC  X(01).
      *          REST
              10 DRPA1730-C-VERWENDUNG-REST  PIC  X(19).
      *       WÄHRUNGSKENNZEICHEN, KONSTANT = '1' ('EURO')
           05 DRPA1730-C-WKZ                 PIC  X(01) VALUE '1'.
      *       RESERVE
           05 FILLER                         PIC  X(02).
      *       ERWEITERUNGSKENNZEICHEN
           05 DRPA1730-C-ERWEITERUNGSKENNZ   PIC  9(03) COMP-3.
      *    DATEINACHSATZ
       01  DRPA1730-SATZART-E.
      *       SATZART = 'E'
           05 DRPA1730-E-SATZART             PIC  X(01) VALUE 'E'.
      *       RESERVE
           05 FILLER                         PIC  X(05).
      *       ANZAHL DER DATENSÄTZE 'C'
           05 DRPA1730-E-ANZAHL-C            PIC  9(07) COMP-3.
      *       RESERVE
           05 FILLER                         PIC  X(07).
      *       SUMME DER KONTONUMMERN AUS 'C'
           05 DRPA1730-E-SUMME-KTO-AUS-C     PIC  9(17) COMP-3.
      *       SUMME DER BANKLEITZAHLEN AUS 'C'
           05 DRPA1730-E-SUMME-BLZ-AUS-C     PIC  9(17) COMP-3.
      *       SUMME DER BETRÄGE AUS 'C'
           05 DRPA1730-E-SUMME-BETRAG-AUS-C  PIC S9(11)V99 COMP-3.
      *       RESERVE
           05 FILLER                         PIC  X(104).
      ******** ENDE COPY-MEMBER **************************************

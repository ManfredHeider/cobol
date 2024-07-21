      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1740.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1740.CPv  $
      *
      *   Rev 2.1   Apr 25 2013 15:09:44   t13ae34
      *APA01464: SEPA - Erstellung SEPA-Überweisungs-DTA für MA-Abr
      *echnung
      *
      *   Rev 2.0   Mar 14 2013 11:43:20   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *  DATENFORMAT DTAUS VON AACHEN FÜR SEPA MA-ABRECHNUNG
      ****************************************************************
      *    COPY-MEMBER: DRPA1740                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01464!14.03.13!2.1 !NEU                        ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DATENTRÄGERVORSATZ
       01  DRPA1740-SATZART-A.
      *       SATZART = 'A'
           05 DRPA1740-A-SATZART             PIC  X(01) VALUE 'A'.
      *       KENNZEICHEN
           05 DRPA1740-A-KENNZEICHEN.
      *          'G'=GUTSCHRIFT, 'L'=LASTSCHRIFT
              10 DRPA1740-A-KENNZEICHEN-GL   PIC  X(01).
      *          'K'=KUNDENDATEI, 'B'=BANKDATEI
              10 DRPA1740-A-KENNZEICHEN-KB   PIC  X(01).
      *       BIC DES KREDITINSTITUTS (DATEIEMPFÄNGERS)
           05 DRPA1740-A-BIC-E               PIC  X(11).
      *       NUR BELEGT, WENN DATEIABSENDER KREDITINSTITUT IST
           05 FILLER                         PIC  X(11).
      *       DATEIABSENDER
           05 DRPA1740-A-KUNDENNAME          PIC  X(27).
      *       DATEIERSTELLUNGSDATUM FORMAT TTMMJJ
           05 DRPA1740-A-DATUM               PIC  9(07) COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(04) VALUE SPACE.
      *       IBAN. ÜBER DIESES KONTO WIRD GEGENWERT VERRECHNET.
           05 DRPA1740-A-IBAN-E              PIC  X(34).
      *       REFERNZNUMMER, OPTINAL
           05 DRPA1740-A-REFERENZNUMMER      PIC  X(10).
      *       RESERVE
           05 FILLER                         PIC  X(15).
      *       AUSFÜHRUNGSDATUM FORMAT TTMMJHJJ, OPTIONAL
      *       WENN GEFÜLLT, MUSS ES JÜNGER ALS DATEIERSTELLUNG SEIN
           05 DRPA1740-A-DATUM-AUSFUEHRUNG   PIC  X(08).
      *       RESERVE
           05 FILLER                         PIC  X(58) VALUE SPACE.
      *       WÄHRUNGSKENNZEICHEN, KONSTANT = '1' ('EURO')
           05 DRPA1740-A-WKZ                 PIC  X(01) VALUE '1'.
      *
      *    ZAHLUNGSAUSTAUSCHSATZ
       01  DRPA1740-SATZART-C.
      *       SATZART = 'C'
           05 DRPA1740-C-SATZART             PIC  X(01) VALUE 'C'.
      *       BIC DES ERSTBETEILIGTE KREDITINSITUS, OPTIONAL
           05 DRPA1740-C-BIC-ERSTBETEILIGT   PIC  X(11).
      *       BIC DES BEGÜNSTIGTEN/ZAHLSTELLE
           05 DRPA1740-C-BIC                 PIC  X(11).
      *       IBAN DES BEGÜNSTIGTEN
           05 DRPA1740-C-IBAN                PIC  X(34).
      *       INTERNE KUNDENNUMMER
           05 DRPA1740-C-INTERNE-KDNR        PIC  X(06).
      *       BANKINTERNES FELD
           05 FILLER                         PIC  9(13) COMP-3.
      *       TEXTSCHLÜSEL
           05 DRPA1740-C-TEXTSCHLUESSEL      PIC  9(05) COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(01).
      *       BIC DES ÜBERWEISENDEN/ZAHLSTELLE
           05 DRPA1740-C-BIC-UEB             PIC  X(11).
      *       IBAN DES ÜBERWEISENDEN
           05 DRPA1740-C-IBAN-UEB            PIC  X(34).
      *       BETRAG
           05 DRPA1740-C-BETRAG              PIC S9(09)V99 COMP-3.
      *       BANKINTERNES FELD
           05 FILLER                         PIC  X(03).
      *       ZAHLUNGSEMPFÄNGER/BEGÜNSTIGTER
           05 DRPA1740-C-EMP                 PIC  X(27).
      *       ÜBERWEISENDER
           05 DRPA1740-C-UEB                 PIC  X(27).
      *       VERWENDUNGSZWECK
           05 DRPA1740-C-VERWENDUNG.
      *          VB-NR
              10 DRPA1740-C-VERWENDUNG-VB-NR PIC  X(07).
      *          VB-PZ
              10 DRPA1740-C-VERWENDUNG-VB-PZ PIC  X(01).
      *          REST
              10 DRPA1740-C-VERWENDUNG-REST  PIC  X(19).
      *       WÄHRUNGSKENNZEICHEN, KONSTANT = '1' ('EURO')
           05 DRPA1740-C-WKZ                 PIC  X(01) VALUE '1'.
      *       RESERVE
           05 FILLER                         PIC  X(02).
      *       ERWEITERUNGSKENNZEICHEN
           05 DRPA1740-C-ERWEITERUNGSKENNZ   PIC  9(03) COMP-3.
      *    DATEINACHSATZ
       01  DRPA1740-SATZART-E.
      *       SATZART = 'E'
           05 DRPA1740-E-SATZART             PIC  X(01) VALUE 'E'.
      *       RESERVE
           05 FILLER                         PIC  X(05).
      *       ANZAHL DER DATENSÄTZE 'C'
           05 DRPA1740-E-ANZAHL-C            PIC  9(07) COMP-3.
      *       ÜBERWEISUNGSSUMME
           05 DRPA1740-E-SUMME-BETRAG-AUS-C  PIC S9(11)V99 COMP-3.
      ******** ENDE COPY-MEMBER **************************************

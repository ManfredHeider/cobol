      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  04.12.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0350.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0350.CPv  $
      *
      *   Rev 2.1   May 02 2019 15:17:14   t13ae23
      *AAN01415: PKS - DSL Bank - Verarbeitung eingereichtes Geschäft
      *
      *   Rev 2.0   Dec 04 2018 15:57:52   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * DSL-BANK (67) SST AUTOMATISCHE MASCH. NACHERFASSUNG
      ****************************************************************
      *    COPY-MEMBER: DRAN0350                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01415!        !2.1 !NEU                        ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES67.
           05 ESATZ-GES67-GESAMT                    PIC X(350).
      *
           05 GES67-VORSATZ REDEFINES ESATZ-GES67-GESAMT.
      **** SATZKENNZEICHEN
              10 GES67-VORSATZ-SATZ-KZ              PIC X(001).
      **** ABSENDER
              10 GES67-VORSATZ-ABSENDER             PIC 9(004).
      **** DTA-NAME
              10 GES67-VORSATZ-DTA-NAME             PIC X(010).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES67-VORSATZ-DAT-BEREITST         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(325).
      *
           05 GES67-ANTRAGSATZ REDEFINES ESATZ-GES67-GESAMT.
      **** SATZKENNZEICHEN
              10 GES67-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** DSL-BANK-VERTRAGSNUMMER
              10 GES67-ANTRAGSATZ-DSL-VNR           PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES67-ANTRAGSATZ-AVERM             PIC 9(007).
      **** KUNDE NAME
              10 GES67-ANTRAGSATZ-KUNDE-NAME        PIC X(030).
      **** KUNDE VORNAME
              10 GES67-ANTRAGSATZ-KUNDE-VNAME       PIC X(030).
      **** DVAG-PRODUKT-SATZ
              10 GES67-ANTRAGSATZ-PRODUKTE OCCURS 9.
      **** DVAG-PRODUKT
                 15 GES67-ANTRAGSATZ-PRODUKT        PIC 9(004).
      **** DVAG-VERTRAGSSUMME
                 15 GES67-ANTRAGSATZ-SUMME          PIC S9(009).
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES67-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** VM (VORGESEHEN IN SPÄTEREN VERSION)
      *       10 GES67-ANTRAGSATZ-VM                PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(140).

           05 GES67-NACHSATZZ REDEFINES ESATZ-GES67-GESAMT.
      **** SATZKENNZEICHEN
              10 GES67-NACHSATZ-SATZ-KZ             PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES67-NACHSATZ-SATZANZAHL          PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(342).

      ******** ENDE COPY-MEMBER **************************************


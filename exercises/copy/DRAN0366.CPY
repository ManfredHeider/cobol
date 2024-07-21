      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0366.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0366.CPv  $
      *
      *   Rev 2.2   Feb 26 2020 10:44:40   u130377
      *AAN01440: PK Smart: Integration der Santander Consumer Bank
      *
      *   Rev 2.1   Nov 25 2019 16:45:38   u130377
      *AAN01440: PK Smart: Integration der Santander Consumer Bank
      *   Rev 2.1   May 02 2019 15:17:14   t13ae23
      *   AAN01440: Santander Consumer Bank (66)
      *             - Verarbeitung eingereichtes Geschäft
      *
      *   Rev 2.0   Nov 21 2019 13:05:22   u130377
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      * Santander Consumer BANK (66)                                 *
      * AUTOMATISCHE MASCH. NACHERFASSUNG                            *
      ****************************************************************
      *    COPY-MEMBER: DRAN0366                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01440!22.11.19!2.1 !NEU                        ! Töpfer   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES66.
           05 ESATZ-GES66-GESAMT                    PIC X(350).
      *
           05 GES66-VORSATZ REDEFINES ESATZ-GES66-GESAMT.
      **** SATZKENNZEICHEN
              10 GES66-VORSATZ-SATZ-KZ              PIC X(001).
      **** ABSENDER
              10 GES66-VORSATZ-ABSENDER             PIC 9(004).
      **** DTA-NAME
              10 GES66-VORSATZ-DTA-NAME             PIC X(010).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES66-VORSATZ-DAT-BEREITST         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(325).
      *
           05 GES66-ANTRAGSATZ REDEFINES ESATZ-GES66-GESAMT.
      **** SATZKENNZEICHEN
              10 GES66-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** DSL-BANK-VERTRAGSNUMMER
              10 GES66-ANTRAGSATZ-SCB-VNR           PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES66-ANTRAGSATZ-AVERM             PIC 9(007).
      **** VERTRAUENSMANN VM
              10 GES66-ANTRAGSATZ-VM                PIC 9(007).
      **** KUNDE NAME
              10 GES66-ANTRAGSATZ-KUNDE-NAME        PIC X(030).
      **** KUNDE VORNAME
              10 GES66-ANTRAGSATZ-KUNDE-VNAME       PIC X(030).
      **** DVAG-PRODUKT-SATZ
              10 GES66-ANTRAGSATZ-PRODUKTE OCCURS 9.
      **** DVAG-PRODUKT
                 15 GES66-ANTRAGSATZ-PRODUKT        PIC 9(004).
      **** DVAG-VERTRAGSSUMME
                 15 GES66-ANTRAGSATZ-SUMME          PIC S9(009).
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES66-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** VM (VORGESEHEN IN SPÄTEREN VERSION)
      *       10 GES66-ANTRAGSATZ-VM                PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(133).

           05 GES66-NACHSATZZ REDEFINES ESATZ-GES66-GESAMT.
      **** SATZKENNZEICHEN
              10 GES66-NACHSATZ-SATZ-KZ             PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES66-NACHSATZ-SATZANZAHL          PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(342).

      ******** ENDE COPY-MEMBER **************************************




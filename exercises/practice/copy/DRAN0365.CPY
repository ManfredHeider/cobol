      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0365.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRAN0365.CPv  $
      *
      *   Rev 2.4   May 07 2020 15:30:48   u130377
      *AAN01433: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB - Masch. Nacherfassen -
      *
      *   Rev 2.3   May 06 2020 12:21:12   u130377
      *AAN01433: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB - Masch. Nacherfassen -
      *
      *   Rev 2.2   Nov 22 2019 12:35:38   u130377
      *AAN01433: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB - Masch. Nacherfassen -
      *
      *   Rev 2.1   Nov 20 2019 15:03:12   u130377
      *AAN01433: PlanetHome- Anbindung der Plattform zur Immobilien
      *vermittlung durch den VB - Masch. Nacherfassen -
      *
      *   Rev 2.0   Oct 25 2019 09:48:28   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * PlanetHome(92) SST AUTOMATISCHE MASCH. NACHERFASSUNG
      ****************************************************************
      *    COPY-MEMBER: DRAN0365                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01432!25.10.19!2.1 !NEU                        ! Töpfer   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES92.
           05 ESATZ-GES92-GESAMT                    PIC X(200).
      *
           05 GES92-VORSATZ REDEFINES ESATZ-GES92-GESAMT.
      **** SATZKENNZEICHEN
              10 GES92-VORSATZ-SATZ-KZ              PIC X(001).
      **** ABSENDER
              10 GES92-VORSATZ-ABSENDER             PIC 9(004).
      **** DTA-NAME
              10 GES92-VORSATZ-DTA-NAME             PIC X(010).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES92-VORSATZ-DAT-BEREITST         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(175).
      *
           05 GES92-ANTRAGSATZ REDEFINES ESATZ-GES92-GESAMT.
      **** SATZKENNZEICHEN
              10 GES92-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** Version
              10 GES92-ANTRAGSATZ-VERSION           PIC X(004).
      **** DVAG-HYP-VERTRAGSNUMMER
              10 GES92-ANTRAGSATZ-DSL-VNR           PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES92-ANTRAGSATZ-AVERM             PIC 9(007).
      **** ABSCHLUSSVERMITTLER-VM
              10 GES92-ANTRAGSATZ-AVERM-VM          PIC 9(007).
      **** KUNDE NAME
              10 GES92-ANTRAGSATZ-KUNDE-NAME        PIC X(030).
      **** KUNDE VORNAME
              10 GES92-ANTRAGSATZ-KUNDE-VNAME       PIC X(030).
      **** KUNDE PLZ
              10 GES92-ANTRAGSATZ-KUNDE-PLZ         PIC X(009).
      **** KUNDE WOHNORT
              10 GES92-ANTRAGSATZ-KUNDE-ORT         PIC X(030).
      **** KUNDE LÄNDERKENNZEICHEN
              10 GES92-ANTRAGSATZ-KUNDE-AUSL        PIC X(003).
      **** DVAG-PRODUKT
              10 GES92-ANTRAGSATZ-PRODUKT           PIC 9(004).
      **** DVAG-VERTRAGSSUMME
              10 GES92-ANTRAGSATZ-BETRAG            PIC S9(09)V99.
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES92-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(038).

           05 GES92-NACHSATZ REDEFINES ESATZ-GES92-GESAMT.
      **** SATZKENNZEICHEN
              10 GES92-NACHSATZ-SATZ-KZ             PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES92-NACHSATZ-SATZANZAHL          PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(192).

      ******** ENDE COPY-MEMBER **************************************






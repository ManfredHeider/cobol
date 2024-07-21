      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0300.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0300.CPv  $
      *
      *   Rev 2.1   Jan 04 2017 15:42:08   t13ae23
      *AAN01299: Neuer Produktpartner TeleSon 
      *
      *   Rev 2.0   Jun 25 2015 08:34:58   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * TELESON SST AUTOMATISCHE PIN-VERGABE DURCH TELESON (GES. 94)
      ****************************************************************
      *    COPY-MEMBER: DRAN0300                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01299!21.11.16!2.1 !NEU                        ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES94.
           05 ESATZ-GES94-GESAMT                    PIC X(401).
      *
           05 GES94-VORSATZ REDEFINES ESATZ-GES94-GESAMT.
      **** SATZKENNZEICHEN
              10 GES94-VORSATZ-SATZ-KZ              PIC X(001).
      **** GESELLSCHAFT
              10 GES94-VORSATZ-GESLLSCHAFT          PIC 9(004).
      **** DTA-NAME
              10 GES94-VORSATZ-DTA-NAME             PIC X(020).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES94-VORSATZ-DAT-BEREITST         PIC X(010).
      **** ANZAHL SÄTZE INKL. VORSATZ
              10 GES94-VORSATZ-SATZANZAHL           PIC 9(006).
      **** FILLER
              10 FILLER                             PIC X(360).
      *
           05 GES94-ANTRAGSATZ REDEFINES ESATZ-GES94-GESAMT.
      **** SATZKENNZEICHEN
              10 GES94-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** PIN
              10 GES94-ANTRAGSATZ-DVAG-VNR          PIC 9(011).
      **** ABSCHLUSSVERMITTLER
              10 GES94-ANTRAGSATZ-AVERM             PIC 9(007).
      **** VM
              10 GES94-ANTRAGSATZ-VM                PIC 9(007).
      **** DVAG-PRODUKTSCHLÜSSEL
              10 GES94-ANTRAGSATZ-PROD-SCHL         PIC 9(004).
      **** DVAG-VERTRAGSSUMME-1
              10 GES94-ANTRAGSATZ-BETRAG            PIC 9(009).
      **** DVAG-TARIFBEZEICHNUNG
              10 GES94-ANTRAGSATZ-TARIF             PIC X(015).
      **** KUNDE NAME
              10 GES94-ANTRAGSATZ-KUNDE-NAME        PIC X(026).
      **** KUNDE VORNAME
              10 GES94-ANTRAGSATZ-KUNDE-VNAME       PIC X(026).
      **** KUNDE PLZ
              10 GES94-ANTRAGSATZ-PLZ               PIC X(009).
      **** KUNDE ORT
              10 GES94-ANTRAGSATZ-ORT               PIC X(021).
      **** DATUM UNTERSCHRIFT KUNDE (ANTRAG-DATUM) FORMAT TT.MM.JHJJ
              10 GES94-ANTRAGSATZ-ANTRAG-DATUM      PIC X(010).
      **** DATUM ERFASSUNG BEI TELESON (ERFASSUNGS-DATUM)
      **** FORMAT TT.MM.JHJJ
              10 GES94-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** ANTRAGSART
              10 GES94-ANTRAGSATZ-ANTRAGS-ART       PIC X(001).
      **** FILLER
              10 FILLER                             PIC X(244).
      *
      ******** ENDE COPY-MEMBER **************************************


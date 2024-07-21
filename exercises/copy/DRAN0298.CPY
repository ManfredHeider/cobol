      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0298.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0298.CPv  $
      *
      *   Rev 2.2   Feb 28 2017 10:06:52   t13ae23
      *AAN01297: BKK-Linde als eigenständige Gesellschaft
      *
      *   Rev 2.1   Jan 06 2017 08:35:32   t13ae23
      *AAN01297: BKK-Linde als eigenständige Gesellschaft
      *
      *   Rev 2.0   Jun 25 2015 08:34:42   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * BKK-LINDE SST AUTOMATISCHE PIN-VERGABE DURCH BKK-LINDE
      ****************************************************************
      *    COPY-MEMBER: DRAN0298                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01297!16.11.16!2.1 !NEU                        ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES51.
           05 ESATZ-GES51-GESAMT                    PIC X(300).
      *
           05 GES51-VORSATZ REDEFINES ESATZ-GES51-GESAMT.
      **** SATZKENNZEICHEN
              10 GES51-VORSATZ-SATZ-KZ              PIC X(001).
      **** DTA-NAME
              10 GES51-VORSATZ-DTA-NAME             PIC X(020).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES51-VORSATZ-DAT-BEREITST         PIC X(010).
      **** ANZAHL SÄTZE INKL. VORSATZ
              10 GES51-VORSATZ-SATZANZAHL           PIC 9(006).
      **** FILLER
              10 FILLER                             PIC X(263).
      *
           05 GES51-ANTRAGSATZ REDEFINES ESATZ-GES51-GESAMT.
      **** SATZKENNZEICHEN
              10 GES51-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** PIN
              10 GES51-ANTRAGSATZ-DVAG-PIN          PIC 9(011).
      **** BKK-VERTRAGSNUMMER
              10 GES51-ANTRAGSATZ-BKK-VNR           PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES51-ANTRAGSATZ-AVERM             PIC 9(007).
      **** VM
              10 GES51-ANTRAGSATZ-VM                PIC 9(007).
      **** DVAG-PRODUKTSCHLÜSSEL
              10 GES51-ANTRAGSATZ-PROD-SCHL         PIC 9(004).
      **** DVAG-VERTRAGSSUMME-1
              10 GES51-ANTRAGSATZ-BETRAG            PIC S9(009).
      **** KUNDE NAME
              10 GES51-ANTRAGSATZ-KUNDE-NAME        PIC X(026).
      **** KUNDE VORNAME
              10 GES51-ANTRAGSATZ-KUNDE-VNAME       PIC X(026).
      **** KUNDE PLZ
              10 GES51-ANTRAGSATZ-PLZ               PIC X(009).
      **** KUNDE ORT
              10 GES51-ANTRAGSATZ-ORT               PIC X(024).
      **** DATUM UNTERSCHRIFT KUNDE (ANTRAG-DATUM) FORMAT TT.MM.JHJJ
              10 GES51-ANTRAGSATZ-ANTRAG-DATUM      PIC X(010).
      **** DATUM ERFASSUNG BEI BKK (ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES51-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** ANTRAGSART (SIEHE KTY=217)
              10 GES51-ANTRAGSATZ-ANTRAGS-ART       PIC X(001).
      *
              10 FILLER                             PIC X(140).
      *
      ******** ENDE COPY-MEMBER **************************************



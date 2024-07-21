      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0370.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0370.CPv  $
      *
      *   Rev 2.1   Apr 09 2020 16:01:34   u130377
      *AAN01470: Deutsche Verrechnungsstelle - Anbindung der TeamFa
      *ctor Plattform (Antrags-DTA)
      *
      *   Rev 2.0   Mar 27 2020 09:13:06   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * Deutsche Verechnungsstelle TEAMFAKTOR (90)                   *
      * AUTOMATISCHE MASCH. NACHERFASSUNG                            *
      ****************************************************************
      * COPY-MEMBER: DRAN0370                                        *
      * SATZBESCHREIBUNG                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01470!27.03.20!2.1 !NEU                        ! Töpfer   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES90.
           05 ESATZ-GES90-GESAMT                    PIC X(350).
      *
           05 GES90-VORSATZ REDEFINES ESATZ-GES90-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-VORSATZ-SATZ-KZ              PIC X(001).
      **** ABSENDER
              10 GES90-VORSATZ-ABSENDER             PIC 9(004).
      **** DTA-NAME
              10 GES90-VORSATZ-DTA-NAME             PIC X(010).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES90-VORSATZ-DAT-BEREITST         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(325).
      *
           05 GES90-ANTRAGSATZ REDEFINES ESATZ-GES90-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** DSL-BANK-VERTRAGSNUMMER
              10 GES90-ANTRAGSATZ-SCB-VNR           PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES90-ANTRAGSATZ-AVERM             PIC 9(007).
      **** VERTRAUENSMANN VM
              10 GES90-ANTRAGSATZ-VM                PIC 9(007).
      **** KUNDE NAME
              10 GES90-ANTRAGSATZ-KUNDE-NAME        PIC X(030).
      **** KUNDE VORNAME
              10 GES90-ANTRAGSATZ-KUNDE-VNAME       PIC X(030).
      **** KUNDE PLZ
              10 GES90-ANTRAGSATZ-KUNDE-PLZ         PIC X(009).
      **** KUNDE ORT
              10 GES90-ANTRAGSATZ-KUNDE-ORT         PIC X(030).
      **** KUNDE LKZ
              10 GES90-ANTRAGSATZ-KUNDE-LKZ         PIC X(003).
      **** DVAG-PRODUKT-SATZ
              10 GES90-ANTRAGSATZ-PRODUKTE OCCURS 9.
      **** DVAG-PRODUKT
                 15 GES90-ANTRAGSATZ-PRODUKT        PIC 9(004).
      **** DVAG-VERTRAGSSUMME
                 15 GES90-ANTRAGSATZ-SUMME          PIC S9(012).
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES90-ANTRAGSATZ-ERF-DATUM         PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(64).

           05 GES90-NACHSATZZ REDEFINES ESATZ-GES90-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-NACHSATZ-SATZ-KZ             PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES90-NACHSATZ-SATZANZAHL          PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(342).

      ******** ENDE COPY-MEMBER **************************************




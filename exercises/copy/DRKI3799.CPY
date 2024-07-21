      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3799.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3799.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:24:00   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 15 2020 13:53:08   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    G50/GEDKRANKEN
      *    AKTUALISIERUNGSDATEN AUS ERMITTLUNG UND AUFBEREITUNG
      *    FÜR AKTION(S-TOOL) - VERTRAG
      *                         (GDKAKTVERTRAG)
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01784!16.06.20!2.1 !NEU                        !T13AC20
      *         !        !    !                           !
      *****************************************************************
      *
           05 DRKI3799.
              10 GES                             PIC  X(002).
              10 VNR                             PIC  X(015).
      *
              10 ANGEBOTSNAME                    PIC  X(010).
              10 VERTRAGSID                      PIC  X(013).
              10 VERTRAGSNUMMER                  PIC  X(015).
              10 UMST-TERMIN                     PIC  X(010).
              10 NACHNAME-VN                     PIC  X(040).
              10 VORNAME-VN                      PIC  X(030).
              10 ANZ-VP                          PIC S9(004) BINARY.
              10 ANNAHME-BIS                     PIC  X(010).
              10 AUFBEWAHR-BIS                   PIC  X(010).


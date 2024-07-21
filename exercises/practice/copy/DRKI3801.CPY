      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3801.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3801.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:25:22   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 15 2020 13:54:52   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    G50/GEDKRANKEN
      *    AKTUALISIERUNGSDATEN AUS ERMITTLUNG UND AUFBEREITUNG
      *    FÜR AKTION(S-TOOL) - TARIFE OHNE UMSTELLUNG
      *                         (GDKAKTTARIFOU)
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
           05 DRKI3801.
              10 GES                             PIC  X(002).
              10 VNR                             PIC  X(015).
      *
              10 ANGEBOTSNAME                    PIC  X(010).
              10 VERTRAGSID                      PIC  X(013).
              10 PERSONLFDNR                     PIC S9(004) BINARY.
              10 TARIFTEIL-ID                    PIC  X(010).
              10 STICHTAG                        PIC  X(010).
              10 TARIFNAME                       PIC  X(040).
              10 GESAMT-BEITRAG                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 SELBSTBEHALT                    PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 VERS-LEISTUNG                   PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 GESETZL-ZUSCHL                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 ZEITL-BEFR-LIM                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 SUM-BEITRZUSCHL                 PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 PAUSCH-LEIST-1                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 PAUSCH-LEIST-2                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 PAUSCH-LEIST-3                  PIC S9(007)V99
                                                        PACKED-DECIMAL.
              10 PAUSCH-LEIST-4                  PIC S9(007)V99
                                                        PACKED-DECIMAL.


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3836.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3836.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:30:46   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 25 2020 09:13:32   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1543
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * AKI01784!25.06.20!NEU                              !T13AC20
      *         !        !                                 !
      *         !        !                                 !
      *         !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *
       01  DRKI3836-RESTARTINFO.
           05 DRKI3836-ID.
              10 FILLER                          PIC  X(008)
                                                      VALUE 'DRKI3836'.
      *
           05 DRKI3836-KEY.
              10 DRKI3836-ANGEBOTSNAME           PIC  X(010).
              10 DRKI3836-VERTRAGSID             PIC  X(013).
              10 DRKI3836-PERSONLFDNR            PIC S9(004)
                                                        BINARY SYNC.
      *
           05 DRKI3836-ZAEHLER PACKED-DECIMAL.
              10 DRKI3836-EZ-AKPEIN              PIC S9(009).
              10 DRKI3836-EZ-AKPEIN-INS          PIC S9(009).
              10 DRKI3836-AZ-AKPFEHL             PIC S9(009).
              10 DRKI3836-VZ-AKP-INS             PIC S9(009).


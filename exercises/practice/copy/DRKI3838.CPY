      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3838.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3838.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:32:06   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 25 2020 09:15:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1545
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
       01  DRKI3838-RESTARTINFO.
           05 DRKI3838-ID.
              10 FILLER                          PIC  X(008)
                                                      VALUE 'DRKI3838'.
      *
           05 DRKI3838-KEY.
              10 DRKI3838-ANGEBOTSNAME           PIC  X(010).
              10 DRKI3838-VERTRAGSID             PIC  X(013).
              10 DRKI3838-PERSONLFDNR            PIC S9(004)
                                                        BINARY SYNC.
              10 DRKI3838-TARIFTEIL-ID           PIC  X(010).
              10 DRKI3838-ANGEBOTSNR             PIC S9(004)
                                                        BINARY SYNC.
      *
           05 DRKI3838-ZAEHLER PACKED-DECIMAL.
              10 DRKI3838-EZ-AKMEIN              PIC S9(009).
              10 DRKI3838-EZ-AKMEIN-INS          PIC S9(009).
              10 DRKI3838-AZ-AKMFEHL             PIC S9(009).
              10 DRKI3838-VZ-AKM-INS             PIC S9(009).


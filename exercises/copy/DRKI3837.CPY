      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3837.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3837.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:31:26   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 25 2020 09:14:28   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1544
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
       01  DRKI3837-RESTARTINFO.
           05 DRKI3837-ID.
              10 FILLER                          PIC  X(008)
                                                      VALUE 'DRKI3837'.
      *
           05 DRKI3837-KEY.
              10 DRKI3837-ANGEBOTSNAME           PIC  X(010).
              10 DRKI3837-VERTRAGSID             PIC  X(013).
              10 DRKI3837-PERSONLFDNR            PIC S9(004)
                                                        BINARY SYNC.
              10 DRKI3837-TARIFTEIL-ID           PIC  X(010).
      *
           05 DRKI3837-ZAEHLER PACKED-DECIMAL.
              10 DRKI3837-EZ-AKOEIN              PIC S9(009).
              10 DRKI3837-EZ-AKOEIN-INS          PIC S9(009).
              10 DRKI3837-AZ-AKOFEHL             PIC S9(009).
              10 DRKI3837-VZ-AKO-INS             PIC S9(009).


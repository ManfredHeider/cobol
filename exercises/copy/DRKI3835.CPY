      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3835.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3835.CPv  $
      *
      *   Rev 2.1   Jul 01 2020 10:30:06   t13ac20
      *AKI01784: Central: Neue Schnittstelle für zusätzliche "Aktio
      *nen" im Bordero-Tool - Source
      *
      *   Rev 2.0   Jun 25 2020 09:12:36   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1542
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
       01  DRKI3835-RESTARTINFO.
           05 DRKI3835-ID.
              10 FILLER                          PIC  X(008)
                                                      VALUE 'DRKI3835'.
      *
           05 DRKI3835-KEY.
              10 DRKI3835-ANGEBOTSNAME           PIC  X(010).
              10 DRKI3835-VERTRAGSID             PIC  X(013).
      *
           05 DRKI3835-ZAEHLER PACKED-DECIMAL.
              10 DRKI3835-EZ-AKVEIN              PIC S9(009).
              10 DRKI3835-EZ-AKVEIN-INS          PIC S9(009).
              10 DRKI3835-AZ-AKVFEHL             PIC S9(009).
              10 DRKI3835-VZ-AKV-INS             PIC S9(009).


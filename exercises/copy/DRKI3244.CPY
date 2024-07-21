      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3244.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3244.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 09:22:44   t13ac17
      *AKI01534: G20 - Projekt LENA: Vertragsteil
      *
      *   Rev 2.0   Dec 28 2017 12:37:56   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1360
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!28.12.17!2.1 ! NEUERSTELLUNG             ! Walbert
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3244-RESTARTINFO.
           05 DRKI3244-KEY.
              10 DRKI3244-VNR                    PIC  X(015).
              10 DRKI3244-LFDNR                  PIC  9(004).
           05 DRKI3244-ZAEHLER PACKED-DECIMAL.
              10 DRKI3244-EZ-LPTEIN              PIC S9(009).
              10 DRKI3244-EZ-LPTEIN-INS          PIC S9(009).
              10 DRKI3244-EZ-LPTEIN-UPD          PIC S9(009).
              10 DRKI3244-AZ-LPTFEHL             PIC S9(009).
              10 DRKI3244-VZ-VER-SEL-VERTID      PIC S9(009).
              10 DRKI3244-VZ-LPT-INS             PIC S9(009).
              10 DRKI3244-VZ-LPT-UPD             PIC S9(009).


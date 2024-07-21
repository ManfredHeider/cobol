      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3224.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3224.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 07:58:00   t13ac20
      *AKI01534: G20 - Projekt LENA: Vertrag
      *
      *   Rev 2.0   Dec 07 2017 16:16:24   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1350
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!07.12.17!2.1 ! NEUERSTELLUNG             ! SWO
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3224-RESTARTINFO.
           05 DRKI3224-KEY.
              10 DRKI3224-VNR                    PIC  X(015).
           05 DRKI3224-ZAEHLER PACKED-DECIMAL.
              10 DRKI3224-EZ-LPVEIN              PIC S9(009).
              10 DRKI3224-EZ-LPVEIN-INS          PIC S9(009).
              10 DRKI3224-EZ-LPVEIN-UPD          PIC S9(009).
              10 DRKI3224-EZ-LPVEIN-DEL          PIC S9(009).
              10 DRKI3224-EZ-LPVEIN-WIK          PIC S9(009).
              10 DRKI3224-AZ-LPVFEHL             PIC S9(009).
              10 DRKI3224-VZ-VER-SEL-VERTID      PIC S9(009).
              10 DRKI3224-VZ-LPV-INS             PIC S9(009).
              10 DRKI3224-VZ-LPV-UPD             PIC S9(009).
              10 DRKI3224-VZ-LPV-DEL             PIC S9(009).


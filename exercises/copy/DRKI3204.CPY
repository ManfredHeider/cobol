      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.11.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3204.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3204.CPv  $
      *
      *   Rev 2.1   Feb 19 2018 07:57:30   t13ac20
      *AKI01534: G20 - Projekt LENA: Vertrag
      *
      *   Rev 2.0   Nov 24 2017 12:31:22   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1348
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!24.11.17!2.1 ! NEUERSTELLUNG             ! SWO
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3204-RESTARTINFO.
           05 DRKI3204-KEY.
              10 DRKI3204-VNR                    PIC  X(015).
           05 DRKI3204-ZAEHLER PACKED-DECIMAL.
              10 DRKI3204-EZ-VEREIN              PIC S9(009).
              10 DRKI3204-EZ-VEREIN-INS          PIC S9(009).
              10 DRKI3204-EZ-VEREIN-UPD          PIC S9(009).
              10 DRKI3204-EZ-VEREIN-DEL          PIC S9(009).
              10 DRKI3204-EZ-VEREIN-WIK          PIC S9(009).
              10 DRKI3204-AZ-VERFEHL             PIC S9(009).
              10 DRKI3204-VZ-MOP-SEL             PIC S9(009).
              10 DRKI3204-VZ-VER-SEL-VERTID      PIC S9(009).
              10 DRKI3204-VZ-VER-SEL             PIC S9(009).
              10 DRKI3204-VZ-VER-INS             PIC S9(009).
              10 DRKI3204-VZ-VER-UPD             PIC S9(009).
              10 DRKI3204-VZ-VRB-INS             PIC S9(009).
              10 DRKI3204-VZ-KND-ALTKUNDE        PIC S9(009).
              10 DRKI3204-VZ-KND-KUNDE           PIC S9(009).


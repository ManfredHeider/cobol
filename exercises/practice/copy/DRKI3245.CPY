      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3245.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3245.CPv  $
      *
      *   Rev 2.2   Mar 06 2020 17:10:08   u130176
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung
      *
      *   Rev 2.1   Feb 16 2018 17:43:34   t13ac14
      *AKI01534: G20 - Projekt LENA: Rechenwerte
      *
      *   Rev 2.0   Dec 29 2017 13:47:22   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1357
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01534!29.12.17!2.1 ! NEUERSTELLUNG             ! von Helden
      * AKI01742!16.01.20!2.2 ! LPT-Felder hinzugefügt    ! Lorson
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3245-RESTARTINFO.
           05 DRKI3245-SA-VERTRAGSID.
              10 DRKI3245-SA                     PIC  X(003).
              10 DRKI3245-VERTRAGSID             PIC  X(013).
           05 DRKI3245-VNR                       PIC  X(015).
           05 DRKI3245-ZAEHLER PACKED-DECIMAL.
              10 DRKI3245-EZ-LPREIN              PIC S9(009).
              10 DRKI3245-AZ-LPRFEHL             PIC S9(009).
              10 DRKI3245-VZ-VER-SEL             PIC S9(009).
              10 DRKI3245-VZ-VER-UPD             PIC S9(009).
              10 DRKI3245-VZ-LPT-SEL             PIC S9(009).
              10 DRKI3245-VZ-LPT-UPD             PIC S9(009).
              10 DRKI3245-VZ-LPR-SEL             PIC S9(009).
              10 DRKI3245-VZ-LPR-UPD             PIC S9(009).
              10 DRKI3245-VZ-LPR-INS             PIC S9(009).



      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3344.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3344.CPv  $
      *
      *   Rev 2.1   May 28 2018 10:14:38   t13ac17
      *AKI01577: FW - G51/BKKLinde
      *
      *   Rev 2.0   May 15 2018 11:20:06   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1320
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01577!15.05.18!2.1 ! NEUERSTELLUNG             ! Walbert
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3344-RESTARTINFO.
           05 DRKI3344-KEY.
              10 DRKI3344-VNR                    PIC  X(015).
           05 DRKI3344-ZAEHLER PACKED-DECIMAL.
              10 DRKI3344-ANZ-EIN                PIC S9(009).
              10 DRKI3344-ANZ-EIN-UPDATE         PIC S9(009).
              10 DRKI3344-ANZ-EIN-INSERT         PIC S9(009).
              10 DRKI3344-ANZ-VER-UPDATE         PIC S9(009).
              10 DRKI3344-ANZ-VER-INSERT         PIC S9(009).
              10 DRKI3344-ANZ-VER-DELETE         PIC S9(009).
              10 DRKI3344-ANZ-VRB-INSERT         PIC S9(009).
              10 DRKI3344-ANZ-KND-UPDATE         PIC S9(009).
              10 DRKI3344-ANZ-FEHL               PIC S9(009).


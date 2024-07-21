      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  15.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3343.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3343.CPv  $
      *
      *   Rev 2.1   May 28 2018 10:14:06   t13ac17
      *AKI01577: FW - G51/BKKLinde
      *
      *   Rev 2.0   May 15 2018 11:19:20   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1318
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
       01  DRKI3343-RESTARTINFO.
           05 DRKI3343-KEY.
              10 DRKI3343-VNR                    PIC  X(015).
           05 DRKI3343-ZAEHLER PACKED-DECIMAL.
              10 DRKI3343-ANZ-EIN                PIC S9(009).
              10 DRKI3343-ANZ-KVP-FEHL           PIC S9(009).
              10 DRKI3343-ANZ-EIN-UPDATE         PIC S9(009).
              10 DRKI3343-ANZ-EIN-INSERT         PIC S9(009).
              10 DRKI3343-ANZ-KVP-DELETE         PIC S9(009).
              10 DRKI3343-ANZ-KVP-INSERT         PIC S9(009).


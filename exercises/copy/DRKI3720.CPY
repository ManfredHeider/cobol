      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3720.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3720.CPv  $
      *
      *   Rev 2.2   Sep 20 2019 10:51:22   t13ac23
      *AKI01717: Neue KD-Zuordnung 
      *
      *   Rev 2.1   Sep 19 2019 08:37:02   t13ac23
      *AKI01717: Neue KD-Zuordnung
      *
      *   Rev 2.0   Sep 02 2019 12:04:00   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    Restartinfo für PMKI1504
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01717!02.09.19!2.1 ! Neuerstellung             ! Detmar
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3720-RESTARTINFO.
           05 DRKI3720-KEY.
              10 DRKI3720-VERTRAGSNUMMER         PIC X(015).
              10 DRKI3720-RUMS-GRUPPE            PIC X(003).
              10 DRKI3720-VNR-10                 PIC X(010).
           05 DRKI3670-ZAEHLER PACKED-DECIMAL.
              10 DRKI3720-EZ-EIN                 PIC S9(009).
              10 DRKI3720-EZ-UPD-KSDS            PIC S9(009).
              10 DRKI3720-EZ-UPD-DB2             PIC S9(009).
              10 DRKI3720-EZ-EIN-KSDS            PIC S9(009).
              10 DRKI3720-EZ-FEHL                PIC S9(009).


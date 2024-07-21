      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.08.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3719.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3719.CPv  $
      *
      *   Rev 2.2   Sep 20 2019 10:50:12   t13ac23
      *AKI01717: Neue KD-Zuordnung 
      *
      *   Rev 2.1   Sep 19 2019 08:35:52   t13ac23
      *AKI01717: Neue KD-Zuordnung
      *
      *   Rev 2.0   Aug 28 2019 14:14:20   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    Änderung der RUMS-GRUPPE AMLEBEN
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01717!30.08.19!2.1 ! Neuerstellung             ! Detmar
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  AML-RUMS-AEND.
           05 DRKI3719-KB-GRUPPE-NEU.
              10 DRKI3719-VERTRAGSNUMMER         PIC X(015).
              10 DRKI3719-VNR-10 REDEFINES DRKI3719-VERTRAGSNUMMER.
                 15 FILLER                       PIC X(005).
                 15 VERTRAGSNUMMER-10            PIC X(010).
              10 DRKI3719-RUMS-GRUPPE            PIC 9(003).
              10 DRKI3719-MANDANT                PIC 9(002).


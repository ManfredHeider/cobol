      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3676.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3676.CPv  $
      *
      *   Rev 2.2   Jun 12 2019 13:21:26   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.1   Jun 11 2019 10:00:02   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.0   May 29 2019 09:39:34   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      *    RESTARTINFORMATION ZU PROGRAMM PMKI1488
      *
      *---------------------------------------------------------------*
      *    LOGBUCH
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AKI01686!29.05.19!2.1 ! NEUERSTELLUNG             ! Detmar
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3676-RESTARTINFO.
           05 DRKI3676-KEY.
              10 DRKI3676-PERSON-ID              PIC X(013).
              10 DRKI3676-GESELLSCHAFT           PIC X(002).
              10 DRKI3676-VERTRAGSID             PIC X(013).
              10 DRKI3676-SPARTE                 PIC X(010).
              10 DRKI3676-ZAHLWEISE              PIC X(001).
              10 DRKI3676-GESAMTBETRAG           PIC S9(9)V9(2)
               COMP-3.
              10 DRKI3676-VNR-URSPRUNG           PIC X(015).
              10 DRKI3676-URSPRUNGSVERTRAG-KZ    PIC X(001).
                 88 DRKI3676-IST-URSPRUNG           VALUE 'J'.
                 88 DRKI3676-IST-NICHT-URSPRUNG     VALUE 'N'.
           05 DRKI3676-ZAEHLER PACKED-DECIMAL.
              10 DRKI3676-EZ-FLBEIN              PIC S9(009).
              10 DRKI3676-EZ-FLBUPD              PIC S9(009).
              10 DRKI3676-EZ-FLB-KEIN-BOS        PIC S9(009).
              10 DRKI3676-EZ-FLB-DOPP-BOS        PIC S9(009).
              10 DRKI3676-EZ-FLB-DREI-BOS        PIC S9(009).



      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  22.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3670.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3670.CPv  $
      *
      *   Rev 2.2   Jun 12 2019 13:16:42   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.1   Jun 11 2019 09:57:04   t13ac23
      *AKI01686: AMVers - Sach: FlexN-Booster (Aktion Triple) - Sou
      *rcen
      *
      *   Rev 2.0   May 22 2019 14:38:48   t13ac23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
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
      * AKI01686!22.05.19!2.1 ! NEUERSTELLUNG             ! Detmar
      *         !        !    !                           !
      *         !        !    !                           !
      *****************************************************************
      *
       01  DRKI3670-RESTARTINFO.
           05 DRKI3670-KEY.
              07 DRKI3670-MANDANT                PIC XX.
              07 DRKI3670-PG-KZ                  PIC XX.
              07 DRKI3670-HERK-BASIS-SCHL        PIC XXX.
              07 DRKI3670-RABATTSPARTE           PIC X(10).
              07 DRKI3670-PRODUKTGRUPPE          PIC 9(4).
              07 DRKI3670-EINZELPRODUKT          PIC 9(4).
              07 DRKI3670-VSNR                   PIC X(15).
              07 DRKI3670-AKZ                    PIC X(15).
              07 DRKI3670-QNR                    PIC S9(15)
                                                         PACKED-DECIMAL.
              07 DRKI3670-WIRKSAM-AB             PIC X(10).
              07 DRKI3670-VG                     PIC XX.
              07 DRKI3670-AD-STELLE              PIC S9(7)       BINARY.
              07 DRKI3670-PIF                    PIC X(15).
              07 DRKI3670-NAME                   PIC X(40).
              07 DRKI3670-VORNAME                PIC X(40).
              07 DRKI3670-PRODUKT                PIC 9(4).
              07 DRKI3670-PRODUKTTYP             PIC 9(4).
              07 DRKI3670-VERTRAGSSPARTE         PIC X(10).
              07 DRKI3670-GEVO                   PIC S9(9)       BINARY.
              07 DRKI3670-GEVO-ID-TEIL-1         PIC S9(5)       BINARY.
              07 DRKI3670-GEVO-ID-TEIL-2         PIC S9(5)       BINARY.
              07 DRKI3670-TIMESTAMP-IN-K         PIC X(26).
              07 DRKI3670-AKTIONS-KZN            PIC X(5).
              07 DRKI3670-BETRAG-1               PIC S9(11)V99
                                                         PACKED-DECIMAL.
              07 DRKI3670-ANZ-EINHEITEN          PIC S9(11)V9
                                                         PACKED-DECIMAL.
              07 DRKI3670-BETRAG-2               PIC S9(11)V99
                                                         PACKED-DECIMAL.
              07 DRKI3670-BEWSCHL                PIC X(6).
              07 DRKI3670-PDPT-ID                PIC X(15).
              07 DRKI3670-KONTO-GES-KZ           PIC X.
              07 DRKI3670-INFO-KZ                PIC X(5).
              07 FILLER                          PIC X(45).
           05 DRKI3670-ZAEHLER PACKED-DECIMAL.
              10 DRKI3670-EZ-FLBEIN              PIC S9(009).
              10 DRKI3670-EZ-FLBEIN-INS          PIC S9(009).
              10 DRKI3670-EZ-FLBEIN-UPD          PIC S9(009).
              10 DRKI3670-EZ-FLBEIN-SEL          PIC S9(009).




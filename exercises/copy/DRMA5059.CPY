      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5059.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5059.CPv  $
      *
      *   Rev 2.3   Nov 15 2018 13:18:20   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Aus
      *bau Besonderheiten 08 und 09
      *
      *   Rev 2.2   Aug 15 2018 10:01:02   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Pro
      *gramme
      *
      *   Rev 2.1   Feb 13 2015 15:26:24   t13ac19
      *AMA00147: Änderung Sonderleistungen 2015 - Copies
      *
      *   Rev 2.0   Jan 26 2015 20:39:02   t13ac19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: REF.BESTAND SONDERLEISTUNGEN VON DVAG
      *
      *     SOURCE-NAME      :  DRMA5059
      *
      *     GENERIERUNG      :  02.11.2018/10:44:40
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00147!26.01.15!2.1  ! NEUERSTELLUNG             ! O.WILHELM
      * AMA00410!01.08.18!2.2  ! FELD AUSNAHME-ALAG        ! D.WILHELM
      *         !        !     ! AUFGENOMMEN               !
      * AMA00410!02.11.18!2.3  ! FELD AUSNAHME-ALAG        ! D.WILHELM
      *         !        !     ! ENTFERNT                  !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5059.
              07 FUNKTION               pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 BUCHDAT                pic X(10).
              07 DGMA0001.
                 09 VBNR                pic 9(7).
                 09 KTY-EP-TYP          pic 9(4).
                 09 KNR-EP-TYP          pic 9(4).
                 09 WERTUNGSZEITRAUM    pic 99.
      *
      **************************************************************


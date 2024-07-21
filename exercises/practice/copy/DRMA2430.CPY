      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2430.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2430.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:14:28   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 26 2015 07:47:58   t13ac13
      *AMA00130: USt: KOMA0500-Buchauszug - Copies
      *
      *   Rev 2.1   Jan 15 2014 18:59:50   t13ac18
      *AMA00011: Kalypso: Teilprojekt 4 NEUANLAGE PMMA* für Buchaus
      *zug
      *
      *   Rev 2.0   Jan 07 2014 14:35:34   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     normierter Satz für Buchauszug
      *
      *     SOURCE-NAME      :  DRMA2430
      *
      *     GENERIERUNG      :  02.10.2017/12:52:51
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00130!07.10.15!2.1  ! ÜBERNAHME AUS PRODUKTION  ! D.WILHELM
      * AMA00130!13.10.15!2.2  ! USt-Felder ergänzt        ! M.Boost
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0128         ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0448 EINBEH. !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2430.
              07 PG                     pic 99.
              07 PRODART                pic 99.
              07 VNR                    pic X(15).
              07 TARIFKZ                pic 99.
              07 VBNR                   pic 9(9).
              07 BUCHDAT                pic X(10).
              07 VN                     pic X(20).
              07 PRODSCHL               pic 999.
              07 PROV-PFL-SU            pic S9(9)V99   packed-decimal.
              07 BEWDAT                 pic X(10).
              07 BNGBETR-1              pic S9(9)V99   packed-decimal.
              07 PROV-AUS               pic S9(9)V99   packed-decimal.
              07 PROV-EINB              pic S9(9)V99   packed-decimal.
              07 PROV-UST               pic S9(9)V99   packed-decimal.
              07 UST-SATZ               pic S999V99    packed-decimal.
              07 UST-BERECH-KZN         pic X.
              07 BEARBTERM              pic S9(15)     packed-decimal.
      *
      **************************************************************


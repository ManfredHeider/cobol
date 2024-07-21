      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5082.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5082.CPv  $
      *
      *   Rev 2.3   Jul 07 2016 16:03:36   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.2   Oct 16 2015 11:46:02   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.1   Mar 20 2015 12:14:22   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.0   Mar 20 2015 08:41:30   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: REFERENZTABELLE DER GESELLSCHAFTEN
      *
      *     SOURCE-NAME      :  DRMA5082
      *
      *     GENERIERUNG      :  23.05.2016/14:58:52
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!20.03.15!   1 ! NEUERSTELLUNG             ! WOLFF
      * AMA00128!30.09.15!2.2  ! PG-UST-RELEVANT EINGEFÜGT ! D.WILHELM
      *         !        !     ! DVAG-GKTO-EIN             !
      *         !        !     ! DVAG-GKTO-AUS             !
      *         !        !     ! AVBG-GKTO-EIN             !
      *         !        !     ! AVBG-GKTO-AUS             !
      *         !        !     ! gelöscht                  !
      * AMA00292!23.05.16!2.3  ! FOLGENDE FELDER EINGEFÜGT:! D.WILHELM
      *         !        !     ! VERSION, VG, GUELTIG-AB,  !
      *         !        !     ! GUELTIG-BIS,              !
      *         !        !     ! AENDERUNGSSYSZEIT,        !
      *         !        !     ! AENDERUNGSKENNUNG.        !
      *         !        !     ! KTO statt DVAG-KONTO      !
      *         !        !     ! und AVBG-KONTO.           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5082.
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 PG-BEZEICH-KURZ        pic X(10).
              07 PG-BEZEICH-LANG        pic X(40).
              07 LKZ                    pic X(4).
              07 BEW-ART-KZN            pic X(10).
              07 KTO                    pic X(10).
              07 PG-UST-RELEVANT        pic X.
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************


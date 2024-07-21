      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5142.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5142.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:30:54   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   May 18 2016 11:25:26   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   Apr 28 2016 13:59:52   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5044   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5142
      *
      *     GENERIERUNG      :  02.10.2017/14:05:47
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00270!29.04.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DEMA0446 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5142.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 VBNR                   pic 9(9).
              07 BUCHDAT                pic X(10).
              07 PROV-AUS-EINB          pic S9(9)V99   packed-decimal.
      *
      **************************************************************


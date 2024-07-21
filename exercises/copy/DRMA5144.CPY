      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5144.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5144.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:31:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   May 18 2016 11:25:48   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   May 02 2016 13:56:36   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Einzelnachweis - var. Teil (Ex.gr.zus.)
      *
      *     SOURCE-NAME      :  DRMA5144
      *
      *     GENERIERUNG      :  02.10.2017/14:07:04
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00270!03.05.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DGMA5007 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 EXGRZUS-DATEN.
              07 UST-PFLICHTIG-KZN      pic X.
              07 DGMA5007.
                 09 UST-BERECH-KZN      pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PROV-AUS-EINB-UST   pic S9(9)V99   packed-decimal.
                 09 PROV-AUS-EINB       pic S9(9)V99   packed-decimal.
                 09 PROV-AUS            pic S9(9)V99   packed-decimal.
                 09 PROV-EINB           pic S9(9)V99   packed-decimal.
                 09 PROV-UST            pic S9(9)V99   packed-decimal.
      *
      **************************************************************


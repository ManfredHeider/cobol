      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2810.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2810.CPv  $
      *
      *   Rev 2.3   Nov 23 2017 10:45:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu SWZ Copy PMM2771
      *
      *   Rev 2.2   Aug 27 2015 19:12:26   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:38:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Provisionseinheiten und -verdienste VBI (DVAG)
      *
      *     SOURCE-NAME      :  DRMA2810
      *
      *     GENERIERUNG      :  27.10.2017/08:19:36
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!25.08.15!2.2  ! ANPASSUNGEN WG. LUCY      ! D.WILHELM
      *         !        !     ! 4 neue Felder PV...UST    !
      * AMA00360!20.09.17!2.3  ! FOLGE/NEU und EINBEHALT   ! WARZ
      *         !        !     ! 8 Felder raus BW-.......  !
      *         !        !     !12 neue Felder ..FG-NEU..  !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2810.
              07 EH-GS-ZU-EG            pic S9(10)V9   packed-decimal.
              07 EH-GS-ZU-GR            pic S9(10)V9   packed-decimal.
              07 EH-GS-AB-EG            pic S9(10)V9   packed-decimal.
              07 EH-GS-AB-GR            pic S9(10)V9   packed-decimal.
              07 PV-GS-NT-EG            pic S9(9)V99   packed-decimal.
              07 PV-GS-NT-GR            pic S9(9)V99   packed-decimal.
              07 PV-FG-NT-EG            pic S9(9)V99   packed-decimal.
              07 PV-FG-NT-GR            pic S9(9)V99   packed-decimal.
              07 EH-FG-ZU-EG            pic S9(10)V9   packed-decimal.
              07 EH-FG-ZU-GR            pic S9(10)V9   packed-decimal.
              07 EH-FG-AB-EG            pic S9(10)V9   packed-decimal.
              07 EH-FG-AB-GR            pic S9(10)V9   packed-decimal.
              07 DRMA2811.
                 09 EH-GS-ZU-EG-VM      pic S9(10)V9   packed-decimal.
                 09 EH-GS-ZU-GR-VM      pic S9(10)V9   packed-decimal.
                 09 EH-GS-AB-EG-VM      pic S9(10)V9   packed-decimal.
                 09 EH-GS-AB-GR-VM      pic S9(10)V9   packed-decimal.
                 09 EH-FG-ZU-EG-VM      pic S9(10)V9   packed-decimal.
                 09 EH-FG-ZU-GR-VM      pic S9(10)V9   packed-decimal.
                 09 EH-FG-AB-EG-VM      pic S9(10)V9   packed-decimal.
                 09 EH-FG-AB-GR-VM      pic S9(10)V9   packed-decimal.
              07 PV-GS-NT-EG-UST        pic S9(9)V99   packed-decimal.
              07 PV-GS-NT-GR-UST        pic S9(9)V99   packed-decimal.
              07 PV-FG-NT-EG-UST        pic S9(9)V99   packed-decimal.
              07 PV-FG-NT-GR-UST        pic S9(9)V99   packed-decimal.
              07 EH-FG-NEU-ZU-EG        pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-ZU-GR        pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-AB-EG        pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-AB-GR        pic S9(10)V9   packed-decimal.
              07 PV-FG-NEU-NT-EG        pic S9(9)V99   packed-decimal.
              07 PV-FG-NEU-NT-GR        pic S9(9)V99   packed-decimal.
              07 EH-FG-NEU-ZU-EG-VM     pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-ZU-GR-VM     pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-AB-EG-VM     pic S9(10)V9   packed-decimal.
              07 EH-FG-NEU-AB-GR-VM     pic S9(10)V9   packed-decimal.
              07 PV-FG-NEU-NT-EG-UST    pic S9(9)V99   packed-decimal.
              07 PV-FG-NEU-NT-GR-UST    pic S9(9)V99   packed-decimal.
      *
      **************************************************************


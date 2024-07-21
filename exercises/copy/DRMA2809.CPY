      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2809.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2809.CPv  $
      *
      *   Rev 2.3   Nov 23 2017 10:45:06   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu SWZ Copy PMM2771
      *
      *   Rev 2.2   Aug 27 2015 19:12:12   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:38:12   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für Erstellung VBI-DATEN (DVAG)
      *
      *     SOURCE-NAME      :  DRMA2809
      *
      *     GENERIERUNG      :  27.10.2017/08:52:10
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!25.08.15!2.2  ! ANPASSUNGEN WG. LUCY      ! D.WILHELM
      *         !        !     ! 4 neue Felder in DRMA2810 !
      * AMA00360!20.09.17!2.3  ! FOLGE/NEU und EINBEHALT   ! WARZ
      *         !        !     ! Anpassungen in DRMA2810   !
      *         !        !     ! 8 Felder raus BW-.......  !
      *         !        !     !12 neue Felder ..FG-NEU..  !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 VBI-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 GESKZ                  pic S9(4)      binary.
              07 PRODSCHL               pic S9(4)      binary.
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 VM-KZ                  pic X.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 DRMA2810.
                 09 EH-GS-ZU-EG         pic S9(10)V9   packed-decimal.
                 09 EH-GS-ZU-GR         pic S9(10)V9   packed-decimal.
                 09 EH-GS-AB-EG         pic S9(10)V9   packed-decimal.
                 09 EH-GS-AB-GR         pic S9(10)V9   packed-decimal.
                 09 PV-GS-NT-EG         pic S9(9)V99   packed-decimal.
                 09 PV-GS-NT-GR         pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-EG         pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-GR         pic S9(9)V99   packed-decimal.
                 09 EH-FG-ZU-EG         pic S9(10)V9   packed-decimal.
                 09 EH-FG-ZU-GR         pic S9(10)V9   packed-decimal.
                 09 EH-FG-AB-EG         pic S9(10)V9   packed-decimal.
                 09 EH-FG-AB-GR         pic S9(10)V9   packed-decimal.
                 09 DRMA2811.
                    11 EH-GS-ZU-EG-VM   pic S9(10)V9   packed-decimal.
                    11 EH-GS-ZU-GR-VM   pic S9(10)V9   packed-decimal.
                    11 EH-GS-AB-EG-VM   pic S9(10)V9   packed-decimal.
                    11 EH-GS-AB-GR-VM   pic S9(10)V9   packed-decimal.
                    11 EH-FG-ZU-EG-VM   pic S9(10)V9   packed-decimal.
                    11 EH-FG-ZU-GR-VM   pic S9(10)V9   packed-decimal.
                    11 EH-FG-AB-EG-VM   pic S9(10)V9   packed-decimal.
                    11 EH-FG-AB-GR-VM   pic S9(10)V9   packed-decimal.
                 09 PV-GS-NT-EG-UST     pic S9(9)V99   packed-decimal.
                 09 PV-GS-NT-GR-UST     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-EG-UST     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-GR-UST     pic S9(9)V99   packed-decimal.
                 09 EH-FG-NEU-ZU-EG     pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-ZU-GR     pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-AB-EG     pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-AB-GR     pic S9(10)V9   packed-decimal.
                 09 PV-FG-NEU-NT-EG     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NEU-NT-GR     pic S9(9)V99   packed-decimal.
                 09 EH-FG-NEU-ZU-EG-VM  pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-ZU-GR-VM  pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-AB-EG-VM  pic S9(10)V9   packed-decimal.
                 09 EH-FG-NEU-AB-GR-VM  pic S9(10)V9   packed-decimal.
                 09 PV-FG-NEU-NT-EG-UST pic S9(9)V99   packed-decimal.
                 09 PV-FG-NEU-NT-GR-UST pic S9(9)V99   packed-decimal.
      *
      **************************************************************


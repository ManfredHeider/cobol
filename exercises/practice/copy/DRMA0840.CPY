      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0840.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0840.CPv  $
      *
      *   Rev 2.2   Aug 27 2015 19:11:42   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:08:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Provisionseinheiten und -verdienste (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0840
      *
      *     GENERIERUNG      :  17.08.2015/18:57:13
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!17.08.15!2.2  ! Ersetzen PV-GS-NT-EG      ! D.WILHELM
      *         !        !     !          PV-GS-NT-GR      !
      *         !        !     !          PV-FG-NT-EG      !
      *         !        !     !          PV-FG-NT-GR      !
      *         !        !     ! durch DGMA5000            !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0840.
              07 DGMA0826.
                 09 EH-GS-ZU-EG         pic S9(9)V9    packed-decimal.
                 09 EH-GS-ZU-GR         pic S9(9)V9    packed-decimal.
                 09 EH-GS-AB-EG         pic S9(9)V9    packed-decimal.
                 09 EH-GS-AB-GR         pic S9(9)V9    packed-decimal.
              07 DGMA5000.
                 09 PV-GS-NT-EG         pic S9(9)V99   packed-decimal.
                 09 PV-GS-NT-GR         pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-EG         pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-GR         pic S9(9)V99   packed-decimal.
              07 DGMA0827.
                 09 EH-FG-ZU-EG         pic S9(9)V9    packed-decimal.
                 09 EH-FG-ZU-GR         pic S9(9)V9    packed-decimal.
                 09 EH-FG-AB-EG         pic S9(9)V9    packed-decimal.
                 09 EH-FG-AB-GR         pic S9(9)V9    packed-decimal.
      *
      **************************************************************


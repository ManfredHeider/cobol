      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5119.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5119.CPv  $
      *
      *   Rev 2.2   Nov 24 2017 09:54:32   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copies 2 DWH
      *
      *   Rev 2.1   Aug 27 2015 19:12:56   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   Aug 12 2015 09:44:46   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     PROV.-EINH. AUS UST-PFLICHT. PROD.
      *
      *     SOURCE-NAME      :  DRMA5119
      *
      *     GENERIERUNG      :  06.11.2017/16:01:14
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00150!21.08.15!2.1  ! NEUERSTELLUNG             ! D.WILHELM
      * AMA00360!06.11.17!2.2  ! ERWEITERUNG FOLGE-NEU     ! D.WILHELM
      *         !        !     ! EH-FELDER UST             !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5119.
              07 DGMA5001.
                 09 EH-GS-ZU-EG-UST     pic S9(9)V9    packed-decimal.
                 09 EH-GS-ZU-GR-UST     pic S9(9)V9    packed-decimal.
                 09 EH-GS-AB-EG-UST     pic S9(9)V9    packed-decimal.
                 09 EH-GS-AB-GR-UST     pic S9(9)V9    packed-decimal.
              07 DGMA5002.
                 09 EH-FG-ZU-EG-UST     pic S9(9)V9    packed-decimal.
                 09 EH-FG-ZU-GR-UST     pic S9(9)V9    packed-decimal.
                 09 EH-FG-AB-EG-UST     pic S9(9)V9    packed-decimal.
                 09 EH-FG-AB-GR-UST     pic S9(9)V9    packed-decimal.
              07 DGMA5015.
                 09 EH-FG-NEU-ZU-EG-UST pic S9(9)V9    packed-decimal.
                 09 EH-FG-NEU-ZU-GR-UST pic S9(9)V9    packed-decimal.
                 09 EH-FG-NEU-AB-EG-UST pic S9(9)V9    packed-decimal.
                 09 EH-FG-NEU-AB-GR-UST pic S9(9)V9    packed-decimal.
      *
      **************************************************************


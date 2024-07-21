      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2522.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2522.CPv  $
      *
      *   Rev 2.3   Nov 24 2017 09:54:14   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copies 2 DWH
      *
      *   Rev 2.2   Aug 27 2015 19:11:56   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:33:42   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Fortschreibung VB-Statistik-Werte
      *
      *     SOURCE-NAME      :  DRMA2522
      *
      *     GENERIERUNG      :  06.11.2017/15:36:44
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!14.08.15!2.2  ! DIV.ANPASSUNGEN WG. LUCY  ! D.WILHELM
      * AMA00360!06.11.17!2.3  ! Anpassungen DGMA2523      ! D.WILHELM
      *         !        !     ! WG. FOLGE-NEU             !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2522.
              07 VBNR                   pic X(7).
              07 EKRIT-DATUM            pic 9(6).
              07 EH-AB-78-EG            pic S9(9)V9    packed-decimal.
              07 EH-AB-78-GR            pic S9(9)V9    packed-decimal.
              07 EH-BIS-78-GS           pic S9(9)V9    packed-decimal.
              07 DGMA2523               occurs 12
                                        indexed by IND.
                 09 VBKZ                pic X.
                 09 DGMA0826.
                    11 EH-GS-ZU-EG      pic S9(9)V9    packed-decimal.
                    11 EH-GS-ZU-GR      pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-EG      pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-GR      pic S9(9)V9    packed-decimal.
                 09 DGMA0827.
                    11 EH-FG-ZU-EG      pic S9(9)V9    packed-decimal.
                    11 EH-FG-ZU-GR      pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-EG      pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-GR      pic S9(9)V9    packed-decimal.
                 09 DGMA5001.
                    11 EH-GS-ZU-EG-UST  pic S9(9)V9    packed-decimal.
                    11 EH-GS-ZU-GR-UST  pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-EG-UST  pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-GR-UST  pic S9(9)V9    packed-decimal.
                 09 DGMA5002.
                    11 EH-FG-ZU-EG-UST  pic S9(9)V9    packed-decimal.
                    11 EH-FG-ZU-GR-UST  pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-EG-UST  pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-GR-UST  pic S9(9)V9    packed-decimal.
                 09 DGMA5014.
                    11 EH-FG-NEU-ZU-EG  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-ZU-GR  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-EG  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-GR  pic S9(9)V9    packed-decimal.
                 09 DGMA5015.
                    11 EH-FG-NEU-ZU-EG-UST
                                        pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-ZU-GR-UST
                                        pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-EG-UST
                                        pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-GR-UST
                                        pic S9(9)V9    packed-decimal.
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0834.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA0834.CPv  $
      *
      *   Rev 2.5   Nov 24 2017 09:54:04   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copies 2 DWH
      *
      *   Rev 2.4   Oct 19 2017 14:11:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Aug 27 2015 19:11:12   t13ac18
      *AMA00150: USt: KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Jul 16 2013 10:34:20   t13ac18
      *AMA00001: Testauftrag
      *
      *   Rev 2.1   Jun 28 2013 16:06:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für DB2-Table GEEV (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0834
      *
      *     GENERIERUNG      :  06.11.2017/15:34:03
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.2  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00150!17.08.15!2.3  ! ANPASSUNGEN AN GEÄNDERTE  ! D.WILHELM
      *         !        !     ! GEEV WG. LUCY             !
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DEMA0160 EINBEH. ! WARZ
      * AMA00360!06.11.17!2.5  ! Einfügen DRMA5163 5164    ! D.WILHELM
      *         !        !     ! 5165 für Folge-Neu        !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 GEEV-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 DRMA0840.
                 09 DGMA0826.
                    11 EH-GS-ZU-EG      pic S9(9)V9    packed-decimal.
                    11 EH-GS-ZU-GR      pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-EG      pic S9(9)V9    packed-decimal.
                    11 EH-GS-AB-GR      pic S9(9)V9    packed-decimal.
                 09 DGMA5000.
                    11 PV-GS-NT-EG      pic S9(9)V99   packed-decimal.
                    11 PV-GS-NT-GR      pic S9(9)V99   packed-decimal.
                    11 PV-FG-NT-EG      pic S9(9)V99   packed-decimal.
                    11 PV-FG-NT-GR      pic S9(9)V99   packed-decimal.
                 09 DGMA0827.
                    11 EH-FG-ZU-EG      pic S9(9)V9    packed-decimal.
                    11 EH-FG-ZU-GR      pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-EG      pic S9(9)V9    packed-decimal.
                    11 EH-FG-AB-GR      pic S9(9)V9    packed-decimal.
              07 PROV-EB-ZFL            pic S9(9)V99   packed-decimal.
              07 EINB-AUSZ-Q            pic S9(9)V99   packed-decimal.
              07 UEBWEIS                pic S9(9)V99   packed-decimal.
              07 KTY-UST-KZ             pic S9(4)      binary.
              07 KNR-UST-KZ             pic S9(4)      binary.
              07 DRMA5118.
                 09 PV-GS-NT-EG-UST     pic S9(9)V99   packed-decimal.
                 09 PV-GS-NT-GR-UST     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-EG-UST     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NT-GR-UST     pic S9(9)V99   packed-decimal.
              07 DRMA5163.
                 09 PV-FG-NEU-NT-EG     pic S9(9)V99   packed-decimal.
                 09 PV-FG-NEU-NT-GR     pic S9(9)V99   packed-decimal.
              07 DRMA5164.
                 09 PV-FG-NEU-NT-EG-UST pic S9(9)V99   packed-decimal.
                 09 PV-FG-NEU-NT-GR-UST pic S9(9)V99   packed-decimal.
              07 DRMA5165.
                 09 DGMA5014.
                    11 EH-FG-NEU-ZU-EG  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-ZU-GR  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-EG  pic S9(9)V9    packed-decimal.
                    11 EH-FG-NEU-AB-GR  pic S9(9)V9    packed-decimal.
      *
      **************************************************************


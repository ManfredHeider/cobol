      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5169.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5169.CPv  $
      *
      *   Rev 2.1   Jun 12 2018 09:44:14   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.0   May 03 2018 13:03:58   T13AC18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz (Neu) für DB2-Table EV
      *
      *     SOURCE-NAME      :  DRMA5169
      *
      *     GENERIERUNG      :  11.06.2018/14:36:08
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00400!26.02.18!2.1  ! Neuerstellung             ! D.WILHELM
      *         !08.06.18!     ! ERWEITERUNG UM EQF-MONAT  ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 EV-SATZ-NEU.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 KNR-VERGUET-TYP        pic S9(4)      binary.
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 KTY-VERGUET-TYP        pic S9(4)      binary.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 EQF                    pic S999V99    packed-decimal.
              07 STORMIND-PROZ          pic S999V99    packed-decimal.
              07 AE-MONATS-DS           pic S9(9)V9    packed-decimal.
              07 AUSZAHLUNG             pic S9(9)V99   packed-decimal.
              07 RUECKBELASTG           pic S9(9)V99   packed-decimal.
              07 AE-MONATS-DS-UST-FREI  pic S9(10)V9   packed-decimal.
              07 AE-MONATS-DS-UST-PFL   pic S9(10)V9   packed-decimal.
              07 AUSZAHLUNG-UST-FREI    pic S9(9)V99   packed-decimal.
              07 AUSZAHLUNG-UST-PFL     pic S9(9)V99   packed-decimal.
              07 AE-MONATS-DS-GRENZE    pic S9(4)V9    packed-decimal.
              07 WERTUNGSZEITRAUM       pic 9.
              07 AE-SUMME               pic S9(9)V9    packed-decimal.
              07 WERTUNGSMONAT          pic 99.
              07 EQF-GRENZE-VON         pic S999V99    packed-decimal.
              07 EQF-GRENZE-BIS         pic S999V99    packed-decimal.
              07 EH-AP-NT-EG-GESAMT     pic S9(10)V9   packed-decimal.
              07 EH-AP-NT-EG-UST-FREI   pic S9(10)V9   packed-decimal.
              07 EH-AP-NT-EG-UST-PFL    pic S9(10)V9   packed-decimal.
              07 EQF-MONAT              pic S999V99    packed-decimal.
      *
      **************************************************************


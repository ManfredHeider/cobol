      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.05.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5168.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5168.CPv  $
      *
      *   Rev 2.1   Jun 12 2018 09:44:04   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.0   May 02 2018 17:08:16   T13AC18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz(Neu) Buchungssätze aus VB-Statistik
      *
      *     SOURCE-NAME      :  DRMA5168
      *
      *     GENERIERUNG      :  11.06.2018/14:30:47
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
           05 DRMA5168.
              07 DGMA4345.
                 09 VBNR                pic 9(9).
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 DIR-NR              pic S9(4)      binary.
                 09 KNR-EP-TYP          pic S9(4)      binary.
                 09 EQF                 pic S999V99    packed-decimal.
                 09 WERTUNGSZEITRAUM    pic 9.
                 09 AE-SUMME-UST-FREI   pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-FREI
                                        pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-FREI-EFF
                                        pic S9(10)V9   packed-decimal.
                 09 BRUTTO-BETRAG-UST-FREI
                                        pic S9(9)V99   packed-decimal.
                 09 AUSZAHLUNG-UST-FREI pic S9(9)V99   packed-decimal.
                 09 STORMIND-PROZ       pic S999V99    packed-decimal.
                 09 AE-MONATS-DS-GRENZE pic S9(4)V9    packed-decimal.
                 09 FAKTOR              pic S9V99      packed-decimal.
                 09 BUCHDAT             pic 9(8).
                 09 AE-SUMME-UST-PFL    pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-PFL
                                        pic S9(10)V9   packed-decimal.
                 09 AE-MONATS-DS-UST-PFL-EFF
                                        pic S9(10)V9   packed-decimal.
                 09 BRUTTO-BETRAG-UST-PFL
                                        pic S9(9)V99   packed-decimal.
                 09 AUSZAHLUNG-UST-PFL  pic S9(9)V99   packed-decimal.
              07 WERTUNGSMONAT          pic 99.
              07 EQF-GRENZE-VON         pic S999V99    packed-decimal.
              07 EQF-GRENZE-BIS         pic S999V99    packed-decimal.
              07 EH-AP-NT-EG-GESAMT     pic S9(10)V9   packed-decimal.
              07 EH-AP-NT-EG-UST-FREI   pic S9(10)V9   packed-decimal.
              07 EH-AP-NT-EG-UST-PFL    pic S9(10)V9   packed-decimal.
              07 EQF-MONAT              pic S999V99    packed-decimal.
      *
      **************************************************************


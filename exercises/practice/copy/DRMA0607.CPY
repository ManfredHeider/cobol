      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0607.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA0607.CPv  $
      *
      *   Rev 2.4   Jul 05 2016 07:55:58   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.3   May 18 2016 11:46:18   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Jul 16 2013 10:34:36   t13ac18
      *AMA00001: Testauftrag
      *
      *   Rev 2.1   Jun 28 2013 16:01:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Buchungssätze aus VB-Statistik
      *
      *     SOURCE-NAME      :  DRMA0607
      *
      *     GENERIERUNG      :  20.06.2016/11:02:33
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!23.03.16!2.3  ! Änderung DGMA4345         !   WALBERT
      *         !        !     ! (war nicht produktiv)     !
      * AMA00270!20.06.16!2.4  ! Änderung in DGMA4345      !   Walbert
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0607.
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
      *
      **************************************************************


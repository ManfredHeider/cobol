      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0837.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0837.CPv  $
      *
      *   Rev 2.2   Jul 05 2016 07:56:12   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:07:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für DB2-Table EV (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0837
      *
      *     GENERIERUNG      :  20.06.2016/16:34:57
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!20.06.16!2.2  ! Lucy2 Integrat. UST-Felder!   Walbert
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 EV-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 KNR-VERGUET-TYP        pic S9(4)      binary.
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 KTY-VERGUET-TYP        pic S9(4)      binary.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 EQF                    pic S999V99    packed-decimal.
              07 STORMIND-PROZ          pic S999V99    packed-decimal.
              07 AE-MONATS-DS           pic S9(9)V9    packed-decimal.
              07 BRUTTO-BETRAG          pic S9(9)V99   packed-decimal.
              07 RUECKBELASTG           pic S9(9)V99   packed-decimal.
              07 AE-MONATS-DS-UST-FREI  pic S9(10)V9   packed-decimal.
              07 AE-MONATS-DS-UST-PFL   pic S9(10)V9   packed-decimal.
              07 BRUTTO-BETRAG-UST-FREI pic S9(9)V99   packed-decimal.
              07 BRUTTO-BETRAG-UST-PFL  pic S9(9)V99   packed-decimal.
              07 AE-MONATS-DS-GRENZE    pic S9(4)V9    packed-decimal.
              07 WERTUNGSZEITRAUM       pic 9.
              07 AE-SUMME               pic S9(9)V9    packed-decimal.
      *
      **************************************************************


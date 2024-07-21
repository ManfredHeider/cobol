      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4301.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA4301.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:21:52   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   May 18 2016 11:25:06   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Oct 15 2013 09:46:20   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 17:27:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Einzelnachweis - var. Teil (Ex.gr.bon.)
      *
      *     SOURCE-NAME      :  DRMA4301
      *
      *     GENERIERUNG      :  02.10.2017/13:29:57
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!24.09.13!2.1  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!01.10.13!2.2  ! DAISY ANPASSUNG           !WALBERT
      * AMA00270!04.05.16!2.3  ! UST-PFLICHTIG-KZN UND     ! WOLFF
      *         !        !     ! DGMA5007 AUFGENOMMEN      !
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DGMA5007 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 EXGRBON-DATEN.
              07 AE-MONATS-DS           pic S9(10)V9   packed-decimal.
              07 BRUTTO-BETRAG-EXB      pic S9(9)V99   packed-decimal.
              07 EQF                    pic S999V99    packed-decimal.
              07 FAKTOR                 pic S9V99      packed-decimal.
              07 UST-PFLICHTIG-KZN      pic X.
              07 DGMA5007.
                 09 UST-BERECH-KZN      pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PROV-AUS-EINB-UST   pic S9(9)V99   packed-decimal.
                 09 PROV-AUS-EINB       pic S9(9)V99   packed-decimal.
                 09 PROV-AUS            pic S9(9)V99   packed-decimal.
                 09 PROV-EINB           pic S9(9)V99   packed-decimal.
                 09 PROV-UST            pic S9(9)V99   packed-decimal.
      *
      **************************************************************


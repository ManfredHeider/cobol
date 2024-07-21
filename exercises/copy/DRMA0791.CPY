      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0791.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA0791.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:10:56   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   May 18 2016 11:24:44   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Oct 15 2013 09:45:46   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jun 28 2013 16:03:38   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Einzelnachweis - var. Teil (Erf.-Prov.)
      *
      *     SOURCE-NAME      :  DRMA0791
      *
      *     GENERIERUNG      :  02.10.2017/12:36:42
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!24.09.13!2.1  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!01.10.13!2.2  ! DAISY ANPASSUNG           !WALBERT
      * AMA00270!02.05.16!2.3  ! UST-PFLICHTIG-KZN UND     ! WOLFF
      *         !        !     ! DGMA5007 AUFGENOMMEN      !
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DGMA5007 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 ERFPROV-DATEN.
              07 AE-MONATS-DS           pic S9(7)V99   packed-decimal.
              07 BRUTTO-BETRAG-EP       pic S9(7)V99   packed-decimal.
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


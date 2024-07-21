      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3160.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA3160.CPv  $
      *
      *   Rev 2.4   Nov 24 2017 09:54:22   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copies 2 DWH
      *
      *   Rev 2.3   Oct 19 2017 14:16:52   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Dec 16 2013 14:05:14   t13ac17
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.1   Jul 01 2013 11:41:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     SST VB-Statistik
      *
      *     SOURCE-NAME      :  DRMA3160
      *
      *     GENERIERUNG      :  14.11.2017/16:28:16
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!16.12.13!2.1  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00027!16.12.13!2.2  ! DAISY ANPASSUNG           !WALBERT
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0128         !WARZ
      *         !        !     ! ÄNDERUNG DEMA0160 EINBEH. !
      * AMA00360!06.11.17!2.4  ! AUSTAUSCH DYN-KZN2 DURCH  !D.WILHELM
      *         !        !     ! ERWERB-FOLGE-KZN          !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3160.
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 PRODART                pic 99.
              07 PROVAUS                pic S9(9)V99   packed-decimal.
              07 PROVEINB               pic S9(9)V99   packed-decimal.
              07 PROVEINH               pic S9(10)V9   packed-decimal.
              07 PE-MANR                pic 9(9).
              07 ERWERB-FOLGE-KZN       pic X.
              07 BUCHDAT                pic X(10).
              07 HZRBEG-JAHR            pic 9(4).
              07 HZRBEG-MONAT           pic 99.
              07 BEWART                 pic 9(4).
      *
      **************************************************************


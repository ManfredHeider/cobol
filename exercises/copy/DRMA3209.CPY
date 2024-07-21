      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3209.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA3209.CPv  $
      *
      *   Rev 2.5   Oct 19 2017 14:17:44   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   Dec 09 2015 12:03:00   t13ac13
      *AMA00225:  USt: CR Anpassung der Managementkennzahlen - Copy
      * ÜaP
      *
      *   Rev 2.3   Jul 28 2015 14:34:52   t13ac13
      *AMA00149: USt: KOMA0507-Statistiken - Teil 1 - Copies
      *
      *   Rev 2.2   Oct 14 2013 14:14:10   t13ac17
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.1   Jul 01 2013 17:13:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Provisionsdaten ÜaP
      *
      *     SOURCE-NAME      :  DRMA3209
      *
      *     GENERIERUNG      :  02.10.2017/13:12:52
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.1  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!01.10.13!2.2  ! DAISY ANPASSUNG           !WALBERT
      * AMA00149!25.06.15!2.3  ! Änderungen wegen LUCY     !BOOST
      * AMA00225!26.11.15!2.4  ! Umbenennen PROVAUS-BRUTTO !BOOST
      *         !        !     ! in PROVAUS                !
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG DEMA0128         ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0160 EINBEH. !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3209.
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 PRODART                pic 99.
              07 PROVAUS                pic S9(9)V99   packed-decimal.
              07 PROVEINB               pic S9(9)V99   packed-decimal.
              07 PE-AE                  pic S9(10)V9   packed-decimal.
              07 PE-MANR                pic 9(9).
              07 DYN-KZN2               pic 9.
              07 BUCHDAT                pic X(10).
              07 ERWERB-FOLGE-KZN       pic X(8).
              07 PROVAUS-UST            pic S9(9)V99   packed-decimal.
      *
      **************************************************************


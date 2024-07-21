      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3241.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA3241.CPv  $
      *
      *   Rev 2.5   Oct 19 2017 14:18:38   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   Aug 31 2015 10:08:34   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:40:24   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Oct 14 2013 14:17:06   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 11:44:42   p13sp20
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
      *     SOURCE-NAME      :  DRMA3241
      *
      *     GENERIERUNG      :  05.10.2017/12:44:50
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.1  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!01.10.13!2.2  ! DAISY ANPASSUNG           !WALBERT
      * AMA00128!29.04.15!2.3  ! neues Umsatzsteuer-Feld   !Walbert
      *         !01.07.15!     ! Provaus --> Provaus-netto !
      * AMA00128!17.08.15!2.4  ! UST-BERECH-KZN integriert !Walbert
      *         !        !     ! für AMA00150              !
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG DEMA0128         !WARZ
      *         !        !     ! ÄNDERUNG DEMA0160 EINBEH. !
      *         !        !     !                           !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3241.
              07 PG                     pic 99.
              07 PE-MANR                pic 9(9).
              07 AVERM                  pic 9(9).
              07 PRODSCHL               pic 999.
              07 PRODART                pic 99.
              07 PROVAUS-NETTO          pic S9(9)V99   packed-decimal.
              07 PROVAUS-BRUTTO         pic S9(9)V99   packed-decimal.
              07 PROVEINB               pic S9(9)V99   packed-decimal.
              07 PE-AE                  pic S9(10)V9   packed-decimal.
              07 VM-BETEIL-KZN          pic X.
              07 DYN-KZN2               pic 9.
              07 BEWART                 pic 9(4).
              07 EIGEN-KZN              pic X.
              07 BAV-KZN                pic X.
              07 PROVAUS-UST            pic S9(9)V99   packed-decimal.
              07 UST-BERECH-KZN         pic X.
              07 ERWERB-FOLGE-KZN       pic X(8).
      *
      **************************************************************


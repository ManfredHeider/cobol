      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5025.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA5025.CPv  $
      *
      *   Rev 2.5   Oct 19 2017 14:24:12   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   Oct 16 2015 11:40:24   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:43:12   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Nov 14 2013 15:28:56   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.1   Oct 09 2013 14:17:00   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.0   Jul 16 2013 08:43:30   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5003   MA-DATEN ANREICHERN
      *
      *     SOURCE-NAME      :  DRMA5025
      *
      *     GENERIERUNG      :  02.10.2017/13:39:16
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!15.07.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!14.11.13!2.2  ! AKTUALISIEREN LOGBUCH     !VON HELDEN
      * AMA00128!05.05.15!2.3  ! ERGÄNZEN FELDER           ! WALBERT
      *         !01.07.15!     !ERGÄNZEN PROVAUS-NETTO;-UST!
      * AMA00128!28.09.15!2.4  ! ERSETZEN PROVAUS-NETTO    ! D.WILHELM
      *         !        !     ! DURCH PROV-AUS            !
      *         !        !     ! PROVAUS-UST DURCH PROV-UST!
      *         !        !     ! PROV-RUECK NEU            !
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG DEMA0128         ! S.WARZ
      *         !        !     ! ÄNDERUNG DEMA0448 EINBEH. !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5025.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 TARIF-KZN              pic 99.
              07 SAMMEL-NR              pic 9(9).
              07 POL-DAT                pic X(10).
              07 BUZ-TARIF              pic 9(4).
              07 RAB-STF                pic X(4).
              07 BESVV                  pic 9.
              07 PRODSCHL               pic 999.
              07 VBNR                   pic 9(9).
              07 PR-PFL-SU              pic S9(9)V99   packed-decimal.
              07 RWERTSU                pic S9(9)V99   packed-decimal.
              07 DYN-KZN2               pic 9.
              07 PRODART                pic 99.
              07 VERTRAGS-ID            pic X(15).
              07 PROV-AUS               pic S9(9)V99   packed-decimal.
              07 PROV-EINB              pic S9(9)V99   packed-decimal.
              07 PROV-UST               pic S9(9)V99   packed-decimal.
      *
      **************************************************************


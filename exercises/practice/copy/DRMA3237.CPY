      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3237.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRMA3237.CPv  $
      *
      *   Rev 2.9   Oct 19 2017 14:18:28   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.8   Mar 16 2017 13:12:34   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.7   Mar 21 2016 19:18:32   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.6   Sep 25 2015 12:15:04   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.5   Jul 28 2015 14:49:18   t13ac19
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.4   Oct 15 2013 10:07:40   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.3   Oct 09 2013 14:15:36   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 22 2013 11:55:24   p13sp14
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 11:44:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINZELNACHWEIS - VAR. TEIL (DAISY)
      *
      *     SOURCE-NAME      :  DRMA3237
      *
      *     GENERIERUNG      :  02.10.2017/13:15:49
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00018!20.06.13!2.3  ! ANPASSUNGEN DAISY         !VON HELDEN
      * AMA00018!15.10.13!2.4  ! ANPASSUNGEN LOGBUCH       !VON HELDEN
      * AMA00129!20.07.15!2.5  ! ÄNDERUNG DGMA0102         ! O.WILHELM
      * AMA00199!23.09.15!2.6  ! ÄNDERUNG DGMA0102         ! D.WILHELM
      * AMA00278!09.03.16!2.7  ! ÄNDERUNG DGMA0102         ! O.WILHELM
      * AMA00297!21.02.17!2.8  ! EINFÜGEN FLEXN-FELDER     ! S.WARZ
      * AMA00360!20.09.17!2.9  ! DGMA0102 GEÄNDERT         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DAISY-DATEN.
              07 DGMA0102.
                 09 VN                  pic X(20).
                 09 VT                  pic X(20).
                 09 DGV                 pic 9(5).
                 09 AVERM               pic 9(9).
                 09 PRODART-VG          pic 99.
                 09 HZRBEG              pic X(10).
                 09 HZREND              pic X(10).
                 09 PRO-RATA-SU         pic S9(9)V99   packed-decimal.
                 09 VM-BETEIL-KZN       pic X.
                 09 PR-PFL-SU           pic S9(9)V99   packed-decimal.
                 09 PR-PFL-SU-ABR       pic S9(9)V99   packed-decimal.
                 09 FAELLIG-VON         pic X(10).
                 09 FAELLIG-BIS         pic X(10).
                 09 PDPT-ID             pic S9(15)     packed-decimal.
                 09 VERTRAGS-ID         pic X(15).
                 09 REIHENFOLGE-NR      pic 999.
                 09 HAFTZEIT-KZN        pic XX.
                 09 BEW-ERSTELL-ZPKT    pic X(26).
                 09 DYN-KZN1            pic 9.
                 09 DYN-KZN2            pic 9.
                 09 PRO-RATA-ZAEHLER    pic 999.
                 09 PRO-RATA-NENNER     pic 999.
                 09 PRODSCHL            pic 999.
                 09 PROVSCHL            pic X(5).
                 09 RWERTSU             pic S9(9)V99   packed-decimal.
                 09 BEWSCHL             pic X(7).
                 09 PGV                 pic 9(4).
                 09 PG-RECH-NR          pic X(12).
                 09 ERWERB-FOLGE-KZN    pic X(8).
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 DGMA5008.
                    11 UST-KORREKTUR-KZN
                                        pic X.
                    11 KORREKTUR-ZIEL-STATUS
                                        pic X.
                    11 ABR-NR-KORR      pic X(15).
                    11 STEUERJAHR       pic X(4).
              07 BAV-KZN                pic X.
              07 VFM-KZN                pic X.
              07 EMP-TYP                pic 99.
              07 PRSATZ                 pic S999V99    packed-decimal.
              07 PE-MANR                pic 9(9).
              07 PE-AE                  pic S9(10)V9   packed-decimal.
              07 EIGEN-KZN              pic X.
              07 VSP-FLEXN-RELEVANZ     pic X.
              07 VSP-FLEXN-FOLGE-KZ     pic X.
              07 VSP-PRODUKT-KOMBI-NR   pic 9(5).
              07 VSP-PRODUKT-KOMBI-BEZ  pic XXX.
              07 VSP-FLEXN-BERECHTIGUNG pic X.
      *
      **************************************************************


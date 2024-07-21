      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5036.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5036.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:25:48   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Jul 28 2015 14:45:30   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Sep 30 2013 12:09:16   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Sep 24 2013 14:18:18   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.0   Sep 24 2013 13:49:30   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: MONATSBESTAND BEWEGUNGEN SPNP/AE-ST
      *
      *     SOURCE-NAME      :  DRMA5036
      *
      *     GENERIERUNG      :  02.10.2017/13:45:09
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!30.09.13!2.2  ! ÄNDERUNG PRO-RATA-SU-FONDS!VON HELDEN
      * AMA00128!06.05.15!2.3  ! INTEGRATION UST-FELDER    ! WALBERT
      *         !01.07.15!     ! GEÄNDERTE DGMA0114        !
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DGMA0114         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5036.
              07 DGMA0114.
                 09 DRMA3185.
                    11 PG               pic 99.
                    11 PRODSCHL-SORT    pic 999.
                    11 VERTRAGS-ID-SORT pic X(15).
                    11 TARIF-KZN        pic 99.
                 09 DGMA0126.
                    11 VN               pic X(20).
                    11 VT               pic X(20).
                    11 DGV              pic 9(5).
                    11 AVERM            pic 9(9).
                    11 PRODART-VG       pic 99.
                    11 HZRBEG           pic X(10).
                    11 HZREND           pic X(10).
                    11 PRO-RATA-SU      pic S9(9)V99   packed-decimal.
                    11 VM-BETEIL-KZN    pic X.
                    11 PR-PFL-SU        pic S9(9)V99   packed-decimal.
                    11 PR-PFL-SU-ABR    pic S9(9)V99   packed-decimal.
                    11 FAELLIG-VON      pic X(10).
                    11 FAELLIG-BIS      pic X(10).
                    11 PDPT-ID          pic S9(15)     packed-decimal.
                    11 VERTRAGS-ID      pic X(15).
                    11 REIHENFOLGE-NR   pic 999.
                    11 HAFTZEIT-KZN     pic XX.
                    11 BEW-ERSTELL-ZPKT pic X(26).
                    11 DYN-KZN1         pic 9.
                    11 DYN-KZN2         pic 9.
                    11 PRO-RATA-ZAEHLER pic 999.
                    11 PRO-RATA-NENNER  pic 999.
                    11 PRODSCHL         pic 999.
                    11 PROVSCHL         pic X(5).
                    11 RWERTSU          pic S9(9)V99   packed-decimal.
                    11 BEWSCHL          pic X(7).
                    11 PGV              pic 9(4).
                 09 BUCHDAT             pic X(10).
                 09 POL-DAT             pic X(10).
                 09 RABSTUFE            pic X(5).
                 09 SAMNR               pic 9(9)       packed-decimal.
                 09 BESVV               pic 9.
                 09 BUZTAR              pic 9(4).
                 09 DAUER               pic S9(4)      binary.
                 09 ZAHLW               pic 99.
                 09 INKST-DATUM         pic X(10).
                 09 VN-STR              pic X(20).
                 09 VN-PLZ              pic X(5).
                 09 VN-ORT              pic X(20).
                 09 PRODART-PG          pic 99.
                 09 EINH-BEW            pic S9(10)V9   packed-decimal.
                 09 PRO-RATA-SU-FONDS   pic S9(9)V99   packed-decimal.
                 09 ANZ-PE              pic 99.
                 09 PE-STRUKTUR         occurs 20.
                    11 PE-PROVAUS       pic S9(9)V99   packed-decimal.
                    11 PE-PROVEINB      pic S9(9)V99   packed-decimal.
                    11 PE-EMP-TYP       pic 99.
                    11 PE-PRSATZ        pic S999V99    packed-decimal.
                    11 PE-MANR          pic 9(9).
                    11 PE-AE            pic S9(10)V9   packed-decimal.
                    11 PE-EIGEN-KZN     pic X.
                    11 PE-BUCH-RELEVANT pic X.
      *
      **************************************************************


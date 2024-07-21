      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5006.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRMA5006.CPv  $
      *
      *   Rev 2.11   Oct 19 2017 14:22:24   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.10   Mar 16 2017 13:12:48   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.9   Mar 21 2016 19:19:10   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.8   Oct 16 2015 11:39:12   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.7   Sep 25 2015 12:15:18   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.6   Jul 28 2015 14:41:24   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.5   Nov 14 2013 15:28:02   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.4   Oct 09 2013 14:16:08   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.3   Jul 16 2013 11:37:18   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 10 2013 17:38:32   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Jul 03 2013 11:35:38   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.0   Jun 21 2013 15:45:14   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: EINZELNACHWEIS PROVISION JE MA
      *
      *     SOURCE-NAME      :  DRMA5006
      *
      *     GENERIERUNG      :  02.10.2017/13:32:13
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00018!21.06.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!03.07.13!2.2  ! "SPIELEN" MIT EINCHECKEN  !VON HELDEN
      * AMA00018!10.07.13!2.3  ! "SPIELEN" MIT EINCHECKEN  !VON HELDEN
      * AMA00018!16.07.13!2.4  ! "SPIELEN" MIT EINCHECKEN  !VON HELDEN
      * AMA00018!14.11.13!2.5  ! AKTUALISIEREN LOGBUCH     !VON HELDEN
      * AMA00128!29.04.15!2.6  ! UST.-FELDER INTEGRIEREN   ! WALBERT
      *         !11.05.15!     ! FAELLIG-BIS INTEGRIEREN   ! WALBERT
      *         !01.07.15!     ! GEÄNDERTE DGMA3476        ! WALBERT
      *         !20.07.15!     ! GEÄNDERTE DGMA3476        ! WALBERT
      * AMA00128!24.09.15!2.7  ! GEÄNDERTE DGMA3476        ! D.WILHELM
      * AMA00128!28.09.15!2.8  ! GEÄNDERTE DGMA3476        ! D.WILHELM
      * AMA00278!09.03.16!2.9  ! GEÄNDERTE DGMA3476        ! O.WILHELM
      * AMA00297!21.02.17!2.10 ! GEÄNDERTE DGMA3476        ! S.WARZ
      * AMA00360!20.09.17!2.11 ! GEÄNDERTE DGMA3476        ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5006.
              07 DGMA3476.
                 09 DRMA3185.
                    11 PG               pic 99.
                    11 PRODSCHL-SORT    pic 999.
                    11 VERTRAGS-ID-SORT pic X(15).
                    11 TARIF-KZN        pic 99.
                 09 DAISY-DATEN.
                    11 DGMA0102.
                       13 VN            pic X(20).
                       13 VT            pic X(20).
                       13 DGV           pic 9(5).
                       13 AVERM         pic 9(9).
                       13 PRODART-VG    pic 99.
                       13 HZRBEG        pic X(10).
                       13 HZREND        pic X(10).
                       13 PRO-RATA-SU   pic S9(9)V99   packed-decimal.
                       13 VM-BETEIL-KZN pic X.
                       13 PR-PFL-SU     pic S9(9)V99   packed-decimal.
                       13 PR-PFL-SU-ABR pic S9(9)V99   packed-decimal.
                       13 FAELLIG-VON   pic X(10).
                       13 FAELLIG-BIS   pic X(10).
                       13 PDPT-ID       pic S9(15)     packed-decimal.
                       13 VERTRAGS-ID   pic X(15).
                       13 REIHENFOLGE-NR
                                        pic 999.
                       13 HAFTZEIT-KZN  pic XX.
                       13 BEW-ERSTELL-ZPKT
                                        pic X(26).
                       13 DYN-KZN1      pic 9.
                       13 DYN-KZN2      pic 9.
                       13 PRO-RATA-ZAEHLER
                                        pic 999.
                       13 PRO-RATA-NENNER
                                        pic 999.
                       13 PRODSCHL      pic 999.
                       13 PROVSCHL      pic X(5).
                       13 RWERTSU       pic S9(9)V99   packed-decimal.
                       13 BEWSCHL       pic X(7).
                       13 PGV           pic 9(4).
                       13 PG-RECH-NR    pic X(12).
                       13 ERWERB-FOLGE-KZN
                                        pic X(8).
                       13 DGMA5007.
                          15 UST-BERECH-KZN
                                        pic X.
                          15 UST-SATZ   pic S999V99    packed-decimal.
                          15 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                          15 PROV-AUS-EINB
                                        pic S9(9)V99   packed-decimal.
                          15 PROV-AUS   pic S9(9)V99   packed-decimal.
                          15 PROV-EINB  pic S9(9)V99   packed-decimal.
                          15 PROV-UST   pic S9(9)V99   packed-decimal.
                       13 DGMA5008.
                          15 UST-KORREKTUR-KZN
                                        pic X.
                          15 KORREKTUR-ZIEL-STATUS
                                        pic X.
                          15 ABR-NR-KORR
                                        pic X(15).
                          15 STEUERJAHR pic X(4).
                    11 BAV-KZN          pic X.
                    11 VFM-KZN          pic X.
                    11 EMP-TYP          pic 99.
                    11 PRSATZ           pic S999V99    packed-decimal.
                    11 PE-MANR          pic 9(9).
                    11 PE-AE            pic S9(10)V9   packed-decimal.
                    11 EIGEN-KZN        pic X.
                    11 VSP-FLEXN-RELEVANZ
                                        pic X.
                    11 VSP-FLEXN-FOLGE-KZ
                                        pic X.
                    11 VSP-PRODUKT-KOMBI-NR
                                        pic 9(5).
                    11 VSP-PRODUKT-KOMBI-BEZ
                                        pic XXX.
                    11 VSP-FLEXN-BERECHTIGUNG
                                        pic X.
                 09 BUCHDAT             pic X(10).
      *
      **************************************************************


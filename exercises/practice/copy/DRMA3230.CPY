      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3230.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRMA3230.CPv  $
      *
      *   Rev 2.11   Oct 19 2017 14:18:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.10   Mar 16 2017 13:12:20   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.9   May 18 2016 11:19:30   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.8   Mar 21 2016 19:18:18   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.7   Sep 25 2015 12:14:48   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.6   Jul 28 2015 14:49:02   t13ac19
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.5   Oct 14 2013 14:16:48   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.4   Jul 24 2013 15:28:16   p13sp14
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.3   Jul 23 2013 15:37:24   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.2   Jul 22 2013 09:26:40   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 11:44:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINZELNACHWEIS DER SALDENVERÄNDERUNG AUS SST - NEU
      *
      *     SOURCE-NAME      :  DRMA3230
      *
      *     GENERIERUNG      :  12.10.2017/07:32:23
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00018!24.09.13!2.2  ! ÜBERNAHME AUS MIGRATION   ! WALBERT
      * AMA00018!        !2.3  ! ÄNDERUNG ??               ! P13SP14
      * AMA00018!        !2.4  ! ÄNDERUNG ??               ! WALBERT
      * AMA00018!01.10.13!2.5  ! DAISY ANPASSUNG           ! WALBERT
      * AMA00129!20.07.15!2.6  ! ÄNDERUNGEN WEGEN LUCY     ! O.WILHELM
      *         !        !     ! DRMA5083 NEU              !
      *         !        !     ! DGMA0109 GELÖSCHT         !
      *         !        !     ! DEMA0181 GELÖSCHT         !
      *         !        !     ! DRMA3237 GELÖSCHT         !
      *         !        !     ! DRMA3469 GELÖSCHT         !
      *         !        !     ! DRMA0791 GELÖSCHT         !
      *         !        !     ! DRMA4301 GELÖSCHT         !
      *         !        !     ! DRMA4349 GELÖSCHT         !
      * AMA00199!23.09.15!2.7  ! ÄNDERUNGEN WEGEN LUCY     ! D.WILHELM
      *         !        !     ! DRMA5083 GEÄNDERT         !
      * AMA00278!09.03.16!2.8  ! ÄNDERUNG DRMA5083         ! O.WILHELM
      * AMA00263!18.04.16!2.9  ! ÄNDERUNG DRMA5083         ! KUBE
      * AMA00297!21.02.17!2.10 ! ÄNDERUNG DRMA5083         ! S.WARZ
      * AMA00360!20.09.17!2.11 ! ÄNDERUNG DRMA5083         ! S.WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3230.
              07 DRMA5083.
                 09 BUCHUNGSDATEN.
                    11 SALDENDATEN.
                       13 VBNR          pic 9(9).
                       13 FACH-KENNUNG  pic X(10).
                       13 DV-KENNUNG    pic X(10).
                       13 BUCHDAT       pic X(10).
                       13 KTO-KZN-1     pic 9.
                       13 BNGBETR-1     pic S9(9)V99   packed-decimal.
                       13 KTO-KZN-2     pic X.
                       13 BNGBETR-2     pic S9(9)V99   packed-decimal.
                    11 SORT-FELDER.
                       13 PG            pic 99.
                       13 PRODSCHL-SORT pic 999.
                       13 VERTRAGS-ID-SORT
                                        pic X(15).
                       13 TARIF-KZN     pic 99.
                 09 SST-DATEN           pic X(400).
                 09 DAISY-DATEN redefines SST-DATEN.
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
                 09 DAISY-PG-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 ERWERB-FOLGE-KZN pic X(8).
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-EINB pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-EINB     pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                 09 FIBU-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 FIBU-SORT-ID     pic 9(4).
                    11 FIBU-SORT-LFDNR  pic 9(4).
                    11 UST-PFLICHTIG-KZN
                                        pic X.
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-EINB pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-EINB     pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                    11 DGMA5008.
                       13 UST-KORREKTUR-KZN
                                        pic X.
                       13 KORREKTUR-ZIEL-STATUS
                                        pic X.
                       13 ABR-NR-KORR   pic X(15).
                       13 STEUERJAHR    pic X(4).
                    11 REFERENZ-NR      pic X(16).
                    11 KNR-VERARB-TYP   pic S9(4)      binary.
                 09 SONDLST-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 UST-PFLICHTIG-KZN
                                        pic X.
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-EINB pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-EINB     pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                 09 SONSTIGE-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
      *
      **************************************************************


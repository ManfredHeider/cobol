      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5073.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRMA5073.CPv  $
      *
      *   Rev 2.8   Oct 19 2017 14:27:48   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.7   Mar 16 2017 13:14:26   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.6   May 18 2016 11:20:12   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.5   Mar 21 2016 19:22:00   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.4   Sep 24 2015 14:10:48   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Copies/Programme
      *
      *   Rev 2.3   Jul 28 2015 08:25:18   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.2   Jun 26 2015 17:44:56   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.1   Apr 02 2015 12:08:56   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme
      *
      *   Rev 2.0   Apr 01 2015 16:06:28   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5025   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5073
      *
      *     GENERIERUNG      :  02.10.2017/14:11:02
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!02.04.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00148!19.06.15!2.2  ! ÄNDERUNG DRMA5083         ! WOLFF
      * AMA00148!27.07.15!2.3  ! ÄNDERUNG DRMA5083         ! WOLFF
      * AMA00148!24.09.15!2.4  ! ÄNDERUNG DRMA5083         ! WOLFF
      * AMA00278!21.03.16!2.5  ! ÄNDERUNG DRMA5083         ! O.WILHELM
      * AMA00297!21.02.17!2.6  ! ÄNDERUNG DRMA5083         ! S.WARZ
      * AMA00360!20.09.17!2.7  ! ÄNDERUNG DRMA5083 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5073.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 AUSWAHL-FUNKTION       pic X(10).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
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


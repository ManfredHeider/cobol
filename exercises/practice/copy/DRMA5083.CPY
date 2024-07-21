      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5083.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRMA5083.CPv  $
      *
      *   Rev 2.8   Oct 19 2017 14:28:22   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.7   Mar 16 2017 13:14:34   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.6   May 18 2016 11:20:22   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.5   Mar 21 2016 19:22:14   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.4   Sep 24 2015 14:11:02   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Copies/Programme
      *
      *   Rev 2.3   Jul 30 2015 07:42:46   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.2   Jun 26 2015 17:45:08   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.1   Mar 31 2015 10:01:12   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme
      *
      *   Rev 2.0   Mar 23 2015 09:01:56   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: EINZELNACHWEIS DER SALDENVERÄNDERUNG AUS
      *
      *     SOURCE-NAME      :  DRMA5083
      *
      *     GENERIERUNG      :  02.10.2017/13:52:10
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!23.03.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00148!26.06.15!2.2  ! ÄNDERUNGEN WEGEN LUCY     ! O.WILHELM
      *         !        !     ! DGMA0109 GEÄNDERT         !
      *         !        !     ! DRMA3237 GEÄNDERT         !
      *         !        !     ! DRMA0791 GELÖSCHT         !
      *         !        !     ! DRMA4301 GELÖSCHT         !
      *         !        !     ! DRMA4349 GELÖSCHT         !
      *         !        !     ! DRMA5108 NEU              !
      *         !        !     ! DRMA3469 GEÄNDERT         !
      *         !        !     ! DRMA5109 NEU              !
      * AMA00148!20.07.15!2.3  ! DRMA3237 GEÄNDERT         ! O.WILHELM
      * AMA00199!23.09.15!2.4  ! DRMA3237 GEÄNDERT         ! D.WILHELM
      *         !        !     ! DRMA5108 GEÄNDERT         !
      * AMA00278!09.03.16!2.5  ! ÄNDERUNG DRMA3237         ! O.WILHELM
      *         !        !     ! ÄNDERUNG DRMA5108         ! O.WILHELM
      * AMA00263!25.04.16!2.6  ! ÄNDERUNG DRMA3469         ! KUBE
      *         !13.05.16!     ! DGMA5010 NEU              ! WOLFF
      * AMA00297!21.02.17!2.7  ! ÄNDERUNG DRMA3237         ! S.WARZ
      * AMA00360!20.09.17!2.8  ! ÄNDERUNG DRMA3237         ! S.WARZ
      *         !        !     ! ÄNDERUNG DGMA5010         !
      *         !        !     ! ÄNDERUNG DRMA5108         !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5083.
              07 BUCHUNGSDATEN.
                 09 SALDENDATEN.
                    11 VBNR             pic 9(9).
                    11 FACH-KENNUNG     pic X(10).
                    11 DV-KENNUNG       pic X(10).
                    11 BUCHDAT          pic X(10).
                    11 KTO-KZN-1        pic 9.
                    11 BNGBETR-1        pic S9(9)V99   packed-decimal.
                    11 KTO-KZN-2        pic X.
                    11 BNGBETR-2        pic S9(9)V99   packed-decimal.
                 09 SORT-FELDER.
                    11 PG               pic 99.
                    11 PRODSCHL-SORT    pic 999.
                    11 VERTRAGS-ID-SORT pic X(15).
                    11 TARIF-KZN        pic 99.
              07 SST-DATEN              pic X(400).
              07 DAISY-DATEN redefines SST-DATEN.
                 09 DGMA0102.
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
                    11 PG-RECH-NR       pic X(12).
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
                    11 DGMA5008.
                       13 UST-KORREKTUR-KZN
                                        pic X.
                       13 KORREKTUR-ZIEL-STATUS
                                        pic X.
                       13 ABR-NR-KORR   pic X(15).
                       13 STEUERJAHR    pic X(4).
                 09 BAV-KZN             pic X.
                 09 VFM-KZN             pic X.
                 09 EMP-TYP             pic 99.
                 09 PRSATZ              pic S999V99    packed-decimal.
                 09 PE-MANR             pic 9(9).
                 09 PE-AE               pic S9(10)V9   packed-decimal.
                 09 EIGEN-KZN           pic X.
                 09 VSP-FLEXN-RELEVANZ  pic X.
                 09 VSP-FLEXN-FOLGE-KZ  pic X.
                 09 VSP-PRODUKT-KOMBI-NR
                                        pic 9(5).
                 09 VSP-PRODUKT-KOMBI-BEZ
                                        pic XXX.
                 09 VSP-FLEXN-BERECHTIGUNG
                                        pic X.
              07 DAISY-PG-DATEN redefines SST-DATEN.
                 09 BUCHTEXT            pic X(92).
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
              07 FIBU-DATEN redefines SST-DATEN.
                 09 BUCHTEXT            pic X(92).
                 09 FIBU-SORT-ID        pic 9(4).
                 09 FIBU-SORT-LFDNR     pic 9(4).
                 09 UST-PFLICHTIG-KZN   pic X.
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
                 09 REFERENZ-NR         pic X(16).
                 09 KNR-VERARB-TYP      pic S9(4)      binary.
              07 SONDLST-DATEN redefines SST-DATEN.
                 09 BUCHTEXT            pic X(92).
                 09 UST-PFLICHTIG-KZN   pic X.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
              07 SONSTIGE-DATEN redefines SST-DATEN.
                 09 BUCHTEXT            pic X(92).
      *
      **************************************************************


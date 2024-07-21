      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.06.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5008.CPY  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/mf/CPY/DRMA5008.CPv  $
      *
      *   Rev 2.11   Oct 19 2017 14:22:46   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.10   Mar 16 2017 13:13:04   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.9   Mar 21 2016 19:19:38   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.8   Oct 16 2015 11:39:42   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.7   Jul 28 2015 14:41:54   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.6   Oct 07 2013 17:10:56   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.5   Oct 01 2013 09:15:58   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.4   Jul 16 2013 13:42:48   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.3   Jul 16 2013 11:37:36   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 10 2013 17:38:50   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Jul 03 2013 08:59:16   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.0   Jun 21 2013 15:45:58   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: NORMIERTE PROVISIONSBEWEGUNG
      *
      *     SOURCE-NAME      :  DRMA5008
      *
      *     GENERIERUNG      :  02.10.2017/13:33:38
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
      * AMA00018!24.09.13!2.5  ! ÄNDERUNG BEI NEUERSTELLUNG!VON HELDEN
      * AMA00018!07.10.13!2.6  ! ÄNDERUNG BEI NEUERSTELLUNG!VON HELDEN
      * AMA00128!29.04.15!2.7  ! UST.-FELDER INTEGRIEREN   ! WALBERT
      *         !20.07.15!     ! NEU: KORREKTUR-ZIEL-STATUS! WALBERT
      * AMA00128!12.10.15!2.8  ! DGMA0105 GEÄNDERT         ! D.WILHELM
      * AMA00278!04.03.16!2.9  ! ÄNDERUNG DGMA0105         ! O.WILHELM
      * AMA00297!21.02.17!2.10 ! ÄNDERUNG DGMA0105         ! S.WARZ
      * AMA00360!20.09.17!2.11 ! ÄNDERUNG DGMA0105         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5008.
              07 DGMA0105.
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
                 09 VN-STR              pic X(40).
                 09 VN-AUSL-KZN         pic X(4).
                 09 VN-PLZ              pic X(9).
                 09 VN-ORT              pic X(40).
                 09 PRODART-PG          pic 99.
                 09 EINH-BEW            pic S9(10)V9   packed-decimal.
                 09 TARIFBEZ-PG         pic X(15).
                 09 PRO-RATA-SU-FONDS   pic S9(9)V99   packed-decimal.
                 09 UST-KORREKTUR-KZN   pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PG-RECH-NR          pic X(12).
                 09 STEUERJAHR          pic X(4).
                 09 VSP-FLEXN-RELEVANZ  pic X.
                 09 VSP-FLEXN-FOLGE-KZ  pic X.
                 09 VSP-PRODUKT-KOMBI-NR
                                        pic S9(5)      binary.
                 09 VSP-PRODUKT-KOMBI-BEZ
                                        pic XXX.
                 09 VSP-FLEXN-BERECHTIGUNG
                                        pic X.
                 09 ANZ-PE              pic 99.
                 09 PE-STRUKTUR         occurs 20.
                    11 PE-PROVAUS-NETTO pic S9(9)V99   packed-decimal.
                    11 PE-PROVEINB      pic S9(9)V99   packed-decimal.
                    11 PE-EMP-TYP       pic 99.
                    11 PE-PRSATZ        pic S999V99    packed-decimal.
                    11 PE-MANR          pic 9(9).
                    11 PE-AE            pic S9(10)V9   packed-decimal.
                    11 PE-EIGEN-KZN     pic X.
                    11 PE-BUCH-RELEVANT pic X.
                    11 PE-PROVAUS-UST   pic S9(9)V99   packed-decimal.
                    11 PE-UST-BERECH-KZN
                                        pic X.
                    11 PE-ABR-NR-KORR   pic X(15).
                    11 KORREKTUR-ZIEL-STATUS
                                        pic X.
      *
      **************************************************************


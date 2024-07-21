      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5030.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRMA5030.CPv  $
      *
      *   Rev 2.7   Oct 19 2017 14:25:06   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.6   Mar 16 2017 13:13:48   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.5   Mar 21 2016 19:20:56   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.4   Oct 16 2015 11:41:34   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:44:30   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Oct 07 2013 17:11:28   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Oct 01 2013 09:16:32   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.0   Aug 08 2013 18:19:50   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5008 BEWEGUNGS-SST-DATEN NORMIEREN
      *
      *     SOURCE-NAME      :  DRMA5030
      *
      *     GENERIERUNG      :  02.10.2017/13:42:36
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00018!08.08.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!07.10.13!2.2  ! ÄNDERUNG BEI NEUERSTELLUNG!VON HELDEN
      * AMA00128!05.05.15!2.3  ! UST.-FELDER INTEGRIEREN   ! WALBERT
      *         !20.07.15!     ! NEUE DGMA0105 INTEGRIERT  ! WALBERT
      * AMA00128!12.10.15!2.4  ! DGMA0105 GEÄNDERT         ! D.WILHELM
      * AMA00278!04.03.16!2.5  ! ÄNDERUNG DGMA0105         ! O.WILHELM
      * AMA00297!21.02.17!2.6  ! ÄNDERUNG DGMA0105         ! S.WARZ
      * AMA00360!20.09.17!2.7  ! ÄNDERUNG DGMA0105         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5030.
              07 STATUS-SQL-BEREICH.
                 09 STATUS-BEREICH.
                    11 PROGRAMMNAME     pic X(8).
                    11 RC               pic 99.
                    11 MELDUNGS-BESCHREIBUNG.
                       13 MELDUNGS-NR   pic 9(8).
                       13 SUB-VAR-1     pic X(20).
                       13 SUB-VAR-2     pic X(20).
                    11 FEHL-BER-ALT     pic X(45).
                    11 FILLER           occurs 47
                                        pic X.
                 09 SQL-BEREICH.
                    11 VIEWNAME         pic X(18).
                    11 OPERATION        pic XXX.
           88 KST-SELECT                value 'SEL'.
           88 KST-INSERT                value 'INS'.
           88 KST-UPDATE                value 'UPD'.
           88 KST-DELETE                value 'DEL'.
           88 KST-START                 value 'ST '.
           88 KST-BACKWARD              value 'BW '.
           88 KST-FORWARD               value 'FW '.
           88 KST-OPEN                  value 'OPE'.
           88 KST-CLOSE                 value 'CLO'.
           88 KST-FETCH                 value 'FET'.
                    11 AUS-SQLCA.
                       13 SQLAID        pic X(8).
                       13 SQLABC        pic S9(9)      binary.
                       13 AUS-SQLCODE   pic S9(9)      binary.
                       13 SQLERRM.
                          15 SQLERRML   pic S9(4)      binary.
                          15 SQLERRMC   pic X(70).
                       13 SQLERRP       pic X(8).
                       13 SQLERRD       occurs 6
                                        pic S9(9)      binary.
                       13 SQLWARN.
                          15 SQLWARN0   pic X.
                          15 SQLWARN1   pic X.
                          15 SQLWARN2   pic X.
                          15 SQLWARN3   pic X.
                          15 SQLWARN4   pic X.
                          15 SQLWARN5   pic X.
                          15 SQLWARN6   pic X.
                          15 SQLWARN7   pic X.
                       13 SQLEXT.
                          15 SQLWARN8   pic X.
                          15 SQLWARN9   pic X.
                          15 SQLWARNA   pic X.
                          15 AUS-SQLSTATE
                                        pic X(5).
                    11 FILLER           occurs 93
                                        pic X.
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


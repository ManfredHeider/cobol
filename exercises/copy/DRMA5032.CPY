      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5032.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRMA5032.CPv  $
      *
      *   Rev 2.7   Oct 19 2017 14:25:28   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.6   Mar 16 2017 13:14:04   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.5   Mar 21 2016 19:21:22   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.4   Oct 16 2015 11:42:02   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:45:00   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Oct 07 2013 17:12:00   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Oct 01 2013 09:17:04   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.0   Aug 08 2013 18:20:34   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5009  BEWEGUNGS-SST-DATEN PRÜFEN
      *
      *     SOURCE-NAME      :  DRMA5032
      *
      *     GENERIERUNG      :  02.10.2017/13:43:47
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00018!08.08.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!07.10.13!2.2  ! ÄNDERUNG BEI NEUERSTELLUNG!VON HELDEN
      * AMA00128!05.05.15!2.3  ! UST.-FELDER INTEGRIEREN   ! WALBERT
      * AMA00128!12.10.15!2.4  ! ÄNDERUNG DGMA0104         ! D.WILHELM
      * AMA00278!04.03.16!2.5  ! ÄNDERUNG DGMA0104         ! O.WILHELM
      * AMA00297!21.02.17!2.6  ! ÄNDERUNG DGMA0104         ! S.WARZ
      * AMA00360!20.09.17!2.7  ! ÄNDERUNG DGMA0104         ! S.WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5032.
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
              07 AZ-UAFILE1             pic S9(9)      packed-decimal.
              07 DGMA0104.
                 09 SATZ-KZ             pic X.
                 09 PGEB-GES            pic S9(4)      binary.
                 09 PROD-PROD-SCHL-SORT pic S9(4)      binary.
                 09 VNR-SORT            pic X(15).
                 09 TARIF-KZ            pic S9(4)      binary.
                 09 TARIF-BEZEICHNUNG   pic X(15).
                 09 KUNDENNAME          pic X(20).
                 09 VP-NAME             pic X(20).
                 09 DGV                 pic S9(4)      binary.
                 09 AVERM               pic 9(7)       binary.
                 09 PRODART             pic 9(4)       binary.
                 09 HAFTZ-VON           pic X(10).
                 09 HAFTZ-BIS           pic X(10).
                 09 PRO-RATA-SUMME      pic S9(9)V99   packed-decimal.
                 09 PRO-RATA-SUMME-GES  pic S9(9)V99   packed-decimal.
                 09 VM-GESCHFT-KZ       pic X.
                 09 PPFL-SUMME          pic S9(9)V99   packed-decimal.
                 09 FAELLIG-VON         pic X(10).
                 09 PDPT-ID             pic S9(15)     packed-decimal.
                 09 VNR                 pic X(15).
                 09 RFOLGE-NR           pic S9(4)      binary.
                 09 HAFTZEIT-KZ         pic XX.
                 09 FIRST-INSERT        pic X(26).
                 09 MA-ABR-DAT          pic X(10).
                 09 DYN-KZ-1            pic S9(4)      binary.
                 09 DYN-KZ-2            pic S9(4)      binary.
                 09 POL-DAT             pic X(10).
                 09 PRO-RATA-FAK-Z      pic S9(4)      binary.
                 09 PRO-RATA-FAK-N      pic S9(4)      binary.
                 09 PROD-PROD-SCHL      pic S9(4)      binary.
                 09 PROV-SCHL           pic X(5).
                 09 RAB-STF             pic S9(4)      binary.
                 09 RWERT-SUMME         pic S9(9)V99   packed-decimal.
                 09 SAMMEL-NR           pic S9(9)      binary.
                 09 BESVV               pic 9.
                 09 PGV                 pic S9(4)      binary.
                 09 BUZ-TARIF           pic S9(4)      binary.
                 09 VERTRAGSDAUER       pic 9(04).
                 09 ZAHLWEISE           pic S9(4)      binary.
                 09 INKASSOSTAND        pic X(10).
                 09 VN-STRASSE          pic X(30).
                 09 VN-PLZ              pic X(5).
                 09 VN-ORT              pic X(30).
                 09 PG-PRODART          pic S9(4)      binary.
                 09 EINH-BEW            pic S9(6)V9    packed-decimal.
                 09 BEWSCHL             pic X(7).
                 09 UST-KORREKTUR-KZ    pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PG-RECH-NR          pic X(12).
                 09 FAELLIG-BIS         pic X(10).
                 09 STEUERJAHR          pic X(4).
                 09 VSP-FLEXN-RELEVANZ  pic X.
                 09 VSP-FLEXN-FOLGE-KZ  pic X.
                 09 VSP-PRODUKT-KOMBI-NR
                                        pic S9(5)      binary.
                 09 VSP-PRODUKT-KOMBI-BEZ
                                        pic XXX.
                 09 VSP-FLEXN-BERECHTIGUNG
                                        pic X.
                 09 ANZAHL-PROV-EMP     pic S9(4)      binary.
                 09 SATZLAENGE          pic S9(4)      binary.
                 09 STRUKTUR            occurs 20.
                    11 PROV-BETRAG-AUS  pic S9(9)V99   packed-decimal.
                    11 UST-BETRAG-AUS   pic S9(9)V99   packed-decimal.
                    11 PROV-BETRAG-EINB pic S9(9)V99   packed-decimal.
                    11 UST-BETRAG-EINB  pic S9(9)V99   packed-decimal.
                    11 BUCH-RELEVANT    pic X.
                    11 EMP-TYP          pic S9(4)      binary.
                    11 DIFF-PROV-STF    pic S9(4)      binary.
                    11 ST-NR            pic 9(7)       binary.
                    11 EINH-EMP         pic S9(6)V9    packed-decimal.
                    11 EIGEN-KZ         pic X.
                    11 UST-BERECH-KZ    pic X.
                    11 UST-ABR-NR       pic X(15).
                    11 KORREKTUR-ZIEL-STATUS
                                        pic X.
      *
      **************************************************************


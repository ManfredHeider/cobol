      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5078.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5078.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:28:00   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Jul 28 2015 15:42:32   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.2   Mar 20 2015 10:44:38   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.1   Mar 19 2015 17:24:52   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.0   Mar 18 2015 16:42:06   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5027   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5078
      *
      *     GENERIERUNG      :  02.10.2017/13:50:53
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!18.03.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00148!20.03.15!2.2  ! ÄNDERUNG DRMA5081         ! WOLFF
      * AMA00128!08.06.15!2.3  ! ÄNDERUNG DRMA5081         ! D.WILHELM
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DRMA5081         ! S.WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5078.
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
              07 DRMA5081.
                 09 VERSION             pic X.
                 09 VG                  pic 9(4).
                 09 PG                  pic 99.
                 09 PRODSCHL            pic 999.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 PROD-BEZEICH-KURZ   pic X(10).
                 09 PROD-BEZEICH-LANG   pic X(40).
                 09 PRODSPARTE          pic S9(4)      binary.
                 09 KNR-ART-PROV-TYP    pic S9(4)      binary.
                 09 IND-JW              pic S9(4)      binary.
                 09 PRODART-MIN         pic XX.
                 09 PRODART-MAX         pic XX.
                 09 TARIFBEZ            pic X(8).
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 SL-KZN              pic S9(4)      binary.
                 09 ZUSATZTAR-KZN       pic X.
                 09 ZUSATZTAR-BEZEICH   pic X(16).
                 09 ERWERB-FOLGE-KZN    pic X.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************


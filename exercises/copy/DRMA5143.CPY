      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  28.04.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5143.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5143.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:31:06   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   May 18 2016 11:25:38   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   Apr 28 2016 14:00:08   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5044   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5143
      *
      *     GENERIERUNG      :  02.10.2017/14:06:22
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00270!29.04.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DGMA5009 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5143.
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
              07 DGMA5009.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
              07 DGMA5009-KU.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
              07 DGMA5009-U.
                 09 DGMA5007.
                    11 UST-BERECH-KZN   pic X.
                    11 UST-SATZ         pic S999V99    packed-decimal.
                    11 PROV-AUS-EINB-UST
                                        pic S9(9)V99   packed-decimal.
                    11 PROV-AUS-EINB    pic S9(9)V99   packed-decimal.
                    11 PROV-AUS         pic S9(9)V99   packed-decimal.
                    11 PROV-EINB        pic S9(9)V99   packed-decimal.
                    11 PROV-UST         pic S9(9)V99   packed-decimal.
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
                 09 UST-SCHLUESSEL      pic S9(4)      binary.
      *
      **************************************************************


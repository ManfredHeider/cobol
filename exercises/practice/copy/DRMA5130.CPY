      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5130.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5130.CPv  $
      *
      *   Rev 2.3   Jan 05 2018 15:40:02   t13ex15
      *AMA00399: FiBu - Neue Buchungssystematik steuerpflichtige Ges
      *
      *   Rev 2.2   May 18 2016 11:21:16   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.1   Apr 20 2016 10:01:58   t13ac20
      *AMA00217: USt: LUCY II - Vorarbeiten / Redesign - Copies
      *
      *   Rev 2.0   Jan 19 2016 14:50:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5041   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5130
      *
      *     GENERIERUNG      :  05.01.2018/15:05:02
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00217!19.02.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00263!27.04.16!2.2  ! ÄNDERUNG DRMA5132         ! WOLFF
      * AMA00399!05.01.18!2.3  ! ÄNDERUNG DRMA5132         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5130.
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
              07 PROV-UST-VTF           pic S9(9)V99   packed-decimal.
              07 DRMA5132-TAB-MAX       pic S9(4)      binary.
              07 DRMA5132               occurs 20.
                 09 KONTO-IND           pic 9(4).
                 09 KONTO               pic X(12).
                 09 KONTO-BEZEICH       pic X(30).
                 09 SH-KZN              pic X.
                 09 BNGBETR             pic 9(14)V99.
                 09 GEGENKONTO          pic X(12).
                 09 BUCHTEXT            pic X(40).
                 09 KOSTENSTELLE        pic X(6).
                 09 KONTO-AUS           pic X(12).
      *
      **************************************************************


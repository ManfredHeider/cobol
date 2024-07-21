      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2351.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA2351.CPv  $
      *
      *   Rev 2.5   Sep 20 2018 15:02:28   t13ex15
      *AMA00400: Generali Allfinanz AG Copy Einbehalt
      *
      *   Rev 2.4   Oct 19 2017 14:14:16   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Oct 14 2013 10:13:08   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.2   Jul 23 2013 18:09:40   t13ac19
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 11:31:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA2349   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA2351
      *
      *     GENERIERUNG      :  21.08.2018/10:48:00
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!04.09.13!2.1  ! ÄNDERUNG DRMA3475         ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DRMA3475         ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2351.
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
              07 TAB-MAX                pic 9(4).
              07 DRMA3475               occurs 100
                                        indexed by DRMA2351-IND.
                 09 FILE-KEY.
                    11 VBNR             pic 9(9).
                    11 PG               pic 99.
                    11 VERTRAGS-ID      pic X(15).
                    11 STRUK-BEGRIFF    pic X(10).
                    11 PRODSCHL         pic 999.
                    11 REIHENFOLGE      pic X(26).
                 09 BEGINN              pic X(7).
                 09 ABLAUF              pic X(7).
                 09 PROVAUS             pic S9(9)V99   packed-decimal.
                 09 PROVEINB            pic S9(9)V99   packed-decimal.
                 09 PROVBRUTTO          pic S9(9)V99   packed-decimal.
                 09 GEVO-AKT            pic 9(5).
                 09 HAFTZEIT-KZN        pic XX.
                 09 HZRBEG-VB           pic X(10).
                 09 HZREND-VB           pic X(10).
                 09 HZRBEG-PG           pic X(10).
                 09 HZREND-PG           pic X(10).
                 09 VN                  pic X(20).
                 09 VERF-ALT-NEU-KZN    pic X.
                 09 NVP                 pic S9(9)V99   packed-decimal.
                 09 BUCHDAT             pic X(10).
                 09 PRODART             pic 99.
                 09 GEL-KENNUNG         pic X(6).
      *
      **************************************************************


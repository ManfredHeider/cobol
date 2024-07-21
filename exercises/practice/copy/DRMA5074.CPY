      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.04.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5074.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5074.CPv  $
      *
      *   Rev 2.2   Aug 21 2015 14:34:08   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.1   Apr 02 2015 12:09:08   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme
      *
      *   Rev 2.0   Apr 01 2015 16:06:50   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5025   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5074
      *
      *     GENERIERUNG      :  21.08.2015/14:30:25
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!02.04.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00148!21.08.15!2.2  ! ANZ-SEITEN EINGEFÜGT      ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5074.
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
              07 ZEILE                  pic X(168).
              07 ANZ-SEITEN             pic 9(6).
      *
      **************************************************************


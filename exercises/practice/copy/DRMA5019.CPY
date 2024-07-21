      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5019.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5019.CPv  $
      *
      *   Rev 2.4   Sep 05 2013 13:54:14   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.3   Aug 19 2013 12:14:06   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.2   Jul 16 2013 08:38:56   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.1   Jul 08 2013 10:57:16   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.0   Jul 04 2013 15:01:40   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5004   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5019
      *
      *     GENERIERUNG      :  05.09.2013/13:47:15
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!04.07.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!04.07.13!2.2  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!04.07.13!2.3  ! NEUERSTELLUNG             ! WOLFF
      * AMA00020!05.09.13!2.4  ! LOGBUCH AKTUALISIERT      ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5019.
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
           88 KST-SELECT                value 'SEL'.
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
              07 DATUM                  pic X(30).
              07 DATUM-ISO redefines DATUM
                                        pic X(10).
              07 DATUM-TIMESTAMP redefines DATUM
                                        pic X(26).
      *
      **************************************************************

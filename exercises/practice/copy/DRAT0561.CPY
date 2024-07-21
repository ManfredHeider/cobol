      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0561.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0561.CPv  $
      *
      *   Rev 2.3   Jul 23 2013 17:50:54   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 08 2013 16:14:16   t13ac18
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.1   Jun 28 2013 15:08:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ALLGEMEINER STATUS-SQL-BEREICH
      *
      *     SOURCE-NAME      :  DRAT0561
      *
      *     GENERIERUNG      :  23.07.2013/17:46:48
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AUFTRAG ! DATUM  ! AENDERUNG                     ! NAME
      * --------+--------+-------------------------------+-------------
      * AMA00018!26.06.13! NEUERSTELLUNG                 ! VON HELDEN
      * --------+--------+-------------------------------+-------------
      *         !        !                               !
      * --------+--------+-------------------------------+-------------
      *         !        !                               !
      * --------+--------+-------------------------------+-------------
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRAT0561.
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
      *
      **************************************************************

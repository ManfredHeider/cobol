      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0557.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0557.CPv  $
      *
      *   Rev 2.3   Jul 23 2013 17:50:20   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 08 2013 16:13:54   t13ac18
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.1   Jun 28 2013 15:07:48   p13sp20
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
      *     SOURCE-NAME      :  DRAT0557
      *
      *     GENERIERUNG      :  23.07.2013/17:39:53
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AUFTRAG ! DATUM  ! AENDERUNG                     ! NAME
      * --------+--------+-------------------------------+-------------
      * AMA00018!08.07.13! NEUERSTELLUNG                 ! VON HELDEN
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
           05 STATUS-SQL-BEREICH.
              07 STATUS-BEREICH.
                 09 PROGRAMMNAME        pic X(8).
                 09 RC                  pic 99.
                 09 MELDUNGS-BESCHREIBUNG.
                    11 MELDUNGS-NR      pic 9(8).
                    11 SUB-VAR-1        pic X(20).
                    11 SUB-VAR-2        pic X(20).
                 09 FEHL-BER-ALT        pic X(45).
                 09 FILLER              occurs 47
                                        pic X.
              07 SQL-BEREICH.
                 09 VIEWNAME            pic X(18).
                 09 OPERATION           pic XXX.
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
                 09 AUS-SQLCA.
                    11 SQLAID           pic X(8).
                    11 SQLABC           pic S9(9)      binary.
                    11 AUS-SQLCODE      pic S9(9)      binary.
                    11 SQLERRM.
                       13 SQLERRML      pic S9(4)      binary.
                       13 SQLERRMC      pic X(70).
                    11 SQLERRP          pic X(8).
                    11 SQLERRD          occurs 6
                                        pic S9(9)      binary.
                    11 SQLWARN.
                       13 SQLWARN0      pic X.
                       13 SQLWARN1      pic X.
                       13 SQLWARN2      pic X.
                       13 SQLWARN3      pic X.
                       13 SQLWARN4      pic X.
                       13 SQLWARN5      pic X.
                       13 SQLWARN6      pic X.
                       13 SQLWARN7      pic X.
                    11 SQLEXT.
                       13 SQLWARN8      pic X.
                       13 SQLWARN9      pic X.
                       13 SQLWARNA      pic X.
                       13 AUS-SQLSTATE  pic X(5).
                 09 FILLER              occurs 93
                                        pic X.
      *
      **************************************************************

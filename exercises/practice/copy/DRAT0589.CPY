      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0589.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAT0589.CPv  $
      *
      *   Rev 2.2   Jul 23 2013 17:51:12   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Jul 08 2013 16:13:12   t13ac18
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.0   Jul 08 2013 14:20:44   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ERGÄNZENDER SQL-BEREICH OZPS
      *
      *     SOURCE-NAME      :  DRAT0589
      *
      *     GENERIERUNG      :  23.07.2013/17:25:03
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AUFTRAG ! DATUM  ! AENDERUNG                     ! NAME
      * --------+--------+-------------------------------+-------------
      * AMA00011!08.07.13! NEUERSTELLUNG                 ! VON HELDEN/
      *         !        !                               ! D.WILHELM
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
           05 SQL-BEREICH.
              07 VIEWNAME               pic X(18).
              07 OPERATION              pic XXX.
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
              07 AUS-SQLCA.
                 09 SQLAID              pic X(8).
                 09 SQLABC              pic S9(9)      binary.
                 09 AUS-SQLCODE         pic S9(9)      binary.
                 09 SQLERRM.
                    11 SQLERRML         pic S9(4)      binary.
                    11 SQLERRMC         pic X(70).
                 09 SQLERRP             pic X(8).
                 09 SQLERRD             occurs 6
                                        pic S9(9)      binary.
                 09 SQLWARN.
                    11 SQLWARN0         pic X.
                    11 SQLWARN1         pic X.
                    11 SQLWARN2         pic X.
                    11 SQLWARN3         pic X.
                    11 SQLWARN4         pic X.
                    11 SQLWARN5         pic X.
                    11 SQLWARN6         pic X.
                    11 SQLWARN7         pic X.
                 09 SQLEXT.
                    11 SQLWARN8         pic X.
                    11 SQLWARN9         pic X.
                    11 SQLWARNA         pic X.
                    11 AUS-SQLSTATE     pic X(5).
              07 FILLER                 occurs 93
                                        pic X.
      *
      **************************************************************

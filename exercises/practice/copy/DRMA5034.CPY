      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5034.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA5034.CPv  $
      *
      *   Rev 2.5   Nov 14 2013 14:53:16   t13ac13
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.4   Nov 11 2013 11:16:18   t13ac13
      *AMA00018: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.3   Oct 17 2013 11:08:12   t13ac13
      *AMA00018: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.2   Oct 17 2013 07:47:06   t13ac13
      *AMA00018: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.1   Oct 09 2013 14:35:50   t13ac13
      *AMA00018: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.0   Aug 22 2013 14:01:50   t13ac13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: Ausgabe UPMA5011
      *
      *     SOURCE-NAME      :  DRMA5034
      *
      *     GENERIERUNG      :  22.08.2013/14:01:50
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00027!22.08.13!2.0  ! NEUERSTELLUNG             !Boost
      * AMA00027!09.10.13!2.1  ! ÄNDERUNG DAISY VB-Abrech  !Boost
      * AMA00027!17.10.13!2.2  ! ÄNDERUNG DAISY VB-Abrech  !Boost
      * AMA00027!17.10.13!2.3  ! ÄNDERUNG DAISY VB-Abrech  !Boost
      * AMA00027!12.11.13!2.4  ! ÄNDERUNG DAISY VB-Abrech  !Boost
      * AMA00027!14.11.13!2.5  ! ÄNDERUNG Logbuch          !Boost
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
           05 DRMA5034.
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
              07 TARIFBEZ               pic X(15).
      *
      **************************************************************

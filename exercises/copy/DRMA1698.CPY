      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1698.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRMA1698.CPv  $
      *
      *   Rev 2.7   May 18 2016 11:24:54   t13ac20
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.6   Oct 14 2013 14:16:32   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.5   Jul 23 2013 18:57:44   t13ac16
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.4   Jul 23 2013 15:37:08   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.3   Jul 22 2013 13:12:18   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.2   Jul 22 2013 10:35:10   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 10:23:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA1696 - SST-Upro Erst.
      *
      *     SOURCE-NAME      :  DRMA1698
      *
      *     GENERIERUNG      :  03.05.2016/12:11:54
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.3  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!        !2.4  ! ÄNDERUNG ??               !??
      * AMA00018!        !2.5  ! ÄNDERUNG ??               !??
      * AMA00018!01.10.13!2.6  ! DAISY ANPASSUNG           !WALBERT
      * AMA00270!03.05.16!2.7  ! BUCHTEXT AUFGENOMMEN      ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1698.
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
              07 BUCHDAT                pic X(10).
              07 BUCHTEXT               pic X(92).
      *
      **************************************************************


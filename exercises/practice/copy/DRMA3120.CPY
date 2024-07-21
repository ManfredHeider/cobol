      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3120.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA3120.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:16:30   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Jul 28 2015 16:02:26   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.2   Jul 23 2013 18:13:18   t13ac19
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 01 2013 11:40:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe-Bereich UPMA3116 - Listen-2
      *
      *     SOURCE-NAME      :  DRMA3120
      *
      *     GENERIERUNG      :  02.10.2017/13:07:42
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00129!06.07.15!2    ! ÄNDERUNG DRMA3114         ! D.WILHELM
      * AMA00360!20.09.17!3    ! ÄNDERUNG DRMA3114 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3120.
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
              07 BUCHDATWERTE.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 IND-JW              pic S9(4)      binary.
                 09 BUCH-DAT            pic X(10).
                 09 DISKONT-BETR-BRUTTO pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-BRUTTO
                                        pic S9(11)V99  packed-decimal.
                 09 DISKONT-BETR-NETTO  pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-NETTO pic S9(11)V99  packed-decimal.
                 09 DISKONT-BETR-UST    pic S9(11)V99  packed-decimal.
                 09 DISKONT-JBETR-UST   pic S9(11)V99  packed-decimal.
                 09 EINB-BETR           pic S9(11)V99  packed-decimal.
                 09 EINB-JBETR          pic S9(11)V99  packed-decimal.
                 09 DARLEHEN-BETR       pic S9(11)V99  packed-decimal.
                 09 DARLEHEN-JBETR      pic S9(11)V99  packed-decimal.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************


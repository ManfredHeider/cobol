      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3387.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA3387.CPv  $
      *
      *   Rev 2.1   Apr 29 2014 11:45:52   t13ac20
      *AMA00059: Badenia: Sourcen
      *
      *   Rev 2.0   Apr 22 2014 08:49:38   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA3385   POSTVERTEILUNGSSCHLÜSSEL
      *
      *     SOURCE-NAME      :  DRMA3387
      *
      *     GENERIERUNG      :  22.04.2014/13:16:10
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00059!22.04.14!2.1  ! NUR ÜBERNAHME AUS AACHEN  ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3387.
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
              07 VB-RC                  pic 99.
              07 VB-BETREUUNG.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 UE-BETREUER-VBNR    pic S9(9)      binary.
                 09 D-BETREUER-VBNR     pic S9(9)      binary.
                 09 DIR-BETREUER-VBNR   pic S9(9)      binary.
                 09 ED-BESITZER-VBNR    pic S9(9)      binary.
                 09 DIR-NR              pic S9(4)      binary.
              07 VB-STAMMDATEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 KTO-FKT             pic XX.
                 09 EROEFF-DAT          pic X(10).
                 09 SCHLIE-DAT          pic X(10).
                 09 EINR-DAT            pic X(10).
                 09 GEB-DAT             pic X(10).
                 09 DIR-NR              pic S9(4)      binary.
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 STELLEN-TYP         pic S9(4)      binary.
                 09 PRAXIS-STUFE        pic S9(4)      binary.
                 09 UMST-NR             pic X(15).
                 09 REGISTER-NR         pic X(30).
                 09 AM-AGENT-NR         pic S9(9)      binary.
                 09 VERRECHNKTO-KZ      pic X.
                 09 MA-ALLFI-STATUS     pic X.
                 09 MA-KEINE-ERF-PROV   pic X.
                 09 AVERM-KZN           pic X.
              07 VB-ADRESSEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 ANREDE              pic X.
                 09 ZUNAME              pic X(40).
                 09 VORNAME             pic X(20).
                 09 TITEL-AKAD          pic X(10).
                 09 TITEL-ADEL          pic X(10).
                 09 AUSL-KZ             pic X(4).
                 09 PLZ                 pic X(9).
                 09 ORT                 pic X(40).
                 09 STR                 pic X(40).
                 09 TELEFON-P           pic X(26).
                 09 FAX-NR-P            pic X(26).
                 09 TELEFON-B           pic X(26).
                 09 FAX-NR-B            pic X(26).
                 09 HANDY               pic X(26).
                 09 EMAIL               pic X(83).
                 09 URL                 pic X(83).
              07 DBE-RC                 pic 99.
              07 DBE-STAMMDATEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 KTO-FKT             pic XX.
                 09 EROEFF-DAT          pic X(10).
                 09 SCHLIE-DAT          pic X(10).
                 09 EINR-DAT            pic X(10).
                 09 GEB-DAT             pic X(10).
                 09 DIR-NR              pic S9(4)      binary.
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 STELLEN-TYP         pic S9(4)      binary.
                 09 PRAXIS-STUFE        pic S9(4)      binary.
                 09 UMST-NR             pic X(15).
                 09 REGISTER-NR         pic X(30).
                 09 AM-AGENT-NR         pic S9(9)      binary.
                 09 VERRECHNKTO-KZ      pic X.
                 09 MA-ALLFI-STATUS     pic X.
                 09 MA-KEINE-ERF-PROV   pic X.
                 09 AVERM-KZN           pic X.
              07 DBE-ADRESSEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 ANREDE              pic X.
                 09 ZUNAME              pic X(40).
                 09 VORNAME             pic X(20).
                 09 TITEL-AKAD          pic X(10).
                 09 TITEL-ADEL          pic X(10).
                 09 AUSL-KZ             pic X(4).
                 09 PLZ                 pic X(9).
                 09 ORT                 pic X(40).
                 09 STR                 pic X(40).
                 09 TELEFON-P           pic X(26).
                 09 FAX-NR-P            pic X(26).
                 09 TELEFON-B           pic X(26).
                 09 FAX-NR-B            pic X(26).
                 09 HANDY               pic X(26).
                 09 EMAIL               pic X(83).
                 09 URL                 pic X(83).
              07 DIRB-RC                pic 99.
              07 DIRB-STAMMDATEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 KTO-FKT             pic XX.
                 09 EROEFF-DAT          pic X(10).
                 09 SCHLIE-DAT          pic X(10).
                 09 EINR-DAT            pic X(10).
                 09 GEB-DAT             pic X(10).
                 09 DIR-NR              pic S9(4)      binary.
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 STELLEN-TYP         pic S9(4)      binary.
                 09 PRAXIS-STUFE        pic S9(4)      binary.
                 09 UMST-NR             pic X(15).
                 09 REGISTER-NR         pic X(30).
                 09 AM-AGENT-NR         pic S9(9)      binary.
                 09 VERRECHNKTO-KZ      pic X.
                 09 MA-ALLFI-STATUS     pic X.
                 09 MA-KEINE-ERF-PROV   pic X.
                 09 AVERM-KZN           pic X.
              07 DIRB-ADRESSEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 ANREDE              pic X.
                 09 ZUNAME              pic X(40).
                 09 VORNAME             pic X(20).
                 09 TITEL-AKAD          pic X(10).
                 09 TITEL-ADEL          pic X(10).
                 09 AUSL-KZ             pic X(4).
                 09 PLZ                 pic X(9).
                 09 ORT                 pic X(40).
                 09 STR                 pic X(40).
                 09 TELEFON-P           pic X(26).
                 09 FAX-NR-P            pic X(26).
                 09 TELEFON-B           pic X(26).
                 09 FAX-NR-B            pic X(26).
                 09 HANDY               pic X(26).
                 09 EMAIL               pic X(83).
                 09 URL                 pic X(83).
      *
      **************************************************************

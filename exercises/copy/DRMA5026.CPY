      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5026.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA5026.CPv  $
      *
      *   Rev 2.5   Oct 19 2017 14:24:24   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   Oct 16 2015 11:40:38   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:43:26   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Nov 14 2013 15:29:14   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.1   Oct 09 2013 14:17:16   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.0   Jul 16 2013 08:43:54   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     AUSGABE: UPMA5003   MA-DATEN ANREICHERN
      *
      *     SOURCE-NAME      :  DRMA5026
      *
      *     GENERIERUNG      :  02.10.2017/13:40:07
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!15.07.13!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00018!14.11.13!2.2  ! AKTUALISIEREN LOGBUCH     !VON HELDEN
      * AMA00128!05.05.15!2.3  ! ERGÄNZEN PR-PFL-ABR       ! WALBERT
      *         !01.07.15!     ! ERGÄNZEN PROVAUS-BRUTTO   !
      * AMA00128!28.09.15!2.4  ! PROVAUS-BRUTTO ENTFERNT   ! D.WILHELM
      *         !        !     ! PROV-AUS-RXXXX-UST NEU    !
      *         !        !     ! PROV-AUS-RXXXX     NEU    !
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG DEMA0445 EINBEH. ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0446 EINBEH. !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5026.
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
              07 VFM-KZN                pic X.
              07 BAV-KZN                pic X.
              07 PR-PFL-SU-ABR          pic S9(9)V99   packed-decimal.
              07 ERWERB-FOLGE-KZN       pic X(8).
              07 PROV-AUS-EINB-UST      pic S9(9)V99   packed-decimal.
              07 PROV-AUS-EINB          pic S9(9)V99   packed-decimal.
      *
      **************************************************************


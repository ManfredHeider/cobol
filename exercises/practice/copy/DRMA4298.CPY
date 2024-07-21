      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  24.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4298.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA4298.CPv  $
      *
      *   Rev 2.3   May 18 2016 11:47:18   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.2   Jul 24 2013 16:05:54   t13ac19
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.1   Jul 24 2013 15:33:48   p13sp14
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Jul 24 2013 15:32:28   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Ausgabe: UPMA4296   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA4298
      *
      *     GENERIERUNG      :  03.05.2016/15:45:58
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!06.04.16!2.3  ! neue Felder zur           !   Walbert
      *         !        !     ! Umsatzsteuer              !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4298.
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
              07 STORMIND-PROZ          pic S999V99    packed-decimal.
              07 AE-MONATS-DS-GRENZE    pic S9(4)V9    packed-decimal.
              07 FAKTOR                 pic S9V99      packed-decimal.
              07 KUERZUNG               pic S9(9)V99   packed-decimal.
              07 EQF-KUERZUNG-VON       pic S99V9      packed-decimal.
              07 EQF-KUERZUNG-BIS       pic S99V9      packed-decimal.
              07 BRUTTO-BETRAG-UST-FREI pic S9(9)V99   packed-decimal.
              07 AUSZAHLUNG-UST-FREI    pic S9(9)V99   packed-decimal.
              07 KUERZUNG-UST-FREI      pic S9(9)V99   packed-decimal.
              07 BRUTTO-BETRAG-UST-PFL  pic S9(9)V99   packed-decimal.
              07 AUSZAHLUNG-UST-PFL     pic S9(9)V99   packed-decimal.
              07 KUERZUNG-UST-PFL       pic S9(9)V99   packed-decimal.
      *
      **************************************************************


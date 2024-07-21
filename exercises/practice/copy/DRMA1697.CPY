      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1697.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRMA1697.CPv  $
      *
      *   Rev 2.6   Jul 28 2015 14:48:48   t13ac19
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.5   Oct 14 2013 14:16:16   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.4   Jul 23 2013 15:36:52   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.3   Jul 22 2013 13:12:02   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.2   Jul 22 2013 10:34:54   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 10:22:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich PMOV1696 - SST-Upro Erst.
      *
      *     SOURCE-NAME      :  DRMA1697
      *
      *     GENERIERUNG      :  13.07.2015/16:09:31
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.3  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!        !2.4  ! ??                        !??
      * AMA00018!01.10.13!2.5  ! DAISY ANPASSUNG           !WALBERT
      * AMA00128!13.07.15!2.6  ! Änderung DEMA0193         !O.Wilhelm
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1697.
              07 PROGRAMMNAME           pic X(8).
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
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 FACH-KENNUNG           pic X(10).
              07 VBNR                   pic S9(9)      binary.
              07 KTO-KZN-1              pic 999.
              07 BNGBETR-1              pic S9(9)V99   packed-decimal.
              07 BNGBETR-2              pic S9(9)V99   packed-decimal.
              07 ZUSATZ-BEREICH         pic X(350).
      *
      **************************************************************


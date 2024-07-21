      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5156.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5156.CPv  $
      *
      *   Rev 2.1   Dec 21 2016 15:24:18   t13ac18
      *AMA00296: USt: LUCY II - CR Erweiterung Deckblatt - Sourcen
      *
      *   Rev 2.0   Nov 01 2016 14:08:00   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA5046   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA5156
      *
      *     GENERIERUNG      :  25.11.2016/17:31:42
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00296!01.11.16!1    ! NEUERSTELLUNG             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5156.
              07 FUNKTION               pic X(8).
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
              07 VG                     pic 9(4).
              07 USERID                 pic X(8).
              07 ERSTELL-DAT            pic X(10).
              07 IND-MAX                pic S9(4)      binary.
              07 BUCHDAT-AKT            pic X(10).
              07 DRMA5158.
                 09 VBNR                pic 9(9).
                 09 KORREKTUR-ZIEL-STATUS
                                        pic X.
                 09 STEUERJAHR          pic X(4).
                 09 NETTOBETRAG         pic S9(9)V99   packed-decimal.
                 09 UST                 pic S9(9)V99   packed-decimal.
                 09 BUCHDAT             pic X(10).
              07 DGMA5011               occurs 10
                                        indexed by IND.
                 09 EIN-STEUERJAHR      pic X(4).
                 09 EIN-NETTOBETRAG     pic S9(9)V99   packed-decimal.
                 09 EIN-UST             pic S9(9)V99   packed-decimal.
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5135.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5135.CPv  $
      *
      *   Rev 2.2   Jul 13 2016 14:11:18   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.1   Apr 20 2016 10:02:20   t13ac20
      *AMA00217: USt: LUCY II - Vorarbeiten / Redesign - Copies
      *
      *   Rev 2.0   Mar 03 2016 13:55:48   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5026   FUNKTION V2
      *
      *     SOURCE-NAME      :  DRMA5135
      *
      *     GENERIERUNG      :  13.07.2016/13:43:09
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00217!03.03.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00292!13.07.16!2.2  ! ERWEITERUNG VERSION/VG    ! D.WILHELM
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5135.
              07 DGAT0102.
                 09 DGAT0100.
                    11 FUNKTION         pic X(8).
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
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 VBNR                   pic 9(9).
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5075.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5075.CPv  $
      *
      *   Rev 2.2   Jul 07 2016 16:03:12   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.1   Mar 20 2015 12:13:56   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.0   Mar 19 2015 17:21:46   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5026   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5075
      *
      *     GENERIERUNG      :  23.05.2016/16:09:14
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!20.03.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00292!23.05.16!2.2  ! ERWEITERUNG VERSION/VG    ! D.WILHELM
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5075.
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
              07 PG                     pic 99.
      *
      **************************************************************


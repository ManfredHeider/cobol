      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5153.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5153.CPv  $
      *
      *   Rev 2.1   Jul 07 2016 16:05:12   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.0   May 10 2016 11:27:22   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich ZXMA0212 - LISTEN-2
      *
      *     SOURCE-NAME      :  DRMA5153
      *
      *     GENERIERUNG      :  12.05.2016/14:46:39
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00292!10.05.16!2.1  ! Neuerstellung             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5153.
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
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 STICHTAG               pic X(10).
      *
      **************************************************************


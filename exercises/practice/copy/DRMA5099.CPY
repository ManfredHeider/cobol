      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5099.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5099.CPv  $
      *
      *   Rev 2.1   Jul 28 2015 15:45:18   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.0   Jun 09 2015 17:09:02   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich ZXPA0211 - LISTEN-2
      *
      *     SOURCE-NAME      :  DRMA5099
      *
      *     GENERIERUNG      :  09.06.2015/17:18:07
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!09.06.15!2.1  ! Neuerstellung             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5099.
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


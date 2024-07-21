      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5097.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5097.CPv  $
      *
      *   Rev 2.1   Jul 28 2015 15:44:48   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.0   Jun 03 2015 12:06:06   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich ZXMA0211 - LISTEN-1
      *
      *     SOURCE-NAME      :  DRMA5097
      *
      *     GENERIERUNG      :  09.06.2015/17:16:13
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!03.06.15!2.1  ! Neuerstellung             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5097.
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
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5077.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5077.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 15:42:16   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.1   Mar 19 2015 17:24:38   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.0   Mar 18 2015 16:41:42   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5027   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5077
      *
      *     GENERIERUNG      :  17.06.2015/15:54:48
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!18.03.15!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00128!08.06.15!2.2  ! ERWEITERUNG VERSION/VG    ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5077.
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
              07 PRODSCHL               pic 999.
      *
      **************************************************************


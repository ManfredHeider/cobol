      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.09.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5046.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5046.CPv  $
      *
      *   Rev 2.1   Sep 10 2014 10:35:46   t13ac20
      *AMA00078: Haftungszeitreduzierung Central / neuer PGV
      *
      *   Rev 2.0   Sep 09 2014 13:11:16   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5014   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5046
      *
      *     GENERIERUNG      :  09.09.2014/13:51:18
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00078!09.09.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5046.
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
           88 KST-SELECT                value 'SEL'.
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 SEL-WERT               pic X(15).
              07 HZRBEG-PG              pic X(10).
              07 HZREND-PG              pic X(10).
      *
      **************************************************************

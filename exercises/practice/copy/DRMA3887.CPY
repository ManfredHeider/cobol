      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3887.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3887.CPv  $
      *
      *   Rev 2.2   Sep 22 2015 15:51:32   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Copies/Programme
      *
      *   Rev 2.1   Jul 01 2013 17:19:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA3889   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA3887
      *
      *     GENERIERUNG      :  22.09.2015/15:23:20
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!22.09.15!2.2  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3887.
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
              07 VG                     pic 9(4).
              07 VBNR                   pic 9(7).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 SUMME-PROV-PFL-SE      pic S9(9)V99   packed-decimal.
              07 SUMME-PRO-RATA-SE      pic S9(9)V99   packed-decimal.
              07 SUMME-BETRAG-0         pic S9(9)V99   packed-decimal.
              07 SUMME-UST              pic S9(9)V99   packed-decimal.
              07 SUMME-BETRAG-1         pic S9(9)V99   packed-decimal.
              07 SUMME-PROVEINHEITEN    pic S9(10)V9   packed-decimal.
              07 BUCHDAT                pic X(10).
      *
      **************************************************************


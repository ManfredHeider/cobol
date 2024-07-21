      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1578.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1578.CPv  $
      *
      *   Rev 2.2   Jun 01 2015 15:19:40   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.1   Jul 01 2013 09:10:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE-BEREICH UPMA1577 - ERSTELLEN STEUERDATEIEN
      *
      *     SOURCE-NAME      :  DRMA1578
      *
      *     GENERIERUNG      :  01.06.2015/13:03:24
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!01.06.15!2.2  ! FACH-KENNUNG IN FACH-     ! WOLFF
      *         !        !     ! KENNUNG-1 UMBENANNT;      !
      *         !        !     ! FACH-KENNUNG-2 NEU;       !
      *         !        !     ! FILLER RAUSGENOMMEN       !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1578.
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
              07 FACH-KENNUNG-1         pic X(10).
              07 FACH-KENNUNG-2         pic X(10).
              07 VBNR                   pic S9(9)      binary.
              07 SEITENANZAHL           pic 9(7)       packed-decimal.
      *
      **************************************************************


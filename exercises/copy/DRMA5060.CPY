      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.02.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5060.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5060.CPv  $
      *
      *   Rev 2.3   Jun 12 2018 09:43:46   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.2   May 18 2016 11:47:54   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Feb 13 2015 15:26:38   t13ac19
      *AMA00147: Änderung Sonderleistungen 2015 - Copies
      *
      *   Rev 2.0   Feb 02 2015 17:01:18   t13ac19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5019   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5060
      *
      *     GENERIERUNG      :  11.06.2018/14:51:43
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00147!02.02.15!2.1  ! NEUERSTELLUNG             ! O.WILHELM
      * AMA00270!11.04.16!2.2  ! NEU: EH-AP-NT-EG-UST      ! WALBERT
      * AMA00400!08.06.18!2.3  ! ERWEITERUNG UM EQF-MONAT  ! D.WILHELM
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5060.
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
              07 VBNR                   pic 9(9).
              07 EQF                    pic S999V99    packed-decimal.
              07 EH-AP-NT-EG            occurs 3
                                        pic S9(10)V9   packed-decimal.
              07 EH-AP-NT-EG-UST        occurs 3
                                        pic S9(10)V9   packed-decimal.
              07 STRUK-STUFE            pic S9(4)      binary.
              07 STRUK-STUFE-TEXT       pic X(4).
              07 DIR-NR                 pic S9(4)      binary.
              07 BUCHDAT                pic 9(8).
              07 EQF-MONAT              pic S999V99    packed-decimal.
      *
      **************************************************************


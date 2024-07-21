      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.02.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5133.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA5133.CPv  $
      *
      *   Rev 2.3   Jun 12 2018 09:43:54   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.2   May 18 2016 11:48:18   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Apr 05 2016 15:55:20   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.0   Feb 29 2016 08:46:38   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5042   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5133
      *
      *     GENERIERUNG      :  08.06.2018/17:16:57
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00270!24.02.16!2.1  ! NEUERSTELLUNG             ! WALBERT
      *         !06.04.16!2.2  ! ERWEITERUNG UM EINH.-UST  !
      * AMA00400!08.06.18!2.3  ! ERWEITERUNG UM EQF-MONAT  ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5133.
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
              07 PRAXIS-STUFE           pic S9(4)      binary.
              07 VBNAME                 pic X(20).
              07 MA-CKV-WECHSEL         pic X.
              07 MA-CKV-ERFPROV         pic X.
              07 EQF-MONAT              pic S999V99    packed-decimal.
      *
      **************************************************************


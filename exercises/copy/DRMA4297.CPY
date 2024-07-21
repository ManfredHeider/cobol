      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4297.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA4297.CPv  $
      *
      *   Rev 2.4   Nov 15 2018 13:18:10   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Aus
      *bau Besonderheiten 08 und 09
      *
      *   Rev 2.3   Aug 15 2018 10:00:54   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Pro
      *gramme
      *
      *   Rev 2.2   May 18 2016 11:47:06   t13ac17
      *AMA00270: USt: LUCY II - KOMA0510-VB-Statistik - Copies
      *
      *   Rev 2.1   Jul 01 2013 17:26:48   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe: UPMA4296   Funktion V1
      *
      *     SOURCE-NAME      :  DRMA4297
      *
      *     GENERIERUNG      :  02.11.2018/10:18:46
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00270!06.04.16!2.2  ! Neue Felder: -UST-FREI    !   Walbert
      *         !        !     !              -UST-PFL     !
      * AMA00410!01.08.18!2.3  ! FELD AUSNAHME-ALAG        ! D.WILHELM
      *         !        !     ! AUFGENOMMEN               !
      * AMA00410!02.11.18!2.4  ! FELD AUSNAHME-ALAG        ! D.WILHELM
      *         !        !     ! ENTFERNT                  !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4297.
              07 FUNKTION               pic X(8).
              07 VG                     pic 9(4).
              07 KNR-EP-TYP             pic S9(4)      binary.
              07 AE-SUMME               pic S9(10)V9   packed-decimal.
              07 AE-MONATS-DS           pic S9(10)V9   packed-decimal.
              07 AE-SUMME-UST-FREI      pic S9(10)V9   packed-decimal.
              07 AE-MONATS-DS-UST-FREI  pic S9(10)V9   packed-decimal.
              07 AE-SUMME-UST-PFL       pic S9(10)V9   packed-decimal.
              07 AE-MONATS-DS-UST-PFL   pic S9(10)V9   packed-decimal.
              07 EQF                    pic S999V99    packed-decimal.
              07 WERTUNGSZEITRAUM       pic 9.
      *
      **************************************************************


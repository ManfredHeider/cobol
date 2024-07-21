      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4284.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA4284.CPv  $
      *
      *   Rev 2.4   Nov 15 2018 13:16:54   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Aus
      *bau Besonderheiten 08 und 09
      *
      *   Rev 2.3   Aug 15 2018 10:00:34   t13ac18
      *AMA00410: Ermittlung Existenzgründerbonus Allfinanz AG - Pro
      *gramme
      *
      *   Rev 2.2   Jun 12 2018 09:43:24   t13ac18
      *AMA00400: Generali Allfinanz AG - Copies DWH
      *
      *   Rev 2.1   Jul 01 2013 17:25:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Referenzbestand Sonderleistungen
      *
      *     SOURCE-NAME      :  DRMA4284
      *
      *     GENERIERUNG      :  02.11.2018/10:15:13
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00400!26.02.18!2.2  ! Änderung DGMA4295         ! D.WILHELM
      * AMA00410!01.08.18!2.3  ! Änderung DGMA4295         ! D.WILHELM
      * AMA00410!02.11.18!2.4  ! Änderung DGMA4295         ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4284.
              07 DGOV4295.
                 09 VBNR                pic 9(9).
                 09 KNR-EP-TYP          pic S9(4)      binary.
                 09 WERTUNGSZEITRAUM    pic 9.
                 09 WERTUNGSMONAT       pic 99.
      *
      **************************************************************


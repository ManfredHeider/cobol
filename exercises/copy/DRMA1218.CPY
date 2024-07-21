      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1218.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1218.CPv  $
      *
      *   Rev 2.2   Feb 15 2019 14:05:08   t13ae53
      *AMA00423: M_9000_2c - Prio A - Löschverfahren für VBs und Be
      *werber (MA-Abrechnung) - Programme TTH
      *
      *   Rev 2.1   Jun 28 2013 16:18:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1211 - DELETE
      *
      *     SOURCE-NAME      :  DRMA1218
      *
      *     GENERIERUNG      :  13.02.2019/14:19:06
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * --------------------------------------------------------------
      * AMA00423!14.01.19!2.2  ! GUELTIG-AB und            ! THURMANN
      *         !        !     ! AENDERUNGSYSZEIT gelöscht !
      * --------------------------------------------------------------
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1218.
              07 FUNKTION               pic X(8).
              07 VERSION                pic X.
              07 VBNR                   pic S9(9)      binary.
      *
      **************************************************************


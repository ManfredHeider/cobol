      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0739.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0739.CPv  $
      *
      *   Rev 2.2   Oct 14 2013 10:11:24   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:02:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSSATZ FÜR LISTE RÜCKST.KONTEN NACH UMBUCHUNG
      *
      *     SOURCE-NAME      :  DRMA0739
      *
      *     GENERIERUNG      :  17.09.2013/09:46:00
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!12.09.13!2.2  ! ÄNDERUNG ANS GEÄNDERTE    ! WOLFF
      *         !        !     ! DATENMODELL               !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0739.
              07 DIR-NR                 pic 9(4).
              07 HAFTUNGS-KZN           pic X.
              07 BUCHDAT                pic X(10).
              07 UEBERDECKUNG           pic S9(11)V99  packed-decimal.
              07 UNTERDECKUNG           pic S9(11)V99  packed-decimal.
      *
      **************************************************************

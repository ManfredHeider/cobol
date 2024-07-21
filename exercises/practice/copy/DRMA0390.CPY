      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0390.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0390.CPv  $
      *
      *   Rev 2.2   Oct 14 2013 10:10:30   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:01:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: NNVP JE VB
      *
      *     SOURCE-NAME      :  DRMA0390
      *
      *     GENERIERUNG      :  16.09.2013/17:54:56
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!16.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0390.
              07 VBNR                   pic 9(9).
              07 NVP                    pic S9(9)V99   packed-decimal.
      *
      **************************************************************

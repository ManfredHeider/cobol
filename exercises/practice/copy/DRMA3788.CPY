      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3788.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3788.CPv  $
      *
      *   Rev 2.2   Oct 11 2013 19:00:26   t13ac20
      *AMA00011: Kalypso: Teilprojekt 4: allgemeine Funktionen
      *
      *   Rev 2.1   Jul 01 2013 17:18:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA3787   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA3788
      *
      *     GENERIERUNG      :  06.09.2013/16:17:17
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!06.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL: GESKZ IN PG  !
      *         !        !     ! GEÄNDERT                  !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3788.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 AUSWAHL-FUNKTION       pic 99.
              07 PG                     pic 99.
              07 VERTRAGS-ID            pic X(15).
      *
      **************************************************************

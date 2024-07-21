      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2657.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA2657.CPv  $
      *
      *   Rev 2.2   Oct 14 2013 10:14:00   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jul 01 2013 11:35:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: FEHLERLISTE
      *
      *     SOURCE-NAME      :  DRMA2657
      *
      *     GENERIERUNG      :  17.09.2013/09:46:50
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!12.09.13!2.2  ! ÄNDERUNG DRMA2595         ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2657.
              07 VBNR                   pic 9(9).
              07 DRMA2595.
                 09 DGMA3824            occurs 8.
                    11 PG               pic 99.
                    11 PG-ALPHA redefines PG
                                        pic XX.
                    11 ANZ              pic 9(7).
                    11 HAFT             pic S9(15)V99  packed-decimal.
      *
      **************************************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0736.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA0736.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:10:46   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:11:04   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:02:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSSATZ FÜR LISTE EINBEH.-KTO. NACH UMBUCHUNG
      *
      *     SOURCE-NAME      :  DRMA0736
      *
      *     GENERIERUNG      :  02.10.2017/12:35:11
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!12.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0340 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0736.
              07 VBNR                   pic 9(9).
              07 NAME                   pic X(20).
              07 BUCHDAT                pic X(10).
              07 EINBBIS                pic S9(15)V99  packed-decimal.
              07 EINBUEBER              pic S9(15)V99  packed-decimal.
              07 UNTERDECK              pic S9(15)V99  packed-decimal.
              07 GESAMTBED              pic S9(15)V99  packed-decimal.
      *
      **************************************************************


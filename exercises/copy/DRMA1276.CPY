      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1276.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA1276.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:13:14   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:12:18   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:24:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSSATZ FÜR LISTEN FREISETZEN EINBEHALT
      *
      *     SOURCE-NAME      :  DRMA1276
      *
      *     GENERIERUNG      :  02.10.2017/12:47:01
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!10.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0340 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1276.
              07 VBNR                   pic 9(9).
              07 NAME                   pic X(20).
              07 BUCHDAT                pic X(10).
              07 EINBVOL                pic S9(15)V99  packed-decimal.
              07 KTOSTAND               pic S9(15)V99  packed-decimal.
              07 EINBBIS1               pic S9(15)V99  packed-decimal.
              07 EINBBIS2               pic S9(15)V99  packed-decimal.
              07 EINBBIS3               pic S9(15)V99  packed-decimal.
              07 EINBBIS4               pic S9(15)V99  packed-decimal.
              07 EINBUEBER              pic S9(15)V99  packed-decimal.
              07 KEINEAUSZ              pic S9(15)V99  packed-decimal.
              07 EINBBIQ1               pic S9(15)V99  packed-decimal.
              07 EINBBIQ2               pic S9(15)V99  packed-decimal.
              07 EINBBIQ3               pic S9(15)V99  packed-decimal.
              07 EINBBIQ4               pic S9(15)V99  packed-decimal.
      *
      **************************************************************


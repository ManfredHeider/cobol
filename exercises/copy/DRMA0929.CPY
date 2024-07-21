      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0929.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA0929.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:11:50   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:11:42   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:09:34   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSSATZ FÜR LISTE EINB.KONTEN NACH UMBUCHUNG
      *
      *     SOURCE-NAME      :  DRMA0929
      *
      *     GENERIERUNG      :  06.10.2017/11:48:07
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!16.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0160 EINBEH. ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0164 EINBEH. !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0929.
              07 VBNR                   pic 9(9).
              07 DIR-NR-T.
                 09 DIR-NR              pic 9(4).
                 09 TAETIG              pic X.
              07 BUCHDAT                pic X(10).
              07 STRUK-STUFE-TEXT       pic X(4).
              07 VB-NAME                pic X(40).
              07 ABR-JAHR               pic 9(4).
              07 NVP                    pic S9(9)V99   packed-decimal.
              07 PR-SOLL-EINB           pic S9(9)V99   packed-decimal.
              07 EINB-SATZ              pic S999V99    packed-decimal.
              07 KTO-STAND-R            pic S9(9)V99   packed-decimal.
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2647.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2647.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:14:40   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:13:42   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jul 01 2013 11:34:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: SAMMELLISTE/ANSCHREIBEN
      *
      *     SOURCE-NAME      :  DRMA2647
      *
      *     GENERIERUNG      :  02.10.2017/12:53:38
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!12.09.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL;              !
      *         !        !     ! ÄNDERUNG DRMA2595         !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0164 EINBEH. ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0340 EINBEH. !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2647.
              07 VBNR                   pic 9(9).
              07 SCHLIE-DAT             pic X(10).
              07 ANREDE                 pic X.
              07 ZUNAME                 pic X(40).
              07 VORNAME                pic X(20).
              07 TITEL-AKAD             pic X(10).
              07 TITEL-ADEL             pic X(10).
              07 AUSL-KZ                pic X(4).
              07 PLZ                    pic X(9).
              07 ORT                    pic X(40).
              07 STR                    pic X(40).
              07 STRUK-STUFE            pic 9(4).
              07 DIR-NR                 pic 9(4).
              07 EINB-SATZ              pic S999V99    packed-decimal.
              07 KTO-STAND-R            pic S9(9)V99   packed-decimal.
              07 DRMA2595.
                 09 DGMA3824            occurs 8.
                    11 PG               pic 99.
                    11 PG-ALPHA redefines PG
                                        pic XX.
                    11 ANZ              pic 9(7).
                    11 HAFT             pic S9(15)V99  packed-decimal.
              07 HAFTVOL                pic S9(15)V99  packed-decimal.
              07 EINBVOL                pic S9(15)V99  packed-decimal.
              07 DECKUNG                pic S9(11)V99  packed-decimal.
              07 HAFTUNGS-KZN           pic X.
              07 BUCHDAT                pic X(10).
      *
      **************************************************************


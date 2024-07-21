      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5022.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5022.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:23:40   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Oct 14 2013 10:19:16   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.0   Jul 10 2013 16:29:26   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA3705   FUNKTION V2
      *
      *     SOURCE-NAME      :  DRMA5022
      *
      *     GENERIERUNG      :  02.10.2017/13:37:02
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00020!10.07.13!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DEMA0160 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5022.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 VBNR                   pic 9(9).
              07 PROVAUS                pic S9(9)V99   packed-decimal.
              07 PROVEINB               pic S9(9)V99   packed-decimal.
              07 STICHTAG               pic X(10).
      *
      **************************************************************


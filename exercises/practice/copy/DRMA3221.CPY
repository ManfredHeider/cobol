      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3221.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3221.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:18:06   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 11:43:06   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Fortschreibungsdatei AE-Statistik neu
      *
      *     SOURCE-NAME      :  DRMA3221
      *
      *     GENERIERUNG      :  02.10.2017/13:14:23
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! HELD-AS 07 ERSETZT DURCH  ! WARZ
      *         !        !     ! HELD-AS 01                !
      *         !        !     ! ÄNDERUNG DEMA0128         !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3221.
              07 GESKZ                  pic 99.
              07 PRODSCHL               pic 999.
              07 PRODART-SICHT          pic XX.
              07 JAHR                   pic 9(4).
              07 MONAT                  pic 99.
              07 SUMME1                 pic S9(10)V9   packed-decimal.
              07 SUMME2                 pic S9(10)V9   packed-decimal.
              07 SUMME3                 pic S9(10)V9   packed-decimal.
              07 SUMME4                 pic S9(10)V9   packed-decimal.
              07 SUMME5                 pic S9(10)V9   packed-decimal.
              07 SUMME6                 pic S9(10)V9   packed-decimal.
              07 SUMME7                 pic S9(10)V9   packed-decimal.
              07 SUMME8                 pic S9(10)V9   packed-decimal.
              07 GESAMT                 pic S9(10)V9   packed-decimal.
      *
      **************************************************************


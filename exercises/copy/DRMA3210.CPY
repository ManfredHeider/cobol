      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3210.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA3210.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:17:54   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Jul 28 2015 14:35:06   t13ac13
      *AMA00149: USt: KOMA0507-Statistiken - Teil 1 - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:42:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Fortschreibung Übers. abger. Prov. akt. Jahr
      *
      *     SOURCE-NAME      :  DRMA3210
      *
      *     GENERIERUNG      :  05.10.2017/11:00:52
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00149!26.06.15!2.2  ! Änderungen wegen LUCY     ! M.BOOST
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DRMA3049 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3210.
              07 BUCHDAT                pic X(10).
              07 GESKZ                  pic 99.
              07 PRODSCHL               pic 999.
              07 KENNZEICHEN            pic X.
              07 PROVAUSZAHLUNG         pic S9(11)V99  packed-decimal.
              07 PROVEINBEHALT          pic S9(11)V99  packed-decimal.
              07 VTF                    pic S9(11)V99  packed-decimal.
              07 VWF                    pic S9(11)V99  packed-decimal.
              07 PROVGESAMT             pic S9(11)V99  packed-decimal.
              07 AE                     pic S9(11)V99  packed-decimal.
              07 PROVAUS-UST            pic S9(11)V99  packed-decimal.
              07 VTF-UST                pic S9(11)V99  packed-decimal.
              07 PROVGESAMT-UST         pic S9(11)V99  packed-decimal.
      *
      **************************************************************


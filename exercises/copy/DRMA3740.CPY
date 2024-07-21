      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3740.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3740.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:21:10   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 17:18:10   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Management-Kennzahlen je Direktion
      *
      *     SOURCE-NAME      :  DRMA3740
      *
      *     GENERIERUNG      :  02.10.2017/13:27:06
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3740.
              07 VBNR                   pic 9(9).
              07 BUCHDAT                pic 9(8).
              07 DIR-NR                 pic 9(4).
              07 GESKZ                  pic 99.
              07 PRODSCHL               pic 999.
              07 AUSZAHLUNG-E-D         pic S9(11)V99  packed-decimal.
              07 EINBEHALT-E-D          pic S9(11)V99  packed-decimal.
              07 VTF-E-D                pic S9(11)V99  packed-decimal.
              07 VWF-E-D                pic S9(11)V99  packed-decimal.
              07 AUSZAHLUNG-F-D         pic S9(11)V99  packed-decimal.
              07 EINBEHALT-F-D          pic S9(11)V99  packed-decimal.
              07 VTF-F-D                pic S9(11)V99  packed-decimal.
              07 VWF-F-D                pic S9(11)V99  packed-decimal.
              07 AUSZAHLUNG-E-A         pic S9(11)V99  packed-decimal.
              07 EINBEHALT-E-A          pic S9(11)V99  packed-decimal.
              07 VTF-E-A                pic S9(11)V99  packed-decimal.
              07 VWF-E-A                pic S9(11)V99  packed-decimal.
              07 AUSZAHLUNG-F-A         pic S9(11)V99  packed-decimal.
              07 EINBEHALT-F-A          pic S9(11)V99  packed-decimal.
              07 VTF-F-A                pic S9(11)V99  packed-decimal.
              07 VWF-F-A                pic S9(11)V99  packed-decimal.
              07 STRUK-STUFE-TEXT       pic X(4).
              07 VERTRAGS-ID            pic X(15).
              07 TARIFKZ                pic 99.
      *
      **************************************************************


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2703.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA2703.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:15:24   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Aug 04 2015 16:22:10   t13ac13
      *AMA00149: USt: KOMA0507-Statistiken - Teil 1 - Copies
      *
      *   Rev 2.2   Jul 28 2015 14:35:22   t13ac13
      *AMA00149: USt: KOMA0507-Statistiken - Teil 1 - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:37:14   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Übers. abger. Provisionen akt. Monat Herr Frei
      *
      *     SOURCE-NAME      :  DRMA2703
      *
      *     GENERIERUNG      :  02.10.2017/12:54:20
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00149!08.07.15!2.2  ! UST-Felder eingefügt      ! M.BOOST
      * AMA00149!04.08.15!2.3  ! neu generiert wg. Ausprg. ! M.BOOST
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2703.
              07 BUCHDAT                pic X(10).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 KENNZEICHEN            pic X.
              07 PROVAUS-ERWERB         pic S9(11)V99  packed-decimal.
              07 PROVAUS-ERWERB-UST     pic S9(11)V99  packed-decimal.
              07 PROVAUS-FOLGE          pic S9(11)V99  packed-decimal.
              07 PROVAUS-FOLGE-UST      pic S9(11)V99  packed-decimal.
              07 PROVEINB-ERWERB        pic S9(11)V99  packed-decimal.
              07 PROVEINB-FOLGE         pic S9(11)V99  packed-decimal.
              07 VTF-ERWERB             pic S9(11)V99  packed-decimal.
              07 VTF-ERWERB-UST         pic S9(11)V99  packed-decimal.
              07 VTF-FOLGE              pic S9(11)V99  packed-decimal.
              07 VTF-FOLGE-UST          pic S9(11)V99  packed-decimal.
              07 VWF-ERWERB             pic S9(11)V99  packed-decimal.
              07 VWF-FOLGE              pic S9(11)V99  packed-decimal.
              07 PROVGESAMT             pic S9(11)V99  packed-decimal.
              07 PROVGESAMT-UST         pic S9(11)V99  packed-decimal.
              07 PROV-ERWERB            pic S9(11)V99  packed-decimal.
              07 PROV-ERWERB-UST        pic S9(11)V99  packed-decimal.
              07 PROV-FOLGE             pic S9(11)V99  packed-decimal.
              07 PROV-FOLGE-UST         pic S9(11)V99  packed-decimal.
              07 AE                     pic S9(11)V99  packed-decimal.
              07 AE-ERWERB              pic S9(11)V99  packed-decimal.
              07 AE-FOLGE               pic S9(11)V99  packed-decimal.
      *
      **************************************************************


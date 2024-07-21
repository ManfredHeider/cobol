      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1107.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1107.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 15:59:00   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:12:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVKONTEN00
      *
      *     SOURCE-NAME      :  DRMA1107
      *
      *     GENERIERUNG      :  29.06.2015/10:41:08
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00129!29.06.15!2.2  ! Änderung wg. Lucy         ! D.WILHELM
      *         !        !     ! ABR-NR-KTO-H              !
      *         !        !     ! ABR-NR-KTO-D              !
      *         !        !     ! ABR-NR-KTO-A              !
      *         !        !     ! von S9(4) bin auf X(15)   !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 KONTEN.
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 VBNR                   pic S9(9)      binary.
              07 HIST-KZN               pic X.
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 ABR-NR-KTO-H           pic X(15).
              07 ABR-NR-KTO-D           pic X(15).
              07 ABR-NR-KTO-A           pic X(15).
              07 LAST-BUCH-DAT-ABR      pic X(10).
              07 LAST-BUCH-DAT-PROV     pic X(10).
              07 NUR-SALDOBUCH-KZN      pic X.
              07 KTO-STAND-H            pic S9(9)V99   packed-decimal.
              07 BUCH-DAT-H             pic X(10).
              07 KTO-STAND-R            pic S9(9)V99   packed-decimal.
              07 BUCH-DAT-R             pic X(10).
              07 KTO-STAND-D            pic S9(9)V99   packed-decimal.
              07 BUCH-DAT-D             pic X(10).
              07 KTO-STAND-A            pic S9(9)V99   packed-decimal.
              07 BUCH-DAT-A             pic X(10).
              07 UEB-BETRAG             pic S9(9)V99   packed-decimal.
              07 UEB-DATUM              pic X(10).
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************


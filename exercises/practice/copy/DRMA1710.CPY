      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1710.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1710.CPv  $
      *
      *   Rev 2.2   Jul 28 2015 16:00:46   t13ac18
      *AMA00129: USt: KOMA0508-Datenbasis und Zugriffe - Copies
      *
      *   Rev 2.1   Jul 01 2013 10:24:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVREF_SALD_BUCH00
      *
      *     SOURCE-NAME      :  DRMA1710
      *
      *     GENERIERUNG      :  26.06.2015/14:12:03
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15! 2.1 ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * --------!--------!-----!---------------------------!----------
      * AMA00129!24.06.15! 2.2 ! Änderungen wg. Lucy       ! D.WILHELM
      * --------!--------!-----!---------------------------!----------
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 REF-SALD-BUCH.
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 FACH-KENNUNG           pic X(10).
              07 DV-KENNUNG             pic X(10).
              07 LFDNR                  pic S9(9)      packed-decimal.
              07 BUCHTEXT               pic X(92).
              07 BUCHTEXT-FIBU          pic X(40).
              07 KTO-KZN-1              pic S9         packed-decimal.
              07 KTO-KZN-2              pic X.
              07 FIBU-KONTO             pic S9(7)      packed-decimal.
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************


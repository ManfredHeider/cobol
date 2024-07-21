      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0821.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA0821.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:11:06   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Jul 28 2015 14:40:08   t13ac17
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:05:04   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Kontrollwerte je Produktgeber
      *
      *     SOURCE-NAME      :  DRMA0821
      *
      *     GENERIERUNG      :  02.10.2017/12:37:42
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!11.05.15!2.2  ! Übernahme aus Migration   ! Walbert
      *         !        !     ! Integration UST-Feld      !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DEMA0160 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0821.
              07 PG                     pic 99.
              07 BUCHDAT                pic X(10).
              07 PROVBRUTTO             pic S9(11)V99  packed-decimal.
              07 PROVBRUTTO-VWF         pic S9(11)V99  packed-decimal.
              07 PROVBRUTTO-VTF         pic S9(11)V99  packed-decimal.
              07 PROVBRUTTO-EDV         pic S9(11)V99  packed-decimal.
              07 PROVAUS-VB             pic S9(9)V99   packed-decimal.
              07 PROVEINB-VB            pic S9(9)V99   packed-decimal.
              07 PROVAUS-UST-VB         pic S9(9)V99   packed-decimal.
              07 PROVAUS-UST-GESAMT     pic S9(9)V99   packed-decimal.
      *
      **************************************************************


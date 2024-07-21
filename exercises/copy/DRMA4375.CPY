      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4375.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA4375.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:22:04   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 17:30:44   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: Ausgabesatz Anschreiben  DVAG-Wien
      *
      *     SOURCE-NAME      :  DRMA4375
      *
      *     GENERIERUNG      :  02.10.2017/13:30:50
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG DEMA0164 EINBEH. ! WARZ
      *         !        !     ! ÄNDERUNG DEMA0340 EINBEH. !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4375.
              07 VBNR                   pic S9(9)      binary.
              07 DIR-NR                 pic S9(4)      binary.
              07 STICHTAG               pic X(10).
              07 EINB-SATZ              pic S999V99    packed-decimal.
              07 KTO-STAND-R            pic S9(9)V99   packed-decimal.
              07 HAFTVOL                pic S9(15)V99  packed-decimal.
              07 EINBVOL                pic S9(15)V99  packed-decimal.
              07 DECKUNG                pic S9(11)V99  packed-decimal.
      *
      **************************************************************


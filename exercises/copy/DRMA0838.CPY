      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0838.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0838.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:11:38   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jun 28 2013 16:07:34   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz für DB2-Table KTO (DVAG)
      *
      *     SOURCE-NAME      :  DRMA0838
      *
      *     GENERIERUNG      :  12.10.2017/13:44:14
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! STAND-EINB bei DEMA0282   ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 KTO-SATZ.
              07 ST-NR                  pic S9(9)      binary.
              07 MONAT                  pic X(10).
              07 KNR-WAEHRUNG           pic S9(4)      binary.
              07 KNR-KTO-STAND          pic S9(4)      binary.
              07 KTY-WAEHRUNG           pic S9(4)      binary.
              07 KTY-KTO-STAND          pic S9(4)      binary.
              07 STAND-DISK             pic S9(9)V99   packed-decimal.
              07 STAND-VORSCH           pic S9(9)V99   packed-decimal.
              07 STAND-EINB             pic S9(9)V99   packed-decimal.
      *
      **************************************************************


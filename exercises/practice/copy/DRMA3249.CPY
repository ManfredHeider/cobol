      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3249.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3249.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:18:48   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 11:45:18   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Schnittstelle "nicht verdiente Prov." für DVAG
      *
      *     SOURCE-NAME      :  DRMA3249
      *
      *     GENERIERUNG      :  02.10.2017/13:17:23
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG DEMA0160 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3249.
              07 MONAT                  pic X(6).
              07 VB-NR                  pic S9(9)      binary.
              07 PRODUKTGEBER-NR        pic S9(4)      binary.
              07 DIR-NR                 pic S9(4)      binary.
              07 TAETIG-KZ              pic X.
              07 PROV-NNV-GES           pic S9(9)V99   packed-decimal.
              07 PROV-EB-SATZ           pic S9(9)V99   packed-decimal.
              07 RS-KTO-STAND           pic S9(9)V99   packed-decimal.
              07 DIFF-RS-KTO-PROV-RS    pic S9(9)V99   packed-decimal.
      *
      **************************************************************


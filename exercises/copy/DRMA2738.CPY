      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2738.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA2738.CPv  $
      *
      *   Rev 2.2   Nov 17 2020 10:55:00   t13ac18
      *AMA00486: Layoutänderung SPNP 
      *
      *   Rev 2.1   Jul 01 2013 11:37:24   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Druckdaten SPNP test
      *
      *     SOURCE-NAME      :  DRMA2738
      *
      *     GENERIERUNG      :  30.10.2020/12:19:07
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00486!29.10.20!2    ! Aufnahme RAP-Felder       ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2738.
              07 GESKZ                  pic 999.
              07 GESBEZ                 pic X(40).
              07 ZUGANG                 pic S9(11)V99  packed-decimal.
              07 FOLGE                  pic S9(11)V99  packed-decimal.
              07 ABGANG                 pic S9(11)V99  packed-decimal.
              07 NETTO                  pic S9(11)V99  packed-decimal.
              07 RAP                    pic S9(11)V99  packed-decimal.
              07 RAPZEHNTEL             pic S9(11)V99  packed-decimal.
              07 RAPDYN                 pic S9(11)V99  packed-decimal.
              07 RAPDYNZEHNTEL          pic S9(11)V99  packed-decimal.
              07 NETTO-ANTEIL           pic S999V99    packed-decimal.
              07 PROVEINH               pic S9(9)V9    packed-decimal.
              07 PROVEINH-ANTEIL        pic S999V99    packed-decimal.
              07 STCK-ZUG               pic S9(7)      packed-decimal.
              07 EQF                    pic S999V99    packed-decimal.
      *
      **************************************************************

      *
      **************************************************************


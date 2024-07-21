      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2764.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA2764.CPv  $
      *
      *   Rev 2.2   Nov 17 2020 10:55:22   t13ac18
      *AMA00486: Layoutänderung SPNP 
      *
      *   Rev 2.1   Jul 01 2013 11:38:00   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     SPNP -> AE-Stat-Daten für Gesellschaft 20
      *
      *     SOURCE-NAME      :  DRMA2764
      *
      *     GENERIERUNG      :  30.10.2020/12:20:52
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00486!29.10.20!2    ! Aufnahme GESKZ            ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2764.
              07 AE-ZUG                 pic S9(11)V99  packed-decimal.
              07 AE-ABG                 pic S9(11)V99  packed-decimal.
              07 AE-AUFH-ZUG            pic S9(11)V99  packed-decimal.
              07 AE-AUFH-ABG            pic S9(11)V99  packed-decimal.
              07 AE-INS-NETTO-1         pic S9(11)V99  packed-decimal.
              07 AE-INS-NETTO-2         pic S9(11)V99  packed-decimal.
              07 AE-EING-GESCH-1-4      pic S9(9)V9    packed-decimal.
              07 AE-EING-GESCH-5-8      pic S9(9)V9    packed-decimal.
              07 AE-EQF                 pic S999V99    packed-decimal.
              07 GESKZ                  pic 999.
      *
      **************************************************************

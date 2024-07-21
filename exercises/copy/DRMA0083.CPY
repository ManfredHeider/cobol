      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0083.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0083.CPv  $
      *
      *   Rev 2.2   Dec 17 2013 10:27:30   t13ac20
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.1   Jun 28 2013 16:00:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     BESTAND VBNRN MIT RÜCKBELASTUNGEN IM AKT JAHR DVAG
      *
      *     SOURCE-NAME      :  DRMA0083
      *
      *     GENERIERUNG      :  17.12.2013/09:22:39
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!17.12.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0083.
              07 VBNR                   pic 9(9).
              07 PROV-RUECKBELASTUNG    pic S9(9)V99   packed-decimal.
              07 BUCHDAT                pic X(10).
              07 STRUK-STUFE            pic XX.
              07 ERFPROV-RUECKBELASTUNG pic S9(9)V99   packed-decimal.
      *
      **************************************************************

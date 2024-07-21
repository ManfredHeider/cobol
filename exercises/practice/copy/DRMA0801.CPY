      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0801.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA0801.CPv  $
      *
      *   Rev 2.5   Jul 28 2015 14:48:32   t13ac19
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.4   Oct 14 2013 14:15:58   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.3   Jul 23 2013 15:36:34   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.2   Jul 22 2013 09:27:00   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jun 28 2013 16:04:20   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Einzelnachweis der Saldenveränderung aus SST - neu
      *
      *     SOURCE-NAME      :  DRMA0801
      *
      *     GENERIERUNG      :  29.06.2015/15:31:34
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!24.09.13!2.2  ! ÜBERNAHME AUS MIGRATION   !WALBERT
      * AMA00018!        !2.3  ! ÄNDERUNG ??               !??
      * AMA00018!01.10.13!2.4  ! DAISY ANPASSUNG           !WALBERT
      * AMA00128!29.06.15!2.5  ! Änderung DGMA0108         ! O.Wilhelm
      *         !        !     ! IND-JW gelöscht           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0801.
              07 SALDENDATEN.
                 09 VBNR                pic 9(9).
                 09 FACH-KENNUNG        pic X(10).
                 09 DV-KENNUNG          pic X(10).
                 09 BUCHDAT             pic X(10).
                 09 KTO-KZN-1           pic 9.
                 09 BNGBETR-1           pic S9(9)V99   packed-decimal.
                 09 KTO-KZN-2           pic X.
                 09 BNGBETR-2           pic S9(9)V99   packed-decimal.
      *
      **************************************************************


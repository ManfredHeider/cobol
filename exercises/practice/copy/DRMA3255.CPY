      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3255.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3255.CPv  $
      *
      *   Rev 2.2   Jun 14 2016 09:38:16   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:45:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabesatz Aktualisieren Jahreswerte DIM7/EP
      *
      *     SOURCE-NAME      :  DRMA3255
      *
      *     GENERIERUNG      :  13.06.2016/12:17:15
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00263!13.06.16!2.2  ! NEUE FELDER FÜR NETTO UND ! WOLFF
      *         !        !     ! UST                       !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3255.
              07 VBNR                   pic 9(7).
              07 FILLER                 pic X.
              07 IND-JW                 pic 9(5).
              07 FILLER                 pic X.
              07 VORZ-BRUTTO            pic X.
              07 BETR-BRUTTO            pic 9(13)V99.
              07 FILLER                 pic X.
              07 VORZ-NETTO             pic X.
              07 BETR-NETTO             pic 9(13)V99.
              07 FILLER                 pic X.
              07 VORZ-UST               pic X.
              07 BETR-UST               pic 9(13)V99.
              07 FILLER                 pic X.
              07 BEZEICHNUNG            pic X(80).
      *
      **************************************************************


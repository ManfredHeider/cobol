      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1977.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1977.CPv  $
      *
      *   Rev 2.2   Jan 10 2014 08:11:12   t13ac13
      *AMA00027: Anpassung Daisy - VB-Abrechnung: Programme
      *
      *   Rev 2.2   Jan 09 2013 15:45:00   t13ac13
      *AMA00027: BEARBTERM jetzt DATUM
      *
      *   Rev 2.1   Jul 01 2013 10:39:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      **************************************************************
      *
      *     Stornoneutralstellung unaufbereitet
      *
      *     SOURCE-NAME      :  DRMA1977
      *
      *     GENERIERUNG      :  09.01.2014/15:39:34
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      *         !26.04.13!2.0  ! Archiverstellung          !P13SP20
      * AMA00011!01.07.13!2.1  ! Kalypso: Teilprojekt 4    !P13SP20
      * AMA00027!09.01.14!2.2  ! BEARBTERM jetzt DATUM     !Boost
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1977.
              07 VBNR                   pic 9(9).
              07 FACH-KENNUNG           pic X(10).
              07 BUCHDAT-STORNO         pic X(10).
              07 GESKZ                  pic 99.
              07 VERTRAGS-ID            pic X(15).
              07 VN                     pic X(20).
              07 BEWART                 pic 9(6).
              07 SONDERBER              pic X(02).
              07 AE-STORNO              pic S9(10)V9   packed-decimal.
              07 BEARBTERM-STORNO       pic X(10).
              07 BUCHDAT-AUFHEBUNG      pic X(10).
              07 AE-AUFHEBUNG           pic S9(10)V9   packed-decimal.
              07 BEARBTERM-AUFHEBUNG    pic X(10).
      *
      **************************************************************

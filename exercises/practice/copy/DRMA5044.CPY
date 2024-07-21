      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.07.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5044.CPv  $
      *
      *   Rev 2.1   Sep 17 2014 13:09:16   t13ac17
      *AMA00060: Rückstellungspflicht eines Produktes prüfen 
      *
      *   Rev 2.0   Jul 30 2014 12:20:08   t13ac17
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5013 FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5044
      *
      *     GENERIERUNG      :  30.07.2014/12:16:02
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00060!30.07.14!2.1  ! NEUERSTELLUNG             ! WALBERT
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5044.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 HAFTZEIT-KZN           pic XX.
      *
      **************************************************************

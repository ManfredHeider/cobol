      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  16.01.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5042.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5042.CPv  $
      *
      *   Rev 2.1   Jan 20 2014 13:25:22   t13ac20
      *AMA00018: temporär Wolff
      *
      *   Rev 2.0   Jan 16 2014 15:30:18   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5006   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5042
      *
      *     GENERIERUNG      :  16.01.2014/16:19:58
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!16.01.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5042.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 KONTROLLWERTE-1.
                 09 INHALT              pic X(50).
                 09 SEMI1               pic X.
                 09 WERT1               pic -(9)9,99.
                 09 SEMI2               pic X.
                 09 WERT2               pic -(9)9,99.
                 09 SEMI3               pic X.
                 09 FILLER              pic X.
              07 KONTROLLWERTE-2.
                 09 INHALT              pic X(50).
                 09 SEMI1               pic X.
                 09 WERT1               pic -(9)9,99.
                 09 SEMI2               pic X.
                 09 WERT2               pic -(9)9,99.
                 09 SEMI3               pic X.
                 09 FILLER              pic X.
      *
      **************************************************************

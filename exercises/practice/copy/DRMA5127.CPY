      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5127.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5127.CPv  $
      *
      *   Rev 2.1   Jan 19 2016 12:04:18   t13ac14
      *AMA00218: USt: LUCY I - Nacharbeiten aus AMA00200
      *
      *   Rev 2.0   Dec 15 2015 17:29:40   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5040   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5127
      *
      *     GENERIERUNG      :  16.12.2015/12:21:35
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00218!15.12.15!2.1  ! NEUERSTELLUNG             !VON HELDEN
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5127.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 DGMA5005               occurs 20.
                 09 PMPT-ID             pic 9(15).
                 09 ST-NR               pic 9(7).
                 09 PDPT-ID             pic 9(15).
                 09 FIRST-INSERT        pic X(26).
                 09 KORREKTUR-ZIEL-STATUS
                                        pic X.
      *
      **************************************************************


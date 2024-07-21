      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA4576.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA4576.CPv  $
      *
      *   Rev 2.1   Mar 12 2014 13:44:04   t13ac20
      *AMA00044: PDF-Generierung: übernommene Member aus Aachen
      *
      *   Rev 2.0   Nov 26 2013 16:11:48   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: TLE STRUCTURED FIELD IN EINEM AFPDS
      *
      *     SOURCE-NAME      :  DRMA4576
      *
      *     GENERIERUNG      :  11.03.2014/18:59:54
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00044!11.03.14!2.1  ! NEUERSTELLUNG             ! WOLFF
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA4576.
              07 FILLER                 occurs 23
                                        pic X.
              07 TAG-ID-VALUE-1.
                 09 TAGNAME-1           pic X(5).
                 09 FILLER              occurs 4
                                        pic X.
                 09 DATUM               pic X(10).
              07 TAG-ID-VALUE-2 redefines TAG-ID-VALUE-1.
                 09 TAGNAME-2           pic X(4).
                 09 FILLER              occurs 4
                                        pic X.
                 09 VBNR                pic 9(7).
      *
      **************************************************************

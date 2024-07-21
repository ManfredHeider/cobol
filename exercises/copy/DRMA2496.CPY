      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2496.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA2496.CPv  $
      *
      *   Rev 2.1   Mar 12 2014 13:43:48   t13ac20
      *AMA00044: PDF-Generierung: übernommene Member aus Aachen
      *
      *   Rev 2.0   Nov 26 2013 16:10:42   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: PDF DOKUMENT-GRÖßEN
      *
      *     SOURCE-NAME      :  DRMA2496
      *
      *     GENERIERUNG      :  12.03.2014/09:32:51
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00044!11.03.14!2.1  ! NEU WEGEN PDF-ERSTELLUNG  !VON HELDEN
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2496.
              07 FILLER                 occurs 12
                                        pic X.
              07 KLAMMER1               pic X.
              07 DOCSIZE                pic 9(11).
              07 DOCSIZE-ALPHA redefines DOCSIZE
                                        pic X(11).
              07 KLAMMER2               pic X.
              07 FILLER                 occurs 2
                                        pic X.
              07 PDFDATEI               pic X(31).
      *
      **************************************************************

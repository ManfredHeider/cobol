      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  11.03.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DPAT0073.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DPAT0073.CPv  $
      *
      *   Rev 2.1   Mar 12 2020 11:35:06   t13ae34
      *AAT00081: Abbruchroutine ILBOABN0 durch CEE3ABD ersetzen
      *
      *   Rev 2.0   Mar 11 2020 13:42:30   u130377
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DPAT0073                                      *
      *    --------------------------------------------------------   *
      *    BATCHRAHMEN: ABBRUCHROUTINE CEE3ABD                        *
      *    --------------------------------------------------------   *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAT00081!11.03.20! NEU                            ! HEIDER    *
      *         !        !                                !           *
      *                                                               *
      *****************************************************************
      *
      * ROUTINEN: - BR06-ABBRUCHROUTINE
      *
      *---------------------------------------------------------------*
       BR06-ABBRUCHROUTINE SECTION.
      *---------------------------------------------------------------*
      * INPUT : -
      * OUTPUT:
      * VERARBEITUNG:
      *         - AUFRUF CEE3ABD
      *---------------------------------------------------------------*
           MOVE 'BR06-'                  TO ERR-ORT-SEC
      *
CCCPY1*    CALL K-CEE3ABD  USING K-CEE3ABD-CODE, K-CEE3ABD-TIMING
CCCPY2*    END-CALL
           .
       BR06-EXIT.
           EXIT.
      *------------------------ ENDE DPAT0073 ------------------------*


      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0127.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAN0127.CPv  $
      *
      *   Rev 2.3   22 Dec 2006 19:58:54   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Sep 26 2006 10:25:58   t13ae34
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   15 Sep 2006 16:23:02   t13ae23
      *AAN00655: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   12 Sep 2006 16:26:42   T13AE23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0127  FUNKTION INSERT STANDARD-OZP
      *                 FÜR TBAN0009
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN0665 !14.09.06!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      * AAN00655!22.09.06!2.2 ! ÄNDERUNG                  ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0121                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE ANTP TBAN0009              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0127.
           05 PI-DRAN0127-DATEN.
              10 PI-DRAN0127-ANTRAG-DATEN.
                 15 PI-DRAN0127-ANTRAG-PRODUKT.
                    COPY DRAN0143.
      *
       01  PO-DRAN0127.
           05 PO-DRAN0127-RC                      PIC X(003).
             88 PO-DRAN0127-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0127-RC-DUPLICATE-VALUE        VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ***************************************

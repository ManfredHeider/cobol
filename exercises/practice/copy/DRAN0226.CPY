      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.04.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0226.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0226.CPv  $
      *
      *   Rev 2.2   Sep 17 2009 10:39:56   t13ae34
      *AAN00917:  Anpassung des Antragsmoduls
      *
      *   Rev 2.1   Apr 23 2009 10:16:46   t13ae34
      *AAN00882: Antrag
      *
      *   Rev 2.0   Apr 21 2009 10:37:16   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0226                                     *
      *    FUNKTION   : SCHREIBEN DATEI AN008900                     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00882!21.04.09!2.1 ! NEU                       !HEIDER    *
      * AAN00917!11.09.09!2.2 ! UNTERFUNKTIONSCODES       !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0226                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0226.
110909*>>>>>
           05 PI-DRAN0226-UFKT                   PIC 9(01).
           88 PI-DRAN0226-UFKT-OPEN              VALUE 1.
           88 PI-DRAN0226-UFKT-WRITE             VALUE 2.
           88 PI-DRAN0226-UFKT-CLOSE             VALUE 3.
110909*<<<<<
          COPY DRAN0224.
      *
       01 PO-DRAN0226.
          05 PO-DRAN0226-RC                      PIC  X(003).
             88 PO-DRAN0226-RC-OK                     VALUE 'OK '.
          05 PO-DRAN0226-DATEN.
             10 FILLER                           PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************

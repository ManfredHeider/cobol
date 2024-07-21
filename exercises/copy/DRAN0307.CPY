      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0307.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0307.CPv  $
      *
      *   Rev 2.1   Mar 31 2016 08:23:56   t13ae34
      *AAN01230: Archivierung der Generali Verbraucherinformation
      *
      *   Rev 2.0   Mar 29 2016 17:02:04   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0307  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01230!29.03.16!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0307                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE GENL TBAN0012              *
      *                                                              *
      ****************************************************************
       01  PI-DRAN0307.
           05 PI-DRAN0307-GENL-KEY.
               10 PI-DRAN0307-PGEB-GES          PIC S9(4) COMP.
               10 PI-DRAN0307-GENL-PIN          PIC 9(7).
      *
           05 PI-DRAN0307-GENL-DATEN.
               10 PI-DRAN0307-GENL-PDF.
                  15 PI-DRAN0307-GENL-PDF-LENGTH PIC S9(9) COMP-5.
                  15 PI-DRAN0307-GENL-PDF-DATA   PIC X(1048576).
      *
       01  PO-DRAN0307.
           05 PO-DRAN0307-RC                    PIC  X(003).
             88 PO-DRAN0307-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0307-RC-CHANGED              VALUE 'CHA'.
             88 PO-DRAN0307-RC-NOT-FOUND            VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************



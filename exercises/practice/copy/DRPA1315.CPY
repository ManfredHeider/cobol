      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1315.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRPA1315.CPv  $
      *
      *   Rev 2.2   24 Apr 2001 11:55:28   t13ex21
      *APA00096 - Weitere Unterfunktion
      *
      *   Rev 2.1   Jan 04 2000 13:11:52   T13AE18
      *APA00068
      *
      *
      *   Rev 2.0   Nov 29 1999 13:55:16   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1315 FUNKTION 'LÖSCHEN TS-QUEUE'         *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    03.01.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00068!03.01.00!2.01!NEU                        !BOHLINGER *
      * APA00096!23.04.01!2.02! NEUE UNTERFUNKTION        ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1315                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1315.
          05 PI-DRPA1315-UFKT                  PIC 9(002).
             88 PI-DRPA1315-DEL-TSQ-PROD           VALUE 01 .
             88 PI-DRPA1315-DEL-TSQ-PGVZ           VALUE 02 .
230401       88 PI-DRPA1315-DEL-TSQ-PGKF           VALUE 03 .

       01 PO-DRPA1315.
          05 PO-DRPA1315-RC                    PIC  X(003).
             88 PO-DRPA1315-RC-OK                   VALUE 'OK '.

      *
      ******** Ende Copy-Member **************************************

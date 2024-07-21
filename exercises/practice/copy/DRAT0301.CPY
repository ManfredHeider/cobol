      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0301.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drat0301.cpv  $
      *
      *   Rev 2.1   Sep 09 1999 11:12:18   T13EX44
      * 
      *
      *   Rev 2.0   Mar 16 1999 09:57:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0301                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF MAGNETBANDBEGLEITZETTEL DRUCKEN            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    03.09.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00055!03.09.99! 2.0! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0301                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
          01 PI-DRAT0301.
           05 PI-DRAT0301-DTANR        PIC  9(005).
           05 PI-DRAT0301-EMPFAENGER   PIC  X(030).
           05 PI-DRAT0301-ABSENDER     PIC  X(030).
           05 PI-DRAT0301-INHALT       PIC  X(030).
           05 PI-DRAT0301-DSNAME       PIC  X(030).
           05 PI-DRAT0301-KENNSAETZE   PIC  X(030).
           05 PI-DRAT0301-DICHTE       PIC  X(008).
           05 PI-DRAT0301-RECFM        PIC S9(004)       BINARY.
            88 PI-DRAT0301-RECFM-F     VALUE +0001.
            88 PI-DRAT0301-RECFM-FA    VALUE +0002.
            88 PI-DRAT0301-RECFM-FB    VALUE +0003.
            88 PI-DRAT0301-RECFM-FBA   VALUE +0004.
            88 PI-DRAT0301-RECFM-U     VALUE +0005.
            88 PI-DRAT0301-RECFM-V     VALUE +0006.
            88 PI-DRAT0301-RECFM-VA    VALUE +0007.
            88 PI-DRAT0301-RECFM-VB    VALUE +0008.
            88 PI-DRAT0301-RECFM-VBA   VALUE +0009.
           05 PI-DRAT0301-SATZLAENGE   PIC  9(008).
           05 PI-DRAT0301-BLOCKLAENGE  PIC  9(008).
           05 PI-DRAT0301-BANDANZAHL   PIC  9(004).
           05 FILLER                   OCCURS 10 TIMES.
            10 PI-DRAT0301-BANDNR      PIC  X(008).
           05 PI-DRAT0301-BANDERSTELLT PIC  X(010).
           05 PI-DRAT0301-DATERSTELLT  PIC  X(010).
           05 PI-DRAT0301-SATZANZAHL   PIC  9(008).
      *
          01 PO-DRAT0301.
           05 PO-DRAT0301-RC           PIC  X(003).
            88 PO-DRAT0301-RC-OK              VALUE 'OK '.
            88 PO-DRAT0301-RC-NOK             VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0309.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0309.CPv  $
      *
      *   Rev 2.2   Feb 29 2000 08:37:46   T13EX13
      *APA00082: Währungsprüfung PEG
      *
      *   Rev 2.1   Jan 26 2000 16:58:52   T13EX13
      *APA00082 - Währungskennzeichen mit Datum
      *prüfen
      *
      *
      *   Rev 2.0   Mar 16 1999 09:57:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0309                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAT0309                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILKENS                                          *
      *    DATUM:    25.01.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Apa00082!25.01.00!    ! NEU                       ! WILKENS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRAT0309: IST WÄHRUNG FÜR DATUM GÜLTIG?,
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRAT0309.
           05  PI-DRAT0309-DATEN.
               10 PI-DRAT0309-BEZ-KURZ          PIC  X(004).
               10 PI-DRAT0309-GUELTIG-DAT       PIC  X(010).
      *
       01  PO-DRAT0309.
           05  PO-DRAT0309-RC                   PIC  X(003).
               88 PO-DRAT0309-RC-OK                        VALUE 'OK '.
               88 PO-DRAT0309-RC-NOK                       VALUE 'NOK'.
               88 PO-DRAT0309-RC-WAEHR-UNGUELT             VALUE 'WUG'.
      *


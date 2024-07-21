      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0304.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAT0304.CPv  $
      *
      *   Rev 2.2   Nov 29 1999 17:39:50   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.1   Nov 29 1999 11:00:02   T13AE34
      *AAN00236-PLZ
      *
      *   Rev 2.0   Mar 16 1999 09:57:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0304                                     *
      *    FUNKTION   : POSTLEITZAHLEN - ERMITTELN ORTSNAMEN ZU      *
      *                 GEGEBENER POSTLEITZAHL                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00236!07.10.99!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0304                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0304.
          05 PI-DRAT0304-DATEN.
      *         MUSS-FELD
             10 PI-DRAT0304-PLZ                  PIC  X(005).
      *         OPTIONAL
             10 PI-DRAT0304-ORT-ANFANG           PIC  X(035).
      *
       01 PO-DRAT0304.
          05 PO-DRAT0304-RC                      PIC  X(003).
      *         GENAU EINEN ORT GEFUNDEN
             88 PO-DRAT0304-RC-OK                     VALUE 'OK '.
      *         KEINEN ORT GEFUNDEN
             88 PO-DRAT0304-RC-NOT-FOUND              VALUE 'NFD'.
      *         MEHRERE ORTE GEFUNDEN
             88 PO-DRAT0304-RC-DUPLICATE-VALUE        VALUE 'DVA'.
          05 PO-DRAT0304-DATEN.
             10 PO-DRAT0304-ORT                  PIC  X(035).
      *
      ******** ENDE COPY-MEMBER **************************************

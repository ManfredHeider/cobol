      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0025.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAN0025.CPv  $
      *
      *   Rev 2.1   11 Sep 2000 12:24:38   T13AE34
      *AAN00279-konzessionspflichtiges Geschäft
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0025                                     *
      *    FUNKTION   : FRAGE: HAT VB EINGEREICHTES KONZESSIONS-     *
      *                 PFLICHTIGES GESCHÄFT IM JAHR JHJJ?           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    07.09.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00279!07.09.00!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0025                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0025.
          05 PI-DRAN0025-DATEN.
             10 PI-DRAN0025-VB-NR                PIC S9(009) COMP.
             10 PI-DRAN0025-JAHR                 PIC  X(004).
      *
       01 PO-DRAN0025.
          05 PO-DRAN0025-RC                      PIC  X(003).
             88 PO-DRAN0025-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0025-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0025-DATEN.
             10 FILLER                           PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************

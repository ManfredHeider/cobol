      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0032.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRAN0032.CPv  $
      *
      *   Rev 2.2   07 May 2001 09:32:40   t13ae34
      *FAN00306-AN50:  AG8-Änderung nicht mög-
      *lich + Update PZM über Vorschlagsnummer
      *+ VB oder VM
      *
      *   Rev 2.1   07 Mar 2001 09:57:40   T13AE34
      *AAN00268-PZM Antrag
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0032                                     *
      *    FUNKTION   : UPDATE PIN IN PZM-BESTAND                    *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.12.2000                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00268!12.12.00!2.1 !NEU                        !HEIDER    *
      * FAN00306!07.05.01!2.2 !UPDATE TBAN0002 MIT AVERM  !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0032                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0032.
           05 PI-DRAN0032-PZMA-KEY.
               10 PI-DRAN0032-PGEB-GES        PIC  X(2).
               10 PI-DRAN0032-PZM-HVANTNUM    PIC  X(9).
               10 PI-DRAN0032-PZM-VDABVERM    PIC  S9(9) COMP.
          05 PI-DRAN0032-DATEN.
               10 PI-DRAN0032-PZM-PIN         PIC  S9(9) COMP.
      *
       01 PO-DRAN0032.
          05 PO-DRAN0032-RC                      PIC  X(003).
             88 PO-DRAN0032-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0032-RC-NOK                    VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************

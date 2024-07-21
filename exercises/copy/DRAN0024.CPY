      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0024.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRAN0024.CPv  $
      *
      *   Rev 2.1   10 Jul 2000 15:18:32   T13AE34
      *AVB00487-Hat VB einger. Geschäft im 
      *Jahr JHJJ?
      *
      *   Rev 2.0   Feb 27 1999 11:05:34   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.0   30 Jul 1997 17:35:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0024                                     *
      *    FUNKTION   : FRAGE: HAT VB EINGEREICHTES GESCHÄFT IM JAHR *
      *                 JHJJ                                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    05.07.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00002!12.06.98! 01 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0024                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0024.
          05 PI-DRAN0024-DATEN.
             10 PI-DRAN0024-VB-NR                PIC S9(009) COMP.
             10 PI-DRAN0024-JAHR                 PIC  X(004).
      *
       01 PO-DRAN0024.
          05 PO-DRAN0024-RC                      PIC  X(003).
             88 PO-DRAN0024-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0024-RC-NOK                    VALUE 'NOK'.
          05 PO-DRAN0024-DATEN.
             10 FILLER                           PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************

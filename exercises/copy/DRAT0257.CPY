      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0257.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0257.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Jul 1998 09:26:58   T13AE34
      *AAT00002 - Euro
      *
      *   Rev 1.0   27 Feb 1998 10:16:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0257                                     *
      *    FUNKTION   : EURO - VORBELEGUNG WäHRUNGSKENNZEICHEN       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    12.06.1998                                      *
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
      *    FÜR DIE FUNKTION DRAT0257                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0257.
          05 PI-DRAT0257-DATEN.
             10 PI-DRAT0257-PGEB-GES             PIC S9(004)  BINARY.
      *
       01 PO-DRAT0257.
          05 PO-DRAT0257-RC                      PIC  X(003).
             88 PO-DRAT0257-RC-OK                     VALUE 'OK '.
         05 PO-DRAT0257-DATEN.
             10 PO-DRAT0257-WKZ                  PIC  X(001).
             10 PO-DRAT0257-WKZ-TEXT             PIC  X(003).
             10 PO-DRAT0257-SCHALTER-WKZ         PIC  X(001).
                88 PO-DRAT0257-WKZ-GESPERRT             VALUE '1'.
      *
      ******** Ende Copy-Member **************************************

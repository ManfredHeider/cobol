      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0068.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRAN0068.CPv  $
      *
      *   Rev 2.1   Mar 06 2003 16:54:24   t13ae23
      *AAN00409 -  VIA BADENIA Elektron. Antrag -
      *
      *   Rev 2.0   Mar 18 2002 09:15:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0068                                     *
      *    FUNKTION   : UPDATE PIN IN VIA-BESTAND                    *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00409!07.03.03!2.1 !NEU                        !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0068                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0068.
           05 PI-DRAN0068-VIA-KEY.
               10 PI-DRAN0068-PGEB-GES         PIC  X(2).
               10 PI-DRAN0068-VIA-HVANTNUM     PIC  S9(17) COMP-3.
               10 PI-DRAN0068-VIA-VDABVERM     PIC  S9(9) COMP.
          05 PI-DRAN0068-DATEN.
               10 PI-DRAN0068-VIA-PIN          PIC  S9(9) COMP.
               10 PI-DRAN0068-VIA-PIN2         PIC  S9(9) COMP.
               10 PI-DRAN0068-VIA-HAND-BSV     PIC  X(1).
               10 PI-DRAN0068-VIA-HAND-TARIF   PIC  X(1).
               10 PI-DRAN0068-VIA-HAND-FRST    PIC  X(1).
               10 PI-DRAN0068-VIA-HAND-LEG     PIC  X(1).
               10 PI-DRAN0068-VIA-HAND-VL      PIC  X(1).
               10 PI-DRAN0068-VIA-HAND-VL2     PIC  X(1).
      *
       01 PO-DRAN0068.
          05 PO-DRAN0068-RC                    PIC  X(003).
             88 PO-DRAN0068-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0068-RC-NOK                  VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************


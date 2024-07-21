      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0096.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0096.CPv  $
      *
      *   Rev 2.1   Aug 06 2004 08:36:36   t13ae23
      *AAN00455: Elektronischer Antrag DWS - Programme -
      *
      *   Rev 2.0   Mar 18 2002 09:16:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0096                                     *
      *    FUNKTION   : UPDATE PIN IN DWS-BESTAND                   *
      *                                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00455!06.08.04!2.1 !NEU                        !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0096                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0096.
           05 PI-DRAN0096-DWS-KEY.
               10 PI-DRAN0096-PGEB-GES         PIC  X(2).
               10 PI-DRAN0096-DWS-HVANTNUM    PIC  X(9).
               10 PI-DRAN0096-DWS-VDABVERM    PIC  S9(9) COMP.
          05 PI-DRAN0096-DATEN.
               10 PI-DRAN0096-DWS-PIN          PIC  S9(9) COMP.
               10 PI-DRAN0096-DWS-HAND         PIC  X(1).
      *
       01 PO-DRAN0096.
          05 PO-DRAN0096-RC                    PIC  X(003).
             88 PO-DRAN0096-RC-OK                   VALUE 'OK '.
             88 PO-DRAN0096-RC-NOK                  VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************


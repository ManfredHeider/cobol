      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0755.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0755.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:28:16   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 26 1999 15:25:46   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:45:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0712  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: j.opritz                                        *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! j.opritz *
      * APA02908!17.02.20!2.1 ! PGEB-GES-VORV. HINZU      ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0753                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PVOR TBPA0108              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0755.
          05 PI-DRPA0755-PVOR-DATEN.
             10 PI-DRPA0755-PVOR-LAST-UPDATE PIC X(026).
             10 PI-DRPA0755-PVOR-KENNUNG     PIC X(084).
          05 PI-DRPA0755-PVOR-KEY.
             10 PI-DRPA0755-PRINTE-VERTRAG   PIC S9(015) PACKED-DECIMAL.
170220       10 PI-DRPA0755-PGEB-GES-VORVER  PIC S9(004) BINARY.
             10 PI-DRPA0755-PRINTE-VORVERTRAG
                                             PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0755.
          05 PO-DRPA0755-RC                  PIC  X(003).
             88 PO-DRPA0755-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0755-RC-DUPLICATE-VALUE            VALUE 'DVA'.
             88 PO-DRPA0755-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0755-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************


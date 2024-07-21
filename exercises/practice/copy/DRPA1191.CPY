      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1191.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:/MF/CPY/DRPA1191.CPv  $
      *
      *   Rev 2.0   Nov 26 2001 16:33:54   t13ex21
      *FPA00293 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:04:54   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1191  FUNKTION UPDATE ZUGRIFFS-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    30.10.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00293!30.10.01!2.01! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1191                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE BUCHUNGS-STAND <<<   TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1191.
          05 PI-DRPA1191-PDPT-LAST-UPDATE     PIC  X(26).
          05 PI-DRPA1191-PDPT-KEY.
             10 PI-DRPA1191-PDPT-ID           PIC S9(15)V USAGE COMP-3.
          05 PI-DRPA1191-PDPT-DATEN.
             10 PI-DRPA1191-PDPT-BUCH-RFOLGE  PIC S9(04) USAGE COMP.
             10 PI-DRPA1191-PDPT-FIRST-INSERT PIC  X(26).
      *
       01 PO-DRPA1191.
          05 PO-DRPA1191-RC                   PIC  X(003).
             88 PO-DRPA1191-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1191-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1191-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

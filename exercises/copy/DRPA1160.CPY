      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1160.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:/MF/CPY/DRPA1160.CPv  $
      *
      *   Rev 2.0   Nov 26 2001 16:33:16   t13ex21
      *FPA00293 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:02:06   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1160  FUNKTION UPDATE STANDARD-OZP       *
      *          UPDATE BUCHUNGSREIHENFOLGE UND PDPT-FIRST-INSERT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M. STORCH                                       *
      *    DATUM:    30.10.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00293!30.10.01! 02 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1160                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PDPT TBPA0122              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1160.
          05 PI-DRPA1160-PDPT-LAST-UPDATE        PIC X(26).
          05 PI-DRPA1160-PDPT-KEY.
             10 PI-DRPA1160-PDPT-ID              PIC S9(15)V COMP-3.
          05 PI-DRPA1160-PDPT-BUCH-RFOLGE        PIC S9(4) USAGE COMP.
          05 PI-DRPA1160-PDPT-FIRST-INSERT       PIC X(26).
      *
       01 PO-DRPA1160.
          05 PO-DRPA1160-RC                   PIC  X(003).
             88 PO-DRPA1160-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1160-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1160-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

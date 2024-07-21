      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0723.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0723.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:44   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0723  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0723                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMDS TBPA0123              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0723.
          05 PI-DRPA0723-PMDS-LAST-UPDATE      PIC X(26).
          05 PI-DRPA0723-PMDS-KEY.
           10 PI-DRPA0723-PMBE-ID              PIC S9(15)V COMP-3.
           10 PI-DRPA0723-PMBE-VERSION         PIC S9(4) COMP.
           10 PI-DRPA0723-PMDS-PMBE-ID         PIC S9(15)V COMP-3.
           10 PI-DRPA0723-PMDS-PMBE-VERSION    PIC S9(4) COMP.
      *
       01 PO-DRPA0723.
          05 PO-DRPA0723-RC                   PIC  X(003).
             88 PO-DRPA0723-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0723-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0723-RC-NOT-FOUND                  VALUE 'NFD'.


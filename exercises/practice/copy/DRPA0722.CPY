      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0722.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0722.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:42   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0722  Funktion Update Standard-OZP       *
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
      * APA00012!06.08.98! 02 ! Gueltig-Ab auf Timestamp  ! Jäger    *
      *         !        !    ! umstellen                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0722                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PMDS TBPA0123              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0722.
          05 PI-DRPA0722-PMDS-LAST-UPDATE      PIC X(26).
          05 PI-DRPA0722-PMDS-KEY.
           10 PI-DRPA0722-PMBE-ID              PIC S9(15)V COMP-3.
           10 PI-DRPA0722-PMBE-VERSION         PIC S9(4) COMP.
           10 PI-DRPA0722-PMDS-PMBE-ID         PIC S9(15)V COMP-3.
           10 PI-DRPA0722-PMDS-PMBE-VERSION    PIC S9(4) COMP.
          05 PI-DRPA0722-PMDS-DATEN.
           10 PI-DRPA0722-PMDS-KENNUNG         PIC X(8).
      *    10 PI-DRPA0722-PMDS-GUELTIG-AB      PIC X(10).
           10 PI-DRPA0722-PMDS-GUELTIG-AB      PIC X(26).
           10 PI-DRPA0722-PMDS-GUELTIG-BIS     PIC X(10).
      *
       01 PO-DRPA0722.
          05 PO-DRPA0722-RC                   PIC  X(003).
             88 PO-DRPA0722-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0722-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0722-RC-NOT-FOUND                  VALUE 'NFD'.


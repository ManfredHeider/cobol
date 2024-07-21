      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0724.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0724.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:46   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:43:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0724  Funktion Select Standard-OZP       *
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
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0724                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE PMDS TBPA0123              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0724.
          05 PI-DRPA0724-PMDS-KEY.
           10 PI-DRPA0724-PMBE-ID              PIC S9(15)V COMP-3.
           10 PI-DRPA0724-PMBE-VERSION         PIC S9(4) COMP.
           10 PI-DRPA0724-PMDS-PMBE-ID         PIC S9(15)V COMP-3.
           10 PI-DRPA0724-PMDS-PMBE-VERSION    PIC S9(4) COMP.
      *
       01 PO-DRPA0724.
          05 PO-DRPA0724-RC                   PIC  X(003).
             88 PO-DRPA0724-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0724-RC-DUPLICATE-VALUE            VALUE 'DVA'.
          05 PO-DRPA0724-PMDS-DATEN.
           10 PO-DRPA0724-PMDS-LAST-UPDATE     PIC X(26).
           10 PO-DRPA0724-PMDS-KENNUNG         PIC X(8).
      *    10 PO-DRPA0724-PMDS-GUELTIG-AB      PIC X(10).
           10 PO-DRPA0724-PMDS-GUELTIG-AB      PIC X(26).
           10 PO-DRPA0724-PMDS-GUELTIG-BIS     PIC X(10).


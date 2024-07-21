      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1154.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI1154.CPv  $
      *
      *   Rev 2.2   30 Jan 2001 08:50:24   T13EX13
      * 
      *
      *   Rev 2.1   05 Dec 2000 14:18:10   T13EX13
      * 
      *
      *   Rev 2.0   23 Nov 2000 11:09:04   p13sp16
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1154  FUNKTION DELETE STANDARD-OZP       *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * -------------------------------------------------------------*
      * FKI00431!05.12.00! 01 ! NEUERSTELLUNG             ! Blaess   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI1154                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE <<<             MITARBEITER                   *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI1154.
          05 PI-DRKI1154-MITARBPERSONENID      PIC  X(013).
          05 PI-DRKI1154-MITARBEITERNR         PIC S9(009) COMP.
      *
       01 PO-DRKI1154.
          05 PO-DRKI1154-RC                    PIC  X(003).
             88 PO-DRKI1154-RC-OK                   VALUE 'OK '.
             88 PO-DRKI1154-RC-NOT-FOUND            VALUE 'NFD'.
      *


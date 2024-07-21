      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1256.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1256.CPv  $
      *
      *   Rev 2.1   Jun 07 1999 12:19:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:16   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1256  FUNKTION SELECT ZTPA0105           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    04.06.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!04.06.99!01.01! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1256                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBPA0105 PNAP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1256.
          05 PI-DRPA1256-PNAP-KEY.
             10 PI-DRPA1256-PNAP-ID           PIC S9(015) COMP-3.
      *
       01 PO-DRPA1256.
          05 PO-DRPA1256-RC                   PIC  X(003).
             88 PO-DRPA1256-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1256-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1256-LAST-UPDATE          PIC  X(026).
          05 PO-DRPA1256-PNAP.
             COPY DRPA0407.
      *
      ****** ENDE COPY-MEMBER ****************************************

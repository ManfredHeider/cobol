      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0699.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0699.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:10:56   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   11 Nov 1997 17:01:08   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0699  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.12.97! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0699                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMBE TBPA0113              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0699.
          05 PI-DRPA0699-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0699-PMBE-KEY.
             10 PI-DRPA0699-PMBE-ID          PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA0699-PMBE-VERSION     PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0699.
          05 PO-DRPA0699-RC                   PIC  X(003).
             88 PO-DRPA0699-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0699-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0699-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************

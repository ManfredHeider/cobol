      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0713.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0713.CPv  $
      *
      *   Rev 2.0   May 26 1999 15:15:26   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:42:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0713  Funktion Delete Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0713                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PDPE TBPA0121              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0713.
          05 PI-DRPA0713-PDPE-LAST-UPDATE       PIC X(26).
          05 PI-DRPA0713-PDPE-KEY.
             10 PI-DRPA0713-PDPT-ID             PIC S9(15)V COMP-3.
             10 PI-DRPA0713-PDPE-POSITIONSNR    PIC S9(4) COMP.
      *
       01 PO-DRPA0713.
          05 PO-DRPA0713-RC                   PIC  X(003).
             88 PO-DRPA0713-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0713-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0713-RC-NOT-FOUND                  VALUE 'NFD'.


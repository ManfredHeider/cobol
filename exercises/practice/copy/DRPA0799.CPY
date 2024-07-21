      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0799.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0799.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:16:10   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   12 Dec 1997 09:48:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0799  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    14.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0799                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMBE TBPA0115              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0799.
          05 PI-DRPA0799-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA0799-PMPR-KEY.
           10 PI-DRPA0799-PMPR-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0799-PMPR-VERSION        PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA0799.
          05 PO-DRPA0799-RC                   PIC  X(003).
             88 PO-DRPA0799-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0799-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0799-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************

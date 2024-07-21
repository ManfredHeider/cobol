      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1143.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1143.CPv  $
      *
      *   Rev 2.0   Mar 11 1999 17:18:22   T13EX49
      *AKI00018
      *
      *   Rev 1.0   21 Oct 1998 14:00:14   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1143  FUNKTION DAUER (AMV) ERMITTELN     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                       *
      *    DATUM:    01.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!01.12.98! 01 ! NEUERSTELLUNG             ! SCHRIEBER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1143                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1143.
          05 PI-DRPA1143-DATUM-ANF           PIC  X(010).
          05 PI-DRPA1143-DATUM-END           PIC  X(010).
      *
       01 PO-DRPA1143.
          05 PO-DRPA1143-RC                  PIC  X(003).
             88 PO-DRPA1143-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1143-RC-NOK                VALUE 'NOK'.
             88 PO-DRPA1143-RC-EDAT-NOK           VALUE 'EDN'.
          05 PO-DRPA1143-DATEN.
             10 PO-DRPA1143-DAUER            PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************

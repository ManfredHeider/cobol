      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0339.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0339.CPv  $
      *
      *   Rev 2.0   May 27 1999 10:44:40   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   May 26 1997 15:21:42   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0339                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<HÖCHSTE PIN ERM.    >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    18.06.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!18.06.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0339                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0339.
          05 PI-DRPA0339-DATEN.
             10 PI-DRPA0339-PGEB-GES                PIC S9(004) BINARY.
      *
       01 PO-DRPA0339.
          05 PO-DRPA0339-RC                         PIC  X(003).
             88 PO-DRPA0339-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0339-RC-NOK                        VALUE 'NOK'.
             88 PO-DRPA0339-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA0339-DATEN.
             10 PO-DRPA0339-PIN                     PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

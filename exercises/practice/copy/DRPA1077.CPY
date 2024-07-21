      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1077.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1077.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:30:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:53:22   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   20 Jul 1998 10:20:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1077                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<NORMIEREN JNP       >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    23.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.09.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1077                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1077.
          05 PI-DRPA1077-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA1077-DATEN.
             10 PI-DRPA1077-PMPP.
                COPY DRPA0290.
      *
       01 PO-DRPA1077.
          05 PO-DRPA1077-RC                         PIC  X(003).
             88 PO-DRPA1077-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1077-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA1077-DATEN.
             10 PO-DRPA1077-PMPP.
                COPY DRPA0290.
      *
      ******* ENDE COPY-MEMBER **************************************

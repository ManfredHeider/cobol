      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0564.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0564.cpv  $
      *
      *   Rev 2.0   May 27 1999 14:02:08   T01JM2S
      *APA00042
      *
      *   Rev 1.0   22 Oct 1997 11:55:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0564                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMDS/PMBE ERSTELLEN >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    03.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!03.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0564                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0564.
          05 PI-DRPA0564-DATEN.
             10 PI-DRPA0564-PVER-VERSION            PIC S9(004) BINARY.
             10 PI-DRPA0564-PMBE-NEU.
                COPY DRPA0411.
             10 PI-DRPA0564-PMBE-ALT.
                COPY DRPA0411.
      *
       01 PO-DRPA0564.
          05 PO-DRPA0564-RC                         PIC  X(003).
             88 PO-DRPA0564-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0564-DATEN.
             10 PO-DRPA0564-PMBE.
                COPY DRPA0411.
             10 PO-DRPA0564-PMDS.
                COPY DRPA0570.
      *
      ******* ENDE COPY-MEMBER **************************************

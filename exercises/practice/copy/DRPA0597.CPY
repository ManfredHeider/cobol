      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0597.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0597.CPv  $
      *
      *   Rev 2.1   25 May 2007 08:02:50   t13ex44
      *APA00631: Vertrag mit mehr als 40 Produkten
      *
      *   Rev 2.0   Mar 10 1999 12:11:02   T13EX22
      *MFE-Umstellung
      *
      *   Rev 1.0   22 Oct 1997 13:13:44   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0597                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<NEUES PRUDUKT HINZUGKOMMEN>>             *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    11.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!11.11.97! 01 ! NEU                       ! MUND     *
      * APA00631!22.05.07!2.1 ! OCCURS AUF 100            ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0597                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0597.
          05 PI-DRPA0597-DATEN.
             07 PI-DRPA0597-PMPP.
                COPY DRPA0290.
             07 PI-DRPA0597-MAX-USED-PROD-TAB   PIC  S9(004) BINARY.
220507*      07 PI-DRPA0597-PROD-TAB      OCCURS 50
220507       07 PI-DRPA0597-PROD-TAB      OCCURS 100
                                          INDEXED BY IX-PROD-TAB.
                10 PI-DRPA0597-PROD-PROD-SCHL   PIC  S9(004) BINARY.
      *
       01 PO-DRPA0597.
          05 PO-DRPA0597-RC                   PIC   X(003).
             88 PO-DRPA0597-RC-NEU-PROD-JA    VALUE 'NPJ'.
             88 PO-DRPA0597-RC-NEU-PROD-NEIN  VALUE 'NPN'.
          05 PO-DRPA0597-DATEN.
             10 PO-DRPA0597-MAX-USED-KZ-TAB   PIC  S9(004) BINARY.
220507*      10 PO-DRPA0597-KZ-TAB            OCCURS 50
220507       10 PO-DRPA0597-KZ-TAB            OCCURS 100
                                              INDEXED BY IX-KZ-TAB.
                15 PO-DRPA0597-PROD-KZ        PIC    X(003).
                   88 PO-DRPA0597-PROD-NEU    VALUE 'NEU'.
                   88 PO-DRPA0597-PROD-ALT    VALUE 'ALT'.
      *
      ******* ENDE COPY-MEMBER **************************************

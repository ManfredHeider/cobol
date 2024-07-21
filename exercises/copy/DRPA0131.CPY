      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0131.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0131.CPv  $
      *
      *   Rev 2.1   Oct 19 2017 09:45:52   t13ac20
      *AKI01534: G20 - Projekt LENA: Anpassung Datumsprüfung
      *
      *   Rev 2.0   Feb 27 1999 14:27:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:02:06   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0131  FUNKTION Datum prüfen              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !03.09.96! 01 ! Neuerstellung             ! Prömpeler*
191017* AKI01534!19.10.17!2.1 ! NEUES FORMAT ISO          ! WOLFF    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0131                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0131.
          05 PI-DRPA0131-DATUM              PIC  X(010).
          05 PI-DRPA0131-DATUM-SQL          REDEFINES PI-DRPA0131-DATUM
                                            PIC  X(010).
          05 PI-DRPA0131-DATUM-6-R          REDEFINES PI-DRPA0131-DATUM.
             10 PI-DRPA0131-DATUM-6         PIC  X(006).
             10 PI-DRPA0131-DATUM-TTMMJJ-N  REDEFINES
                PI-DRPA0131-DATUM-6         PIC  9(006).
             10 PI-DRPA0131-DATUM-TTMMJJ-X  REDEFINES
                PI-DRPA0131-DATUM-6         PIC  X(006).
             10 PI-DRPA0131-DATUM-JJMMTT-N  REDEFINES
                PI-DRPA0131-DATUM-6         PIC  9(006).
             10 PI-DRPA0131-DATUM-JJMMTT-X  REDEFINES
                PI-DRPA0131-DATUM-6         PIC  X(006).
             10 FILLER                      PIC  X(004).
          05 PI-DRPA0131-DATUM-8-R          REDEFINES PI-DRPA0131-DATUM.
             10 PI-DRPA0131-DATUM-8          PIC  X(008).
             10 PI-DRPA0131-DATUM-TTMMJJJJ-N REDEFINES
                PI-DRPA0131-DATUM-8          PIC  9(008).
             10 PI-DRPA0131-DATUM-TTMMJJJJ-X REDEFINES
                PI-DRPA0131-DATUM-8          PIC  X(008).
             10 PI-DRPA0131-DATUM-JJJJMMTT-N REDEFINES
                PI-DRPA0131-DATUM-8          PIC  9(008).
             10 PI-DRPA0131-DATUM-JJJJMMTT-X REDEFINES
                PI-DRPA0131-DATUM-8          PIC  X(008).
             10 FILLER                       PIC  X(002).
191017    05 PI-DRPA0131-DATUM-ISO          REDEFINES PI-DRPA0131-DATUM
191017                                      PIC  X(010).
          05 PI-DRPA0131-DATUM-FORMAT        PIC S9(004) BINARY.
             88 PI-DRPA0131-FORMAT-SQL                   VALUE 1.
             88 PI-DRPA0131-FORMAT-TTMMJJ                VALUE 2.
             88 PI-DRPA0131-FORMAT-JJMMTT                VALUE 3.
             88 PI-DRPA0131-FORMAT-TTMMJJJJ              VALUE 4.
             88 PI-DRPA0131-FORMAT-JJJJMMTT              VALUE 5.
191017       88 PI-DRPA0131-FORMAT-ISO                   VALUE 6.
 
       01 PO-DRPA0131.
          05 PO-DRPA0131-RC                 PIC  X(003).
             88 PO-DRPA0131-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0131-RC-NOK                       VALUE 'NOK'.
          05 PO-DRPA0131-DATEN.
             10 PO-DRPA0131-DATUM           PIC  X(010).
      *
      ******** Ende Copy-Member **************************************


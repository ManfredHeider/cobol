      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0127.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0127.CPv  $
      *
      *   Rev 2.2   Sep 21 2018 12:13:30   t13ex44
      *APA02548: Nachfolgeregelung Direktion Familie
      *
      *   Rev 2.1   May 26 2018 15:08:42   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.0   Feb 27 1999 14:27:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:40   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0127  FUNKTION AVERM PROVISION BERECHNEN *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    10.09.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !10.09.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * APA02579!24.04.18!2.4 ! GES UND PROD HINZU        ! SCHEIDEL *
      * APA02548!27.06.18!2.5 ! RED-FAKTOR RESTSTRUKTUR   ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0127                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0127.
          05 PI-DRPA0127-PZVP-VERSION        PIC S9(004)  BINARY.
          05 PI-DRPA0127-DATEN.
             10 PI-DRPA0127-DIFF-PROV-STUFE  PIC S9(004)  BINARY.
             10 PI-DRPA0127-GRUND-PROV       PIC S9(009)V99
                                             PACKED-DECIMAL.
240418       10 PI-DRPA0127-PGEB-GES         PIC S9(004)  BINARY.
240418       10 PI-DRPA0127-PROD-PROD-SCHL   PIC S9(004)  BINARY.
270618       10 PI-DRPA0127-RED-PSTZ-RESTSTRK
270618                                       PIC S9(004)V9(04)
270618                                       PACKED-DECIMAL.
      *
       01 PO-DRPA0127.
          05 PO-DRPA0127-RC                  PIC  X(003).
             88 PO-DRPA0127-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0127-DATEN.
             10 PO-DRPA0127-MA-PROV          PIC S9(009)V99
                                             PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************
 


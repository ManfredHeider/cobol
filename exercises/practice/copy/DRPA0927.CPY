      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0927.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0927.CPv  $
      *
      *   Rev 2.1   Jan 18 2000 14:37:52   T13EX22
      *APA00071: SF44 mit TBPA0026-Zugriff
      *
      *   Rev 2.0   Feb 27 1999 14:29:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:14:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0927  FUNKTION LIST PGV ZUORDNUNG        *
      *                           SORTIERT KRITERIEN, DIE ÜBER       *
      *                           SCHALTER ÜBERGEBEN WERDEN          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                            *
      *    DATUM:    22.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00071!22.11.99! 01 ! NEUERSTELLUNG             !MUND      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

      *
       01 PI-DRPA0927.
          05 PI-DRPA0927-UFKT                PIC  9(002).
          05 PI-DRPA0927-AUSWAHL-KRITERIEN.
             10 PI-DRPA0927-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA0927-PGVZ-PGV         PIC  X(008).
             10 PI-DRPA0927-KNR-PGV          PIC S9(004)  BINARY.
             10 PI-DRPA0927-S-PGVZ-PGV       PIC  X(001).
             10 PI-DRPA0927-S-DAISY-PGV      PIC  X(001).
             10 PI-DRPA0927-S-SORT-DAISY-PGV  PIC  X(001).
      *
       01  PO-DRPA0927.
           05  PO-DRPA0927-RC                      PIC  X(03).
               88 PO-DRPA0927-RC-OK                     VALUE 'OK '.
               88 PO-DRPA0927-RC-EOF                    VALUE 'EOF'.
               88 PO-DRPA0927-RC-LEER                   VALUE 'LER'.
           05 PO-DRPA0927-ERGEBNISZEILE.
              COPY DRPA0955.

      ******* ENDE COPY-MEMBER **************************************

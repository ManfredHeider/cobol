      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1128.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1128.CPv  $
      *
      *   Rev 2.1   Jun 24 1999 17:55:12   T13EX21
      *APA00012 - PVCS-Kopf korrigiert
      *
      *   Rev 1.0   20 Jul 1998 10:22:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1128 FUNKTION UPDATE DER BESTANDSMELDUNGEN
      *                          IN DB2-TABELLEN                     *
      *                          FüR UGPA0002                        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    13.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!13.11.98! 01 !NEU                        !BOHLINGER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1128                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1128.
          10 FILLER                            PIC  X(001).

       01 PO-DRPA1128.
          05 PO-DRPA1128-RC                    PIC  X(003).
             88 PO-DRPA1128-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1128-RC-UNZULAESSIG              VALUE 'UNZ'.

          05 PO-DRPA1128-MELDUNG.
             10 PO-DRPA1128-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1128-MELDUNG-NR         PIC S9(004) BINARY.
      ******** Ende Copy-Member **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0939.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa0939.cpv  $
      *
      *   Rev 2.1   May 31 1999 07:42:52   T01JM2S
      *APA00012 - NEUE RC'S UPPA0100
      *
      *   Rev 2.0   Feb 27 1999 14:29:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:50:06   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 14:16:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0939                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERMITTELN PVW/BATCH >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    27.04.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.04.98! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      * APA00012!26.05.99!2.1 ! NEU PO-DRPA0939-RC-NOK    ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0939                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0939.
          05 PI-DRPA0939-DATEN.
             10 PI-DRPA0939-NPM-TEIL.
                COPY DRPA0940.
             10 PI-DRPA0939-MELD-TEIL.
                COPY DRPA0941.
      *
       01 PO-DRPA0939.
          05 PO-DRPA0939-RC                         PIC  X(003).
             88 PO-DRPA0939-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0939-RC-NICHT-BUCHEN               VALUE 'NBU'.
             88 PO-DRPA0939-RC-NOK                        VALUE 'NOK'.
          05 PO-DRPA0939-DATEN.
             10 PO-DRPA0939-MAX-USED-KOFV-TEIL      PIC S9(004) BINARY.
             10 PO-DRPA0939-KOFV-TEIL               OCCURS 4.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************

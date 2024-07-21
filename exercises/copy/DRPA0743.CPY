      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0743.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0743.CPv  $
      *
      *   Rev 2.2   Jun 09 2020 11:27:24   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.1   05 Nov 1999 13:57:52   T01JM2S
      *APA00065 - VERTRAGSSTRUKTUR VON EVS-PRODUKTEN VERDICHTEN
      *
      *   Rev 2.0   Feb 27 1999 14:29:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Dec 1997 09:44:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0743                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVPT VERDICHTEN     >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.11.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00065!02.11.99!2.1 ! NEU                       ! WOLFF    *
      * APA02908!17.02.20!2.2 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0743                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0743.
          05 PI-DRPA0743-DATEN.
             10 PI-DRPA0743-BASIS-GES       PIC S9(004) BINARY.
             10 PI-DRPA0743-FAELLIG-VON     PIC  X(010).
             10 PI-DRPA0743-PVPT.
                COPY DRPA0420.
      *
       01 PO-DRPA0743.
          05 PO-DRPA0743-RC                 PIC  X(003).
             88 PO-DRPA0743-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0743-RC-VSTAT-MIT-FEHL             VALUE 'VMF'.
             88 PO-DRPA0743-RC-VSTAT-OHNE-FEHL            VALUE 'VOF'.
          05 PO-DRPA0743-DATEN.
             10 PO-DRPA0743-PVPT.
                COPY DRPA0420.
      *
      ******* ENDE COPY-MEMBER **************************************


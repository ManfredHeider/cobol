      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0650.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0650.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:26:46   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 11:42:52   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   11 Nov 1997 16:57:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0650                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PE-STRUKTUR ERMITTEL>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    27.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!27.11.97! 01 ! NEU                       ! PRIGGEN  *
      * APA02908!17.02.20!2.1 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0650                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0650.
          05 PI-DRPA0650-DATEN.
170220       10 PI-DRPA0650-PGEB-GES   PIC S9(004) BINARY.
             10 PI-DRPA0650-PEST-ID    PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0650.
          05 PO-DRPA0650-RC            PIC  X(003).
             88 PO-DRPA0650-RC-OK           VALUE 'OK '.
             88 PO-DRPA0650-RC-NOT-FOUND    VALUE 'NFD'.
          05 PO-DRPA0650-DATEN.
             10 PO-DRPA0650-PEST-KENNUNG
                                       PIC X(008).
             10 PO-DRPA0650-PEST-LAST-UPDATE
                                       PIC X(026).
             10 PO-DRPA0650-PEST-PEBD.
                COPY DRPA0433.
      *
      ******* ENDE COPY-MEMBER **************************************


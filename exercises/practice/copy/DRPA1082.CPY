      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1082.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1082.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:29:42   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 14:15:28   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:20:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1082                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM./ABGLEICHEN VVNR>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    02.10.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!02.10.98! 01 ! NEU                       ! WOLFF    *
      * APA02908!17.02.20!2.1 ! GES-UEBERGR EVS           ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1082                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1082.
          05 PI-DRPA1082-DATEN.
170220       10 PI-DRPA1082-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1082-PRINTE-VERTRAG          PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1082-MAX-USED-VVNR           PIC S9(004) BINARY.
             10 PI-DRPA1082-TABELLE-VVNR.
                15 PI-DRPA1082-TAB-VVNR             OCCURS 10
                                        INDEXED BY PI-DRPA1082-IX-VVNR.
170220             20 PI-DRPA1082-VVGES             PIC S9(004) BINARY.
                   20 PI-DRPA1082-VVNR              PIC  X(015).
      *
       01 PO-DRPA1082.
          05 PO-DRPA1082-RC                         PIC  X(003).
             88 PO-DRPA1082-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1082-RC-NOK                        VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************


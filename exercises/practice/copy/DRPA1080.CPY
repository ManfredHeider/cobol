      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1080.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1080.CPv  $
      *
      *   Rev 2.1   Jun 09 2020 11:29:26   t13ex44
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.0   May 27 1999 14:13:38   T01JM2S
      *APA00042
      *
      *   Rev 1.0   20 Jul 1998 10:20:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1080                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. VVNR ZU PRINTE >>                   *
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
      * APA02908!17.02.20!2.1 ! PGEB-GES-VORVER. HINZU    ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1080                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1080.
          05 PI-DRPA1080-DATEN.
170220       10 PI-DRPA1080-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1080-PRINTE-VERTRAG          PIC S9(015)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA1080.
          05 PO-DRPA1080-RC                         PIC  X(003).
             88 PO-DRPA1080-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1080-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1080-DATEN.
             10 PO-DRPA1080-MAX-USED-VVNR           PIC S9(004) BINARY.
             10 PO-DRPA1080-TABELLE-VVNR.
                15 PO-DRPA1080-TAB-VVNR             OCCURS 10
                                        INDEXED BY PO-DRPA1080-IX-VVNR.
170220             20 PO-DRPA1080-VVGES             PIC S9(004) BINARY.
                   20 PO-DRPA1080-VVNR              PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************


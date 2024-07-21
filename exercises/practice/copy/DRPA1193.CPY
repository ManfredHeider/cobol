      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1193.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1193.CPv  $
      *
      *   Rev 2.1   24 May 2007 07:11:12   t13ex44
      *APA00631: Vertrag mit mehr als 40 Produkten
      *
      *   Rev 2.0   Mar 25 1999 14:10:46   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   21 Oct 1998 14:05:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1193 FUNKTION GET TUNNINGFELDER          *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    19.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!19.03.99! 01  !NEU                       !BOHLINGER *
      * APA00631!22.05.07!2.1  ! ANZAHL PVPT AUF 100      ! SCHEIDEL *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1193                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1193.
             10 PI-DRPA1193-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1193-MAX-USED-PVPT      PIC S9(004) BINARY.
220507*      10 PI-DRPA1193-PVPT-TAB           OCCURS 20.
220507       10 PI-DRPA1193-PVPT-TAB           OCCURS 100.
                15 PI-DRPA1193-PVPT-ID         PIC S9(015)
                                               PACKED-DECIMAL.

       01 PO-DRPA1193.
          05 PO-DRPA1193-RC                    PIC  X(003).
             88 PO-DRPA1193-RC-OK                       VALUE 'OK '.

          05 PO-DRPA1193-DATEN.
             10 PO-DRPA1193-AKT-DGV            PIC S9(004) BINARY.
             10 PO-DRPA1193-AKT-PMBE-ID        PIC S9(015)
                                               PACKED-DECIMAL.
             10 PO-DRPA1193-AKT-FAELLIG-VON    PIC  X(010).
             10 PO-DRPA1193-AKT-FAELLIG-BIS    PIC  X(010).

             10 PO-DRPA1193-MAX-USED-PVPT      PIC S9(004) BINARY.
220507*      10 PO-DRPA1193-PVPT-TAB           OCCURS 20.
220507       10 PO-DRPA1193-PVPT-TAB           OCCURS 100.
                15 PO-DRPA1193-PVPT-ID         PIC S9(015)
                                               PACKED-DECIMAL.
                15 PO-DRPA1193-PEST-ID         PIC S9(015)
                                               PACKED-DECIMAL.
                15 PO-DRPA1193-RC-PEST         PIC  X(003).
                   88 PO-DRPA1193-RC-PEST-OK           VALUE 'OK '.
                   88 PO-DRPA1193-RC-PEST-NOT-FOUND    VALUE 'NFD'.
      *
      ******** Ende Copy-Member **************************************

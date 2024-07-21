      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1125.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1125.CPv  $
      *
      *   Rev 2.1   24 May 2007 07:12:10   t13ex44
      *APA00631: Vertrag mit mehr als 40 Produkten
      *
      *   Rev 2.0   May 26 1999 13:52:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 13:58:34   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1125  FUNKTION                           *
      *                 'GET VERTRAGSDATEN FüR UPDATE'   (UGPA0001)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    11.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!11.11.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00631!22.05.07!2.1 ! ANZAHL PVPT AUF 100       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1125                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1125.
          05 PI-DRPA1125-PRINTE              PIC S9(015)V USAGE COMP-3.
          05 PI-DRPA1125-PVER-VERSION        PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA1125.
          05 PO-DRPA1125-RC                  PIC  X(003).
             88 PO-DRPA1125-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1125-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1125-DATEN.
             10 PO-DRPA1125-EVS-VNR          PIC  X(015).
             10 PO-DRPA1125-S-EVS-VNR        PIC  X(001).
             10 PO-DRPA1125-PVER-DATEN.
                35  PVER-AKT-EINH            PIC S9(006)V99 COMP-3.
                COPY DRPA0408.
             10 PO-DRPA1125-PVPT-DATEN.
                25  PVPT-TABELLE.
                   27 MAX-USED-PVPT2         PIC S9(004) BINARY.
220507*            27 FILLER                 OCCURS 40
220507             27 FILLER                 OCCURS 100
                                             INDEXED BY IX-PVPT2.
                      COPY DRPA0410.

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1141.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1141.CPv  $
      *
      *   Rev 2.1   Nov 03 2006 10:14:20   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 13:52:14   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:00:04   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1141  FUNKTION                           *
      *                 GET ursprungs JAHRESNETTOPRäMIE  (UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.11.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      * APA00452!01.11.06! 2.1 ! DREIST. PRODUKTSCHLUESSEL! SCHEIDEL *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1141                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1141.
          05 PI-DRPA1141-DATEN.
             10 PI-DRPA1141-PRINTE           PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA1141-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA1141-VNR              PIC X(015).
011106*      10 PI-DRPA1141-PROD-SCHL        PIC S9(004)  BINARY.
011106       10 PI-DRPA1141-PROD-PROD-SCHL   PIC S9(004)  BINARY.
      *
       01 PO-DRPA1141.
          05 PO-DRPA1141-RC                        PIC  X(003).
             88 PO-DRPA1141-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1141-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1141-MELDUNG.
             10 PO-DRPA1141-MELDUNG-TYP           PIC  X(001).
             10 PO-DRPA1141-MELDUNG-NR            PIC S9(004) BINARY.

          05 PO-DRPA1141-DATEN.
             10 PO-DRPA1141-JAHRES-NT-PRAEMIE     PIC S9(09)V99
                                                  PACKED-DECIMAL.
             10 PO-DRPA1141-KNR-WAEHRUNG-BK       PIC X(004).
      *
      ******** ENDE COPY-MEMBER **************************************

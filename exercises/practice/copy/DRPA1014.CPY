      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1014.CPv  $
      *
      *   Rev 2.1   Nov 03 2006 10:13:52   t13ex44
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 25 1999 13:54:56   T13AE18
      *apa00012
      *
      *   Rev 1.0   14 Jan 1998 12:47:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1014  FUNKTION                           *
      *                           GET JAHRESNETTOPRäMIE  (UGPA0002)  *
      *    --------------------------------------------------------- *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    14.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!31.07.98! 01  !NEUERSTELLUNG             ! BOHLINGER*
      * APA00452!01.11.06! 2.1 ! DREIST. PRODUKTSCHLUESSEL! SCHEIDEL *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1014                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1014.
          05 PI-DRPA1014-DATEN.
             10 PI-DRPA1014-PRINTE           PIC S9(015)V USAGE COMP-3.
             10 PI-DRPA1014-PGEB-GES         PIC S9(004)  BINARY.
             10 PI-DRPA1014-VNR              PIC X(015).
011106*      10 PI-DRPA1014-PROD-SCHL        PIC S9(004)  BINARY.
011106       10 PI-DRPA1014-PROD-PROD-SCHL   PIC S9(004)  BINARY.
      *
       01 PO-DRPA1014.
          05 PO-DRPA1014-RC                        PIC  X(003).
             88 PO-DRPA1014-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1014-RC-NOT-FOUND          VALUE 'NFD'.

          05 PO-DRPA1014-MELDUNG.
             10 PO-DRPA1014-MELDUNG-TYP           PIC  X(001).
             10 PO-DRPA1014-MELDUNG-NR            PIC S9(004) BINARY.

          05 PO-DRPA1014-DATEN.
             10 PO-DRPA1014-JAHRES-NT-PRAEMIE     PIC S9(09)V99
                                                  PACKED-DECIMAL.
             10 PO-DRPA1014-KNR-WAEHRUNG-BK       PIC X(004).
      *
      ******** ENDE COPY-MEMBER **************************************

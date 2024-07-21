      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1056.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1056.CPv  $
      *
      *   Rev 2.1   03 Nov 2006 14:21:10   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   May 26 1999 15:48:38   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   01 Apr 1998 14:15:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1056 FUNKTION GET PROVISIONSDETAIL       *
      *                          MITTELS PDPT-ID                     *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    09.04.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!09.04.98! 01 !NEU                        !BOHLINGER *
      * APA00452!03.11.06! 2.1!UMBENENNUNG                !WANG      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1056                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1056.
             10 PI-DRPA1056-PRINTE             PIC S9(015)
                                               PACKED-DECIMAL.
             10 PI-DRPA1056-PDPT-ID            PIC S9(015)
                                               PACKED-DECIMAL.
          05 PI-DRPA1056-AUSWAHL-KRITERIEN.
             10 PI-DRPA1056-PGEB-GES           PIC 9(002).
             10 PI-DRPA1056-VNR                PIC X(015).

      *
       01 PO-DRPA1056.
          05 PO-DRPA1056-RC                    PIC  X(003).
             88 PO-DRPA1056-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1056-RC-NOT-FOUND                VALUE 'NFD'.

          05 PO-DRPA1056-MELDUNG.
             10 PO-DRPA1056-MELDUNG-TYP        PIC  X(001).
             10 PO-DRPA1056-MELDUNG-NR         PIC S9(004) BINARY.

      *** MAPSTEUERUNG DER DATENFELDER
      *   05 PO-DRPA1056-MAP-STEUERUNG.
      *      COPY DRPA0930.

          05 PO-DRPA1056-DATEN.
             10 PO-DRPA1056-PMBE-RFOLGE-NR     PIC S9(004) BINARY.
031106*      10 PO-DRPA1056-PROD-SCHL          PIC S9(004) BINARY.
031106       10 PO-DRPA1056-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PO-DRPA1056-ORDN-NR            PIC S9(004) BINARY.
             10 PO-DRPA1056-PROV-DATEN.
             COPY DRPA0901.
      *
      ******** Ende Copy-Member **************************************

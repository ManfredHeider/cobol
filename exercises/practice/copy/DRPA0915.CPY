      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0915.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA0915.CPv  $
      *
      *   Rev 2.1   Apr 10 2002 13:32:46   t13ex43
      *APA00177 : PO-GSMT-PROV-DAT hinzu
      *
      *   Rev 2.0   Feb 27 1999 14:29:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Nov 1998 15:47:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   10 Mar 1998 15:35:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0915                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GSMT-PROV P85 PRÜFEN>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!30.03.98! 01 ! NEU                       ! WOLFF    *
      * APA00177!10.04.02!2.01! PO-GSMT-PROV-DAT HINZU    ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0915                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0915.
          05 PI-DRPA0915-PZVP-VERSION               PIC S9(004) BINARY.
          05 PI-DRPA0915-DATEN.
             10 PI-DRPA0915-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA0915-PROD-PROD-SCHL          PIC S9(004) BINARY.
             10 PI-DRPA0915-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PI-DRPA0915-PRODART                 PIC S9(004) BINARY.
             10 PI-DRPA0915-LAUFZEIT                PIC S9(004) BINARY.
             10 PI-DRPA0915-PMPP-PPFL-BEITRAG       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0915-PMPP-PPFL-SUMME         PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0915-PMPP-PG-GSMT-PROV       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
      *
       01 PO-DRPA0915.
          05 PO-DRPA0915-RC                         PIC  X(003).
             88 PO-DRPA0915-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0915-RC-VSTAT-GEFUELLT             VALUE 'VSG'.
          05 PO-DRPA0915-DATEN.
100402       10 PO-DRPA0915-PMPE-GSMT-PROV-DAT      PIC  X(010).
             10 PO-DRPA0915-KOND-GSP.
                COPY DRPA0396.
      *
      ******* ENDE COPY-MEMBER **************************************

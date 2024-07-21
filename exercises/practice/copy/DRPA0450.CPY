      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0450.cpy  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\drpa0450.cpv  $
      *
      *   Rev 2.0   May 27 1999 13:53:34   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:38:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0450                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPT TRENNEN AP/FP  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                          *
      *    DATUM:    14.08.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.10.97! 01 ! NEU                       ! PRIGGEN  *
      *         !        !    !                           !          *
      * afpa0012!19.05.98! 01 ! rc-mfe eingefügt          ! parent   *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0450                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0450.
          05 PI-DRPA0450-DATEN.
             10 PI-DRPA0450-PPFL-BEGINN             PIC  X(010).
             10 PI-DRPA0450-GSMT-PROV-DAT           PIC  X(010).
             10 PI-DRPA0450-AP-FP-REGEL             PIC S9(004) BINARY.
             10 PI-DRPA0450-ANZAHL-MONATE-AP        PIC S9(009) BINARY.
             10 PI-DRPA0450-KNR-OBJEKT-HKF-TAB      PIC S9(004) BINARY.
             10 PI-DRPA0450-NTZ-PTR-ID              PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA0450-PMPR.
                COPY DRPA0412.
             10 PI-DRPA0450-PMPT.
                COPY DRPA0572.
      *
       01 PO-DRPA0450.
          05 PO-DRPA0450-RC                         PIC  X(003).
             88 PO-DRPA0450-RC-NOK                        VALUE 'NOK'.
             88 PO-DRPA0450-RC-KEINE-AENDERUNG            VALUE 'KAE'.
             88 PO-DRPA0450-RC-AEND-AP-AUF-FP             VALUE 'AAF'.
             88 PO-DRPA0450-RC-AEND-UND-SPLIT             VALUE 'AUS'.
             88 PO-DRPA0450-RC-MELDUNGSFEHLER             VALUE 'MFE'.
          05 PO-DRPA0450-DATEN.
             10 PO-DRPA0450-PMPT-AP.
                COPY DRPA0572.
             10 PO-DRPA0450-PMPT-FP.
                COPY DRPA0572.
      *
      ******* ENDE COPY-MEMBER **************************************

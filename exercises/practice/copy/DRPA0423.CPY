      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0423.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0423.CPv  $
      *
      *   Rev 2.3   Sep 10 2015 17:56:12   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für LUCY PGV's
      *
      *   Rev 2.2   Sep 10 2015 13:12:46   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *PVW-An
      *passungen für LUCY PGV's
      *
      *   Rev 2.1   16 May 2007 06:42:34   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.0   May 28 1999 13:03:02   T01JM2S
      *APA00042
      *
      *   Rev 1.0   23 Jul 1997 12:34:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0423                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVPT ERSTELLEN      >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    23.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!23.10.97! 01 ! NEU                       ! WOLFF    *
      * APA00604!12.04.07!2.1 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA01932!10.09.15!2.2 ! ERWEITERUNG PI-BEREICH    ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0423                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0423.
          05 PI-DRPA0423-DATEN.
             10 PI-DRPA0423-PEST-ID                 PIC S9(015)
                                                        PACKED-DECIMAL.
120407       10 PI-DRPA0423-ANTA-PIN   PIC S9(009)       BINARY.
             10 PI-DRPA0423-PMPE.
                COPY DRPA0289.
             10 PI-DRPA0423-PMPP.
                COPY DRPA0297.
120407*      10 PI-DRPA0423-PEW-ANTRAG.
120407*         COPY DRPA0434.
             10 PI-DRPA0423-PVER.
                COPY DRPA0408.
100915       10 PI-DRPA0423-PVPT.
100915         15 PI-DRPA0423-PVPT-MAX              PIC S9(004) BINARY.
100915         15 PI-DRPA0423-PVPT-TAB              OCCURS 100.
100915            COPY DRPA0410.
      *
       01 PO-DRPA0423.
          05 PO-DRPA0423-RC                         PIC  X(003).
             88 PO-DRPA0423-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0423-DATEN.
             10 PO-DRPA0423-PVPT.
                COPY DRPA0410.
      *
      ******* ENDE COPY-MEMBER **************************************


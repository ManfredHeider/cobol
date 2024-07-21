      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1206.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1206.CPv  $
      *
      *   Rev 2.2   Apr 02 2015 08:34:04   t13ex67
      *APA01986: EVS: Aufteilung Grund- und Mehrprämie
      *
      *   Rev 2.1   Apr 13 1999 10:28:54   T01JM2S
      *APA00012 - CRPA0032/CRPA0032B
      *
      *   Rev 2.0   Mar 30 1999 08:44:02   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1206                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. AKTUELLE JNP   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    30.03.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.03.99!2.1 ! NEU                       ! WOLFF    *
      * APA01986!28.01.15!2.2 ! PI/PO-BEREICH ERWEITERT   ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1206                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1206.
          05 PI-DRPA1206-DATEN.
280115       10 PI-DRPA1206-PGEB-GES                PIC S9(004) BINARY.
             10 PI-DRPA1206-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1206-PROD-PROD-SCHL          PIC S9(004) BINARY.
280115       10 PI-DRPA1206-STICHTAG                PIC  X(010).
      *
       01 PO-DRPA1206.
          05 PO-DRPA1206-RC                         PIC  X(003).
             88 PO-DRPA1206-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1206-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1206-DATEN.
             10 PO-DRPA1206-JAHRES-NT-PRAEMIE       PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
280115       10 PO-DRPA1206-JAHRES-NT-UR-PRAEM      PIC S9(009)V9(2)
                                                        PACKED-DECIMAL.
280115       10 PO-DRPA1206-KNR-ZAHLWEISE           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


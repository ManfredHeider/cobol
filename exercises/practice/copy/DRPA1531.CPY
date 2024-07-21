      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1531.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1531.CPv  $
      *
      *   Rev 2.2   Nov 02 2006 09:24:16   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Mar 20 2002 13:00:46   t13ex43
      *APA00177 : AMV-Rückbelastung mit alten Provisionssätzen
      *
      *   Rev 2.0   04 Jul 2001 11:11:20   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1531  FUNKTION SELECT DB2-LESE-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    19.03.2002                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00177!19.03.02!2.01! NEU                       ! KLEEMEIER*
      * APA00452!01.11.06!2.02! Änderung 3-stell. Produkte! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1531                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        ERMITTELN GUELTIG-AB FÜR KONDITION *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRPA1531.
           05  PI-DRPA1531-PGEB-GES           PIC S9(004)  BINARY.
011106*    05  PI-DRPA1531-PROD-SCHL          PIC S9(004)  BINARY.
011106     05  PI-DRPA1531-PROD-PROD-SCHL     PIC S9(004)  BINARY.
           05  PI-DRPA1531-KNR-KOND-ART       PIC S9(004)  BINARY.
           05  PI-DRPA1531-KTY-KOND-ART       PIC S9(004)  BINARY.
           05  PI-DRPA1531-PRKO-KOND-NR       PIC S9(004)  BINARY.
      *
       01  PO-DRPA1531.
           05  PO-DRPA1531-RC                 PIC  X(003).
               88 PO-DRPA1531-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1531-RC-NFD                       VALUE 'NFD'.
      *
           05  PO-DRPA1531-GUELTIG-AB         PIC  X(010).
           05  PO-DRPA1531-VORTAG             PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************

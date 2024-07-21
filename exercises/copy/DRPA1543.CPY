      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1543.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1543.CPv  $
      *
      *   Rev 2.1   Jul 23 2002 11:17:18   t13ex25
      *APA00207; Neue Funktion zur Prodartermittlung
      *
      *   Rev 2.0   04 Jul 2001 11:11:40   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1543                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<Produktart ableiten aus PGV, ppfl. Beginn*
      *                      und Fälligkeit>>                         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *                                                               *
      *    DATUM:    23.07.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00207!23.07.02! 01 ! NEU                       ! Jäger    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    Einführung wurde notwendig, da es zu bewegungsabhängigen  *
      *    AP's noch keine passende Prodart-Ermittlung gab           *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1543                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1543.
          05 PI-DRPA1543-ZVP-VERSION          PIC S9(004) BINARY.
          05 PI-DRPA1543-DATEN.
             10 PI-DRPA1543-PGV               PIC S9(004) BINARY.
             10 PI-DRPA1543-PPFL-BEGINN       PIC  X(010).
             10 PI-DRPA1543-FAELLIG-VON       PIC  X(010).
      *
       01 PO-DRPA1543.
          05 PO-DRPA1543-RC                   PIC  X(003).
             88 PO-DRPA1543-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1543-DATEN.
             10 PO-DRPA1543-PRODART           PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


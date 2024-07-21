      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1556.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1556.CPv  $
      *
      *   Rev 2.3   May 23 2006 07:36:40   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.2   May 19 2006 09:54:24   t13ex44
      *FPA00846: SA815 erweitern (ppfl. Summe und Sonder-KZ)
      *
      *   Rev 2.1   Sep 12 2002 17:49:54   t13ex43
      *APA00204 : Neue Funktion PMBE-Haftzeit ermitteln
      *
      *   Rev 2.0   Jul 04 2002 15:29:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1556 ERMITTELN PMBE-HAFTZEIT FÜR PMPT-ID  *
      *    -----------------------------------------------------------*
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    12.09.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!12.09.02!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1556                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1556.
          05 PI-DRPA1556-DATEN.
             10 PI-DRPA1556-PGEB-GES                PIC S9(004) COMP.
             10 PI-DRPA1556-PMPT-ID                 PIC S9(015) COMP-3.
             10 PI-DRPA1556-PMPT-VERSION            PIC S9(004) COMP.
      *
       01 PO-DRPA1556.
          05 PO-DRPA1556-RC                         PIC  X(003).
             88 PO-DRPA1556-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1556-RC-NOT-FOUND                 VALUE 'NFD'.
          05 PO-DRPA1556-DATEN.
             10 PO-DRPA1556-HAFTZEIT-BEGINN         PIC  9(008).
             10 PO-DRPA1556-HAFTZEIT-ENDE           PIC  9(008).
      *
      ******* ENDE COPY-MEMBER **************************************

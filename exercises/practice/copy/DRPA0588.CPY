      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0588.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0588.CPv  $
      *
      *   Rev 2.3   Oct 29 2008 09:58:36   i13az23
      *FPA01068: Erweiterung interne Tabellen für Prüfung ähnlicher
      * Produkte
      *
      *   Rev 2.2   Oct 28 2008 10:06:26   i13az23
      *FPA01068: Erweiterung interne Tabellen für Prüfung ähnlicher
      * Produkte
      *
      *   Rev 2.1   Jul 30 2008 11:12:42   t13ex44
      *FPA01032: Korrektur I180-Prüfung
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 11:42:22   T13AE34
      *AAN00167 - Vorvertragsnummer
      *
      *   Rev 1.0   22 Oct 1997 13:13:06   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0588 FUNKTION PRüFEN PRODUKT AUF         *
      *                          äHNLICHE PRODUKTE                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    31.10.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!31.10.97! 01 !NEU                        !BOHLINGER *
      * FPA01031!26.06.08!2.1 ! MAX-USED HINZU            ! SCHEIDEL *
      * FPA01068!28.10.08!2.2 ! ERWEITERUNG AUF 50 ZEILEN !KOB / LTH *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0588                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0588.
          05 PI-DRPA0588-DATEN.
             10 PI-DRPA0588-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA0588-PRUEF-PROD-SCHL      PIC S9(004)  BINARY.
             10 PI-DRPA0588-PRODUKT-TABELLE.
281008*         15 PI-DRPA0588-PRODUKT-ZEILE     OCCURS 15.
281008          15 PI-DRPA0588-PRODUKT-ZEILE     OCCURS 50.
                   20 PI-DRPA0588-PROD-PROD-SCHL PIC S9(004)  BINARY.
260608          15 PI-DRPA0588-MAX-USED-PROD     PIC S9(004)  BINARY.
      *
       01 PO-DRPA0588.
          05 PO-DRPA0588-RC                      PIC  X(003).
             88 PO-DRPA0588-RC-AEPROD                 VALUE 'APR'.
             88 PO-DRPA0588-RC-KEIN-AEPROD            VALUE 'KAP'.
          05 PO-DRPA0588-DATEN.
             10 PO-DRPA0588-KNR-AEPROD           PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************

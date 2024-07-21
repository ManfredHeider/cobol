      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1303.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1303.CPv  $
      *
      *   Rev 2.4   Sep 26 2016 13:58:02   t13ae39
      *APA02280: Dialoganpassungen SF43
      *
      *   Rev 2.3   13 Nov 2006 13:59:48   t13ae46
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.2   Mar 27 2003 11:01:08   t13ex21
      *APA00225 - Vergrößerung Tarif-Bezeichnung
      *
      *   Rev 2.1   Jan 04 2000 13:11:36   T13AE18
      *APA00068
      *
      *
      *   Rev 2.0   Sep 28 1999 14:06:24   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1303 FUNKTION LIST PRODUKT-TARIF-        *
      *                          ZUORDNUNG FÜR DIALOG SF43           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    29.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!29.09.99! 01 !NEU                        !BOHLINGER *
      * APA00225!21.03.03!2.01! ERWEITERUNG PG-TARIF      ! STORCH   *
      * APA00452!08.11.06!2.23! ERWEITERUNG PROD-PROD-SCHL! WANG     *
      * APA02280!04.07.16!2.4 ! Key-Auswahl SF43 ergänzt: ! Strauss  *
      *         !        !    ! gültig-ab und gültig-bis  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1303                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1303.
          05 PI-DRPA1303-UFKT                    PIC  9(02).
          05 PI-DRPA1303-AUSWAHL-KRITERIEN.
               10 PI-DRPA1303-PGEB-GES             PIC 9(002).
081106*        10 PI-DRPA1303-PROD-SCHL            PIC 9(002).
081106         10 PI-DRPA1303-PROD-PROD-SCHL       PIC S9(004) BINARY.
               10 PI-DRPA1303-S-PROD-SCHL          PIC X(001).
210303*        10 PI-DRPA1303-PGTA-TARIF           PIC X(012).
210303         10 PI-DRPA1303-PGTA-TARIF           PIC X(015).
               10 PI-DRPA1303-S-PGTA-TARIF         PIC X(001).
               10 PI-DRPA1303-S-SORT-PROD-SCHL     PIC X(001).
040716         10 PI-DRPA1303-GUELTIG-AB           PIC X(010).
040716         10 PI-DRPA1303-S-GUELTIG-AB         PIC X(010).
040716         10 PI-DRPA1303-GUELTIG-BIS          PIC X(010).
040716         10 PI-DRPA1303-S-GUELTIG-BIS        PIC X(010).
      *
       01 PO-DRPA1303.
          05  PO-DRPA1303-RC                     PIC  X(003).
              88 PO-DRPA1303-RC-OK                        VALUE 'OK '.
              88 PO-DRPA1303-RC-EOF                       VALUE 'EOF'.
              88 PO-DRPA1303-RC-LEER                      VALUE 'LER'.

          05 PO-DRPA1303-ERGEBNISZEILE.
             COPY DRPA0159.
      *
      ******** Ende Copy-Member **************************************


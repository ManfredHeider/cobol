      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.10.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1763.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1763.CPv  $
      *
      *   Rev 2.3   Mar 10 2014 16:50:50   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g) Migration
      *
      *   Rev 2.2   Jan 13 2014 14:05:12   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.1   Dec 11 2013 15:35:54   t13ae23
      *APA01539: Kalypso TP5: Anpassungen Datenmodell -
      *Tabelle TB
      *PA0033 PRKL -
      *
      *   Rev 2.0   Oct 01 2013 10:28:58   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *                                                              *
      *   PARAMETERBESCHREIBUNG                                      *
      *   IN     : PI-ZTPA1763                                       *
      *            PI-EINGABE                                        *
      *            STANDARD-STATUS-BEREICH                           *
      *   OUT    : PO-RUECKGABE                                      *
      *   IN/OUT : ERR-ERROR-BEREICH                                 *
      *                                                              *
      *   VERWENDETE UEBERGABEBEREICHE:                              *
      *                                                              *
      *   COPY : DRPA1763 - Z. B. FUNKTIONSNAME                      *
      *   IN   : PI-ZTPA0033                                         *
      *                                                              *
      *   COPY : DRPA1764 - UEBERGABEBEREICH FUER INSERT             *
      *   IN   : PI-DRPA1764                                         *
      *   OUT  : PO-DRPA1764                                         *
      *                                                              *
      *   COPY : DRPA1765 - UEBERGABEBEREICH FUER UPDATE             *
      *   IN   : PI-DRPA1765                                         *
      *   OUT  : PO-DRPA1765                                         *
      *                                                              *
      *   COPY : DRPA1766 - UEBERGABEBEREICH FUER SELECT ALL         *
      *   IN   : PI-DRPA1766                                         *
      *   OUT  : PO-DRPA1766                                         *
      *                                                              *
      *   COPY : DRPA1767 - UEBERGABEBEREICH FUER DELETE             *
      *   IN   : PI-DRPA1767                                         *
      *   OUT  : PO-DRPA1767                                         *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * APA01539!01.10.13! 2.1! NEUERSTELLUNG             ! RAHNER    *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
       01  PI-ZTPA0033.
           05 PI-ZTPA0033-FUNKTION            PIC X(008).
              88 PI-ZTPA0033-DRPA1764-INS         VALUE 'DRPA1764'.
              88 PI-ZTPA0033-DRPA1765-UPD         VALUE 'DRPA1765'.
              88 PI-ZTPA0033-DRPA1766-SEL-ALL     VALUE 'DRPA1766'.
              88 PI-ZTPA0033-DRPA1767-DEL         VALUE 'DRPA1767'.
      ******* ENDE COPY-MEMBER **************************************


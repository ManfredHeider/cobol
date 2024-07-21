      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1558.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1558.CPv  $
      *
      *   Rev 2.3   Apr 07 2016 10:14:16   t13ae23
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlu
      *ng - Sourcen Antrag -
      *
      *   Rev 2.2   Jun 16 2003 17:06:12   t13ae34
      *AAN00369 - Zugriff auf Daisy-Konfiguration aus eing.Geschäft
      *
      *   Rev 2.1   Feb 21 2003 12:11:46   t13ae34
      *AAN00369 - Zugriff auf Daisy-Konfiguration
      *
      *   Rev 2.0   Jul 04 2002 15:29:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1558 FUNKTION ERMITTELN GRUNDPROVISION   *
      *                          JE PRODUKT                          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    05.02.2003                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00369!05.02.03!2.1 !NEU                        !HEIDER    *
      * AAN00369!12.06.03!2.2 !NEU                        !HEIDER    *
      * APA02220!07.04.16!2.3 !4 VOR- U. NACHKOMMAST.     !RAHNER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1558                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1558.
          05 PI-DRPA1558-DATEN.
             10 PI-DRPA1558-PGEB-GES             PIC S9(004)  BINARY.
             10 PI-DRPA1558-PROD-PROD-SCHL       PIC S9(004)  BINARY.
             10 PI-DRPA1558-STICHTAG             PIC  X(010).
             10 PI-DRPA1558-PRODART              PIC S9(004)  BINARY.
             10 PI-DRPA1558-LAUFZEIT             PIC S9(004)  BINARY.
      *
       01 PO-DRPA1558.
          05 PO-DRPA1558-RC                      PIC  X(003).
             88 PO-DRPA1558-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1558-RC-NOK                    VALUE 'NOK'.
         05 PO-DRPA1558-DATEN.
      *      10 PO-DRPA1558-PRKO-GRP-STZ         PIC S9(009)V999
             10 PO-DRPA1558-PRKO-GRP-STZ         PIC S9(009)V9999
                                                     PACKED-DECIMAL.
      *      ABFRAGE WERTE ÜBER DRAT0155 - KONSTANTEN KTY 119
             10 PO-DRPA1558-KNR-KOND-REGEL-GRP   PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************


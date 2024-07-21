      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0387.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0387.CPv  $
      *
      *   Rev 2.3   May 20 2008 12:12:14   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.2   Jun 25 1999 14:06:20   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Jun 01 1999 11:19:02   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0387                                     *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WILLIG                                           *
      *    DATUM:    23.04.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00051!23.04.99! 01 ! NEUERSTELLUNG             !WILLIG    *
      * AVB00875!11.03.08!2.2 ! Feldverlängerung          !MAESS     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0387                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0387.
          05 PI-DRVB0387-MAAN-BEZUGS-NR          PIC S9(009) COMP.
      *
       01 PO-DRVB0387.

          05 PO-DRVB0387-RC                      PIC  X(003).
             88 PO-DRVB0387-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0387-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0387-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0387-DATEN.
             10 PO-DRVB0387-KNR-AUSL             PIC  X(004)     .
             10 PO-DRVB0387-MAAN-PLZ-PF          PIC  X(009)     .
             10 PO-DRVB0387-MAAN-ORT-PF          PIC  X(040)     .
             10 PO-DRVB0387-MAAN-POSTFACH        PIC  X(006)     .
120308*      10 PO-DRVB0387-MAAN-TELEFON         PIC  X(014)     .
120308*      10 PO-DRVB0387-MAAN-TELEFAX         PIC  X(014)     .
120308       10 PO-DRVB0387-MAAN-TELEFON         PIC  X(026)     .
120308       10 PO-DRVB0387-MAAN-TELEFAX         PIC  X(026)     .

      *

      ***** Ende Copy-Member DRVB0385 *****************************

      *---------------------------------------------------------------*
      *---------------------------------------------------------------*

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0386.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0386.CPv  $
      *
      *   Rev 2.5   May 20 2008 12:12:06   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.4   28 Dec 2000 12:55:46   T13AE30
      *AVB00517 - Anpassung wegen Übernahme Abrechnungsdruck durch DVA-A
      *
      *   Rev 2.3   Sep 02 1999 12:09:20   T13AE30
      * 
      *
      *   Rev 2.2   Jun 25 1999 14:06:16   T13AE30
      *AAS00051 - Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Jun 01 1999 11:19:00   T13AE30
      * 
      *
      *   Rev 2.0   Mar 17 1999 10:59:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0386                                      *
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
      * AVB00517!20.12.00! 02 ! ERWEITERUNG DER SCHNITT-  !WILLIG    *
      *         !        !    ! STELLE                    !          *
      * AVB00875!11.03.08! 02 ! TELEFON-/TELEFAX 14->26   !J.MAESS   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0386                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0386.
          05 PI-DRVB0386-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0386.

          05 PO-DRVB0386-RC                 PIC  X(003).
             88 PO-DRVB0386-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0386-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0386-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0386-DATEN.
             10 PO-DRVB0386-KNR-AUSL         PIC  X(004)      .
             10 PO-DRVB0386-MAAN-PLZ         PIC  X(009)      .
             10 PO-DRVB0386-MAAN-ORT         PIC  X(040)      .
             10 PO-DRVB0386-MAAN-STRASSE     PIC  X(040)      .
110308*      10 PO-DRVB0386-MAAN-TELEFON     PIC  X(014)      .
110308*      10 PO-DRVB0386-MAAN-TELEFAX     PIC  X(014)      .
110308       10 PO-DRVB0386-MAAN-TELEFON     PIC  X(026)      .
110308       10 PO-DRVB0386-MAAN-TELEFAX     PIC  X(026)      .
201200       10 PO-DRVB0386-MAAN-PLZ-PF      PIC  X(009)      .
201200       10 PO-DRVB0386-MAAN-ORT-PF      PIC  X(040)      .
201200       10 PO-DRVB0386-MAAN-POSTFACH    PIC  X(006)      .
             10 PO-DRVB0386-KNR-GRND         PIC S9(004) COMP.
      *
      ***** Ende Copy-Member DRVB0386 *****************************

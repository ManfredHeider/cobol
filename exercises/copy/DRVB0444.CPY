      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0444.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0444.CPv  $
      *
      *   Rev 2.2   May 20 2008 12:12:36   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.1   Dec 14 1999 09:31:56   T13AE27
      *Neue Funktion
      *
      *   Rev 2.0   Aug 04 1999 10:30:14   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0444                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Duerr                                            *
      *    DATUM:    22.09.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00143!22.09.99! 01 ! NEUERSTELLUNG             ! DUERR    *
      * AVB00875!11.03.08! 02 ! TELEFON-/TELEFAX 14->26   ! J.MAESS  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0444                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0444.
          05 PI-DRVB0444-MAAN-BEZUGS-NR     PIC S9(009) COMP.
      *
       01 PO-DRVB0444.

          05 PO-DRVB0444-RC                 PIC  X(003).
             88 PO-DRVB0444-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0444-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0444-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0444-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0444-DATEN.
             10 PO-DRVB0444-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0444-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0444-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0444-MAAN-KNR-AUSL    PIC  X(004).
110308*      10 PO-DRVB0444-MAAN-TELEFON     PIC  X(014).
110308*      10 PO-DRVB0444-MAAN-TELEFAX     PIC  X(014).
110308       10 PO-DRVB0444-MAAN-TELEFON     PIC  X(026).
110308       10 PO-DRVB0444-MAAN-TELEFAX     PIC  X(026).
             10 PO-DRVB0444-KNR-GRND         PIC S9(004) COMP.
             10 PO-DRVB0444-KNR-BEZ          PIC  X(080).
      *
      ***** ENDE COPY-MEMBER DRVB0444 *****************************


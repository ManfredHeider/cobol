      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0613.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0613.CPv  $
      *
      *   Rev 2.1   Nov 27 2002 09:53:30   t13ae35
      *ASL00152- neu, 1000-EUR-GSH
      *
      *   Rev 2.0   Nov 05 2002 11:36:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0613                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0017                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERBEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J. Hering                                        *
      *    DATUM:    12.11.2002                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00152!12.11.02!2.1 ! NEUERSTELLUNG             ! J. Hering*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    alle VBA's die innerhalb von 12 Monaten 1000 Einheiten    *
      *    abgerechnet haben und die nach dem 01.12.02 zum VBA       *
      *    befördert wurden bzw. direkt eingestiegen sind            *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0613.
          05 PI-DRVB0613-PROGRAMM           PIC  X(008).
          05 PI-DRVB0613-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0613-ABRECH-VON-ISO     PIC  X(010).
          05 PI-DRVB0613-ABRECH-BIS-ISO     PIC  X(010).
          05 PI-DRVB0613-BEGINN-VBA-ISO     PIC  X(010).
          05 PI-DRVB0613-FB-STUFE           PIC S9(004) BINARY.
          05 PI-DRVB0613-EH-GRENZE          PIC S9(009)V9 COMP-3.

      *
       01 PO-DRVB0613.

          05 PO-DRVB0613-RC                 PIC  X(003).
             88 PO-DRVB0613-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0613-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0613-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0613-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0613-DATEN.
             10 PO-DRVB0613-MA-ST-NR        PIC  S9(009) BINARY.
             10 PO-DRVB0613-VBA-SEIT-ISO    PIC  X(010).
             10 PO-DRVB0613-EH-GES-GES      PIC  S9(009)V9 COMP-3.
      *-- Direkteinstieg oder befördert                         --*---
             10 PO-DRVB0613-TYP             PIC  X(006).
      *
      ***** ENDE COPY-MEMBER DRVB0613 *****************************


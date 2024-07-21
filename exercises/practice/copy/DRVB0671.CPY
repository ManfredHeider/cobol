      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0671.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0671.CPv  $
      *
      *   Rev 2.2   Sep 01 2003 11:16:56   t13ae34
      *AVB00688 - Kommisarischer Leiter in Rosa Statistik (mit AAS00420)
      *
      *   Rev 2.1   Sep 01 2003 11:12:22   t13ae34
      *Kommisarischer Leiter in Rosa Statistik
      *
      *   Rev 2.0   Feb 17 2003 10:59:10   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0671                                      *
      *    --------------------------------------------------------   *
      *    BESCHREIBUNG:                                              *
      *    FUNKTION "ERMITTELN DIREKTIONSLEITER ZU DIREKTION UND      *
      *    DATUM"                                                     *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00420!27.08.03!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0671                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0671.
          05 PI-DRVB0671-DIR-NR             PIC S9(004)  COMP.
          05 PI-DRVB0671-DATUM              PIC  X(010).
      *
       01 PO-DRVB0671.

          05 PO-DRVB0671-RC                 PIC  X(003).
             88 PO-DRVB0671-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0671-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0671-DATEN.
             10 PO-DRVB0671-VB-NR           PIC S9(009)  COMP.
             10 PO-DRVB0671-DATUM-VON       PIC  X(010).
             10 PO-DRVB0671-DATUM-BIS       PIC  X(010).
      *
      ***** Ende Copy-Member DRVB0671 *****************************

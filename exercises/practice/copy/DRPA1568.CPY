      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1568.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1568.CPv  $
      *
      *   Rev 2.1   Feb 03 2004 11:44:58   t13ae34
      *FAN00485-Korrektur PEWA im Batch; hier: Vstatus '110'
      *
      *   Rev 2.0   Jul 04 2002 15:30:36   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1568  LIST PEWA                          *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    03.02.2004                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN00485!03.02.04!2.1 ! NEUERSTELLUNG             ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1568                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LIST FEHLERHAFTE PEWA TBPA0011                       *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1568.
          05 PI-DRPA1568-UFKT-CODE          PIC  9(002).
          05 PI-DRPA1568-AUSWAHL-KRITERIEN.
      *      OPTIONAL
             10 PI-DRPA1568-VSTAT           PIC  X(004).
      *
       01 PO-DRPA1568.
          05 PO-DRPA1568-RC                 PIC  X(003).
             88 PO-DRPA1568-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1568-RC-LEER                      VALUE 'LER'.
             88 PO-DRPA1568-RC-EOF                       VALUE 'EOF'.
          05 PO-DRPA1568-DATEN.
             10 PO-DRPA1568-PEWA-ID           PIC S9(15)V USAGE COMP-3.
             10 PO-DRPA1568-PGEB-GES          PIC S9(04)  USAGE COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************

      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  08.03.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0035.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAS0035.CPv  $
      *
      *   Rev 2.2   Mar 16 2016 13:00:08   t13ae30
      *AAS01159: Neue Wertung nach Zustrukturierung
      *
      *   Rev 2.1   May 03 2012 16:48:04   t13ae30
      *AAS01008: Siegeradler Wettbewerb 2012 ....
      *
      *   Rev 2.0   Mar 08 2012 15:59:18   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS01008!07.03.12!2.1 ! NEU                       !TH.WILLIG *
      * AAS01159!19.02.16!2.2 ! NEUE REGEL ZUSTRUK.PARTNER! WILLIG   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAS0035                                 *
      *    EH ZUSTRUKTURIERTER PARTNER                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAS0035.
          05 PI-DRAS0035-DATEN.
             10 PI-DRAS0035-PARTDW-BETR      PIC S9(009) COMP.
             10 PI-DRAS0035-JAHR             PIC S9(04) BINARY.
             10 PI-DRAS0035-PARTDW-VON       PIC  X(010).
             10 PI-DRAS0035-PARTDW-BIS       PIC  X(010).
             10 PI-DRAS0035-KTY-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0035-KNR-LEISTG-TYP   PIC S9(04) BINARY.
             10 PI-DRAS0035-KNR-ZUSTRK-TYP   PIC S9(04) BINARY.
      *
       01 PO-DRAS0035.
          05 PO-DRAS0035-RC                  PIC  X(003).
             88 PO-DRAS0035-RC-OK            VALUE 'OK '.
             88 PO-DRAS0035-RC-NFD           VALUE 'NFD'.

          05 PO-DRAS0035-DATEN.
             10 PO-DRAS0035-BETR-ST-NR       PIC S9(009)   COMP.
             10 PO-DRAS0035-EH-ZUSTRUK-PART  PIC S9(009)V9 COMP-3.
      ******** ENDE COPY-MEMBER **************************************


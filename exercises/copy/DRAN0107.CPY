      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0107.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0107.CPv  $
      *
      *   Rev 2.1   Mar 18 2005 10:37:48   t13ae34
      *AKI00479: Funktion: eingereichte Einheiten zu VB in best. Ze
      *itraum
      *
      *   Rev 2.0   Mar 14 2005 12:04:46   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0107                                     *
      *    FUNKTION   : ERMITTELT  VB EINGEREICHTES GESCHÄFT IM JAHR *
      *                 JHJJ                                         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    05.07.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00002!12.06.98! 01 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0107                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0107.
          05 PI-DRAN0107-DATEN.
             10 PI-DRAN0107-VB-NR        PIC S9(007) COMP.
             10 PI-DRAN0107-DATUM-VON    PIC  X(010).
             10 PI-DRAN0107-DATUM-BIS    PIC  X(010).
      *
       01 PO-DRAN0107.
          05 PO-DRAN0107-RC              PIC  X(003).
             88 PO-DRAN0107-RC-OK             VALUE 'OK '.
             88 PO-DRAN0107-RC-NOK            VALUE 'NOK'.
          05 PO-DRAN0107-DATEN.
             10 PO-DRAN0107-PEIGEN.
                15 PO-DRAN0107-PEAEINH   PIC S9(8)V9     COMP-3.
                15 PO-DRAN0107-PESTCK    PIC S9(7)       COMP-3.
                15 PO-DRAN0107-PESUM     PIC S9(11)      COMP-3.
                15 PO-DRAN0107-PEBEITR   PIC S9(11)V99   COMP-3.
             10 PO-DRAN0107-PGRUPPE.
                15 PO-DRAN0107-PGRAEINH  PIC S9(8)V9     COMP-3.
                15 PO-DRAN0107-PGRSTCK   PIC S9(7)       COMP-3.
                15 PO-DRAN0107-PGRSUM    PIC S9(11)      COMP-3.
                15 PO-DRAN0107-PGRBEITR  PIC S9(11)V99   COMP-3.
             10 PO-DRAN0107-PGESAMT.
                15 PO-DRAN0107-PGEAEINH  PIC S9(8)V9     COMP-3.
                15 PO-DRAN0107-PGESTCK   PIC S9(7)       COMP-3.
                15 PO-DRAN0107-PGESUM    PIC S9(11)      COMP-3.
                15 PO-DRAN0107-PGEBEITR  PIC S9(11)V99   COMP-3.
      ******** ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1430.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRPA1430.CPv  $
      *
      *   Rev 2.6   Dec 10 2013 09:54:42   t13ex67
      *FPA01337: Rücknahme Erweiterung ppfl. Summe bei HVB
      *
      *   Rev 2.5   Dec 09 2013 15:47:04   t13ex67
      *FPA01337: Rücknahme Erweiterung ppfl. Summe bei HVB
      *
      *   Rev 2.4   Dec 02 2013 14:52:12   t13ex67
      *APA01698: Erweiterung ppfl. Summe bei HVB
      *
      *   Rev 2.3   Nov 05 2013 09:15:32   t13ex67
      *FPA01334: Rücknahme Erweiterung ppfl. Summe bei HVB
      *
      *   Rev 2.2   Oct 29 2013 07:13:10   t13ex67
      *FPA01333: Erweiterung ppfl. Summe bei HVB
      *
      *   Rev 2.1   15 Nov 2000 16:08:58   T13EX43
      *APA00116 : Hypo-PEG umsetzen und einführen
      *
      *   Rev 2.0   12 Jul 2000 12:33:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1430  PEG-DATENSATZ GES. 64 - HYPO-V-BANK *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.11.00                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00116!08.11.00!2.1 ! NEU                       ! KLEEMEIER*
      * FPA01333!28.10.13!2.2 ! ÄNDERUNG FELD PPFL-SUMME  ! STORCH   *
      * FPA01334!05.11.13!2.3 ! RÜCKNAHME ÄNDERUNG        ! STORCH   *
      * APA01698!02.12.13!2.4 ! ÄNDERUNG FELD PPFL-SUMME  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    HYPO-VEREINSBANK-DTAS.                                    *
      *                                                              *
      ****************************************************************
      *
             30 HYPO-DATENSATZ.
                35 VNR                      PIC  9(015).
                35 FAELLIG-VON              PIC  X(010).
                35 AVERM                    PIC  9(007).
                35 PGEB-GES                 PIC  9(002).
                35 KUNDE-NNAME              PIC  X(030).
                35 KUNDE-VNAME              PIC  X(030).
                35 ABR-DAT-PG               PIC  X(010).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 PGV-PG                   PIC  X(004).
                35 PROD-BEZ                 PIC  X(015).
                35 PROV-SATZ                PIC S9(004)V9(03) COMP-3.
                35 PROV-BETRAG              PIC S9(007)V9(02) COMP-3.
021213*         35 PPFL-SUMME               PIC S9(007)V9(02) COMP-3.
021213          35 PPFL-SUMME               PIC S9(009)V9(02) COMP-3.
021213*         35 FILLER                   PIC  X(019).
021213          35 FILLER                   PIC  X(018).
      *
      ******* ENDE COPY-MEMBER **************************************

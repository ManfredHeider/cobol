      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1513.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1513.CPv  $
      *
      *   Rev 2.1   Dec 05 2001 11:13:14   t13ex43
      *APA00167 : PEG Bank Austria (Sonstige Banken Österreich)
      *
      *   Rev 2.0   04 Jul 2001 11:10:54   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1513  PEG-DATENSATZ GES.65 - BANK AUSTRIA *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    22.11.01                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!22.11.01!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    BANK AUSTRIA-DTAS.                                        *
      *                                                              *
      ****************************************************************
      *
             30 BANK-AUSTRIA-DATENSATZ.
                35 VNR                      PIC  9(015).
                35 VERTRAG-BEGINN           PIC  X(010).
                35 FAELLIG-VON              PIC  X(010).
                35 AVERM                    PIC  9(007).
                35 PGEB-GES                 PIC  9(002).
                35 KUNDE-NNAME              PIC  X(025).
                35 KUNDE-VNAME              PIC  X(020).
                35 ABR-DAT-PG               PIC  X(010).
                35 PIN                      PIC  9(007).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 PGV-PG                   PIC  X(004).
                35 PROD-BEZ                 PIC  X(012).
                35 PROV-SATZ                PIC  9(003)V9(03).
                35 PROV-BETRAG              PIC S9(007)V9(02).
                35 PPFL-SUMME               PIC S9(007)V9(02).
      *
      ******* ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1486.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1486.CPv  $
      *
      *   Rev 2.1   30 Mar 2001 14:47:40   T13EX21
      *APA00131 - Neuerstellung
      *
      *   Rev 2.0   09 Jan 2001 10:22:28   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1486  PEG-DATENSATZ VON DGI (49)          *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    26.03.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!26.03.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DTA'S FÜR DGI.                                            *
      ****************************************************************
      *
             30 DGI-DAISY-DATENSATZ.
                35 VNR                              PIC  X(15).
                35 VERTRAG-BEGINN                   PIC  9(08).
                35 FAELLIG-VON                      PIC  9(08).
                35 AVERM                            PIC  9(07).
                35 PGEB-GES                         PIC  9(02).
                35 KUNDE-NNAME                      PIC  X(25).
                35 KUNDE-VNAME                      PIC  X(20).
                35 ABR-DAT-PG                       PIC  9(08).
                35 PIN                              PIC  9(07).
                35 ZAHLWEISE                        PIC  X(02).
                35 VERTRAG-WAEHRUNG                 PIC  X(03).
                35 PGV-PG.
                   40 STORNO-KZ                     PIC  X(01).
                   40 ERSTER-UMSATZ-KZ              PIC  X(01).
                   40 EINMAL-ANLAGE-KZ              PIC  X(01).
                   40 FILLER                        PIC  X(01).
                35 PG-PROD-BEZ                      PIC  X(15).
                35 PROV-SATZ                        PIC  9(03)V999.
                35 PROV-BETRAG                      PIC S9(07)V99.
                35 PPFL-SUMME                       PIC S9(07)V99.
                35 UMSATZSCHL                       PIC  9(02).
      *
      ******** ENDE COPY-MEMBER **************************************

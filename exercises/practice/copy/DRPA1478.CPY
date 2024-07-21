      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1478.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRPA1478.CPv  $
      *
      *   Rev 2.2   17 Sep 2001 09:22:06   t13ex21
      *APA00150 - Änderung Datenfeld
      *
      *   Rev 2.1   09 Mar 2001 12:09:46   T13EX21
      * 
      *
      *   Rev 2.0   09 Jan 2001 10:22:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1478  PEG-DATENSATZ VON AML-DARLEHEN (63) *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    28.02.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!28.02.01!2.1 ! NEUERSTELLUNG             ! STORCH   *
      * APA00150!10.09.01!2.2 ! ÄNDERUNG PPFL. SUMME      ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    AML-DARLEHEN-DTA'S.                                        *
      ****************************************************************
      *
             30 AML-DARLEHEN-DATENSATZ.
                35 VNR                              PIC  9(15).
                35 VERTRAG-BEGINN                   PIC  X(10).
                35 FAELLIG-VON                      PIC  X(10).
                35 AVERM                            PIC  9(07).
                35 PGEB-GES                         PIC  9(02).
                35 KUNDE-NNAME                      PIC  X(25).
                35 KUNDE-VNAME                      PIC  X(20).
                35 ABR-DAT-PG                       PIC  X(10).
                35 VERTRAG-WAEHRUNG                 PIC  X(03).
                35 PGV-PG                           PIC  X(04).
                35 PG-PROD-BEZ                      PIC  X(12).
                35 PROV-SATZ                        PIC  9(03)V999.
                35 PROV-BETRAG                      PIC S9(07)V99.
100901*         35 PPFL-SUMME                       PIC S9(07)V99.
100901          35 PPFL-SUMME                       PIC S9(09)V99.
      *
      ******** ENDE COPY-MEMBER **************************************


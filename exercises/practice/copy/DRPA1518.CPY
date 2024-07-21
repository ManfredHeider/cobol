      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1518.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1518.CPv  $
      *
      *   Rev 2.1   Feb 06 2002 12:37:18   T13ex25
      *APA00172; PEG DWS neu
      *
      *   Rev 2.0   04 Jul 2001 11:11:02   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1518  PEG-DATENSATZ FUER DWS GES 48
      *    --------------------------------------------------------
      *    ERSTELLT: JAEGER
      *    DATUM:    16.01.02
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00172!16.01.02!2.1 ! NEU                       !JAEGER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DWS-DTA's.                                                *
      *                                                              *
      ****************************************************************
      *
             30 DWS-DATENSATZ.
                35 VNR                    PIC  9(015).
                35 REDEFINES VNR.
                   40 STAMM-NR.
                      45 FILIAL-NR        PIC  9(003).
                      45 KUNDEN-NR        PIC  9(007).
                   40 UNTER-NR            PIC  9(002).
                   40 WAEHRUNG-ACCOUNT    PIC  9(003).
                35 PRODUKT-ID             PIC  X(020).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 PGEB-GES               PIC  9(002).
                35 PIN                    PIC  9(007).
                35 PIN-PRUEFZIFFER        PIC  9(002).
                35 KUNDE-NNAME            PIC  X(035).
                35 KUNDE-VNAME            PIC  X(035).
                35 ABR-DAT-PG             PIC  X(010).
                35 ZAHLWEISE              PIC  9(002).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 PGV-PG                 PIC  X(004).
                35 PRODUKT-TYP            PIC  X(012).
                35 WKN                    PIC  X(012).
                35 PGV-KZ                 PIC  X(001).
                35 PROV-SATZ              PIC  9(003)V999.
                35 GSMT-PROV              PIC S9(007)V99.
                35 PPFL-SUMME             PIC S9(007)V99.
                35 PG-SUMME               PIC S9(007)V99.
                35 VL-KENNZEICHEN         PIC  X(002).
                35 FILLER                 PIC  X(074).
      ******* ENDE COPY-MEMBER **************************************


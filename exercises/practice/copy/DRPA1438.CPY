      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1438.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1438.CPv  $
      *
      *   Rev 2.1   05 Feb 2001 10:19:40   T13AE29
      *APA0118: Einführung der Gesellschaften 46 (BfG Invest) und 
      *          47 (BFG IMMOINVEST)
      *
      *   Rev 2.0   12 Jul 2000 12:33:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1438  PEG-DATENSATZ DER BFG GES. 46/47    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    18.12.00                                         *
      *****************************************************************
      *    VERSION                                                     
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!18.12.00!2.1 ! NEU                       !BOHLINGER *
      * APA00118!17.01.01!2.2 ! zusätzl. Feld UMSATZ-KZ   !Brenner   *
      *         !        !    !               VERTRAGSART            *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    BfG-DTAS.                                                 *
      *          BfG-Immo und BfG-Inest                              *
      ****************************************************************
      *                                                               
             30 BfG-DATENSATZ.
                35 VNR                              PIC  9(015).      
                35 VERTRAG-BEGINN                   PIC  X(010).      
                35 FAELLIG-VON                      PIC  X(010).      
                35 AVERM                            PIC  9(007).      
                35 PGEB-GES                         PIC  9(002).      
                35 KUNDE-NNAME                      PIC  X(030).      
                35 KUNDE-VNAME                      PIC  X(020).      
                35 ABR-DAT-PG                       PIC  X(010).      
                35 PIN                              PIC  9(007).      
                35 ZAHLWEISE                        PIC  9(002).      
                35 VERTRAG-WAEHRUNG                 PIC  X(003).      
                35 PGV-PG                           PIC  X(004).     
                35 WPKN                             PIC  X(012).     
                35 PGV-KENNZEICHEN                  PIC  X(001).     
                35 PROV-SATZ                        PIC  9(003)V999. 
                35 PROV-BETRAG                      PIC S9(007)V99.  
                35 PPFL-SUMME                       PIC S9(007)V99.  
                35 SPARBETRAG                       PIC S9(007)V99.
010117          35 UMSATZ-KZ.
010117             40 UMSATZ-KZ-1                   PIC  X(001).
010117             40 UMSATZ-KZ-2                   PIC  X(001).
010117          35 VERTRAGSART                      PIC  9(002).
                35 FILLER                           PIC  X(015).
      *                                                              
      ******* ENDE COPY-MEMBER **************************************      
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1516.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1516.CPv  $
      *
      *   Rev 2.4   Nov 18 2008 13:18:30   t13ae37
      *APA00828: Anpassung Provisionsschnittstelle Ges 45 
      *
      *   Rev 2.3   Jun 05 2002 16:35:00   t13ex44
      *Anpassung Summenfelder
      *
      *   Rev 2.2   May 15 2002 13:43:46   t13ex44
      *Korrektur Laenge der Felder Provision, povisionspflichtige
      *Summe, Anlagebetrag
      *
      *   Rev 2.1   Dec 04 2001 10:37:50   t13ex25
      *APA00167; Anpassungen für Bank Austria Invest
      *
      *   Rev 2.0   04 Jul 2001 11:11:00   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1516  PEG-DATENSATZ DER                   *
      *                           BANK AUSTRIA INVEST                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jäger-Sokoluk                                    *
      *    *                                                          *
      *    DATUM:    30.11.01                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00167!30.11.01!2.0 ! NEU                       !Jäger-Soko*
      * APA00167!15.05.02!2.1 ! PROV-BETRAG, PPFL-SUMME,  !SCHEIDEL  *
      *         !        !    ! SPARBETRAG -> 8 VK-STELLEN!          *
      * APA00828!17.11.08!2.2 ! ANPASSUNG SST AUSTRIA-INV.!BUCH      *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    Bank Austria DTA's                                        *
      *    Satzlänge: 158 Byte (bei Erstellung)                      *
      ****************************************************************
      *
             30 AUSTRIA-INVEST-DATENSATZ.
                35 PGEB-GES                         PIC  9(002).
                35 VNR                              PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 KUNDE-NNAME                      PIC  X(025).
                35 KUNDE-VNAME                      PIC  X(020).
                35 ABR-DAT-PG                       PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  X(004).
171108*         35 WPKN                             PIC  X(006).
171108          35 ISIN                             PIC  X(012).
                35 PROV-SATZ                        PIC  9(003)V999.
150502*         35 PROV-BETRAG                      PIC S9(007)V99.
150502          35 PROV-BETRAG                      PIC S9(008)V99.
150502*         35 PPFL-SUMME                       PIC S9(007)V99.
150502          35 PPFL-SUMME                       PIC S9(008)V99.
150502*         35 SPARBETRAG                       PIC S9(007)V99.
150502          35 SPARBETRAG                       PIC S9(008)V99.
      *
      ******* ENDE COPY-MEMBER **************************************

      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1393.cpy  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\drpa1393.cpv  $
      *
      *   Rev 2.3   11 Jul 2000 08:32:28   T13EX22
      *APA0098: PEG III 
      *
      *   Rev 2.2   13 Jun 2000 14:03:52   T13EX23
      *APA00098 - VNR / Datum falsch definiert
      *
      *   Rev 2.1   13 Jun 2000 12:32:34   T13EX23
      *APA00098 - Neuerstellung
      *
      *   Rev 2.0   Apr 11 2000 16:58:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1393  PEG-DATENSATZ DER III GES. 41       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRAY                                             *
      *    DATUM:    13.06.00                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00098!13.06.00!2.1 ! NEU                       ! GRAY     *
      * APA00098!13.06.00!2.2 ! EINIGE FELDER FALSCH      ! GRAY     *
      *         !        !    ! DEFINIERT                 !          *
      * APA00098!07.07.00!2.3 ! PPFL-BEITRAG->PPFL-SUMME  ! MUND     *
      *         !        !    ! PG-BEITRAG->SPARBETRAG    !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    III-DTAS.                                                 *
      *                                                              *
      ****************************************************************
      *
             30 III-DATENSATZ.
                35 VNR                              PIC  9(015).
                35 VERTRAG-BEGINN                   PIC  X(010).
                35 FAELLIG-VON                      PIC  X(010).
                35 AVERM                            PIC  9(007).
                35 PGEB-GES                         PIC  9(002).
                35 KUNDE-NNAME                      PIC  X(035).
                35 KUNDE-VNAME                      PIC  X(035).
                35 ABR-DAT-PG                       PIC  X(010).
                35 PIN                              PIC  9(007).
                35 ZAHLWEISE                        PIC  9(002).
                35 VERTRAG-WAEHRUNG                 PIC  X(003).
                35 PGV-PG                           PIC  9(004).
                35 WPKN                             PIC  X(012).
                35 SPARPLAN-NR                      PIC  9(002).
                35 PPFL-SUMME                       PIC S9(007)V99.
                35 SPARBETRAG                       PIC S9(007)V99.
                35 PROV-SATZ                        PIC  9(003)V999.
                35 PROV-BETRAG                      PIC S9(007)V99.
      *
      ******* ENDE COPY-MEMBER **************************************

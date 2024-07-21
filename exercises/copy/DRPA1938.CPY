      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  28.10.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1938.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRPA1938.CPv  $
      *
      *   Rev 2.4   Nov 06 2019 15:40:24   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.3   Nov 06 2019 15:32:28   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.2   Nov 06 2019 08:35:52   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.1   Nov 04 2019 14:16:38   t13ex26
      *APA02879: Baufinanzierung 2.0 - Anbindung einer Finanzierung
      *splattform (Provision)
      *
      *   Rev 2.0   Oct 28 2019 13:50:58   t13ex26
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1938  PEG-DATENSATZ VON                   *
      *                           DVAG-HYP (GES. 61)                  *
      *    ---------------------------------------------------------  *
      *    ERSTELLT: KRANZ                                            *
      *    DATUM:    28.10.19
      *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02879!28.10.19!2.1 ! NEUERSTELLUNG             ! KRANZ    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    DVAG-HYP DTA'S.                                           *
      *                                                              *
      ****************************************************************
      *
             30 DVAG-HYP-DATENSATZ.
                35 PGEB-GES               PIC  9(002).
                35 VNR                    PIC  9(015).
                35 VERTRAG-BEGINN         PIC  X(010).
                35 FAELLIG-VON            PIC  X(010).
                35 AVERM                  PIC  9(007).
                35 KUNDE-NNAME            PIC  X(035).
                35 KUNDE-VNAME            PIC  X(035).
                35 ABR-DAT-PG             PIC  X(010).
                35 VERTRAG-WAEHRUNG       PIC  X(003).
                35 ZAHLWEISE              PIC  X(002).
                35 PG-PGV                 PIC  X(004).
                35 PG-PROD-BEZ            PIC  X(012).
                35 PPFL-SUMME             PIC  9(008)V99.
                35 PROV-SATZ              PIC  9(003)V999.
                35 PROV-BETRAG            PIC  9(007)V99.
                35 ANTRAGSDATEN           PIC  X(055).
                35 FILLER                 PIC  X(025).
      ******* ENDE COPY-MEMBER **************************************
 




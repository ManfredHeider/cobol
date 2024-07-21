      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1546.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1546.CPv  $
      *
      *   Rev 2.3   Mar 19 2009 08:25:16   t13ex44
      *APA00868: Erweiterung Prov- und Rück-DTA Generali
      *
      *   Rev 2.2   Jul 03 2003 16:19:00   t13ex21
      *APA00226 - Neu: Ersatz-PIN
      *
      *   Rev 2.1   Aug 21 2002 09:11:10   t13ae46
      *APA00206: Neuerstellung
      *
      *   Rev 2.0   04 Jul 2001 11:11:46   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *COPY-MEMBER: DRPA1546  PEG-DATENSATZ GES. 26 - GENERALI LEBEN  *
      *                                                               *
      *---------------------------------------------------------------*
      *    ERSTELLT: WANG                                             *
      *    DATUM:    25.06.02                                         *
      *****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!25.06.02!2.1 ! NEU                       ! WANG     *
      * APA00226!07.04.03!2.2 ! NEUES FELD: ERSATZ-PIN    ! STORCH   *
      * APA00868!03.02.09!2.3 ! NEUES FELD 'GHKZ'         ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    GENERALI LEBEN-DTAS.                                      *
      ****************************************************************
      *
             30 GENERALI-LEBEN-DATENSATZ.
                35 PGEB-GES                 PIC  9(002).
                35 VNR                      PIC  9(015).
                35 VERTRAG-BEGINN           PIC  X(010).
                35 FAELLIG-VON              PIC  X(010).
                35 AVERM                    PIC  9(007).
                35 KUNDE-NNAME              PIC  X(025).
                35 KUNDE-VNAME              PIC  X(020).
                35 ABR-DAT-PG               PIC  X(010).
                35 PIN                      PIC  9(007).
                35 ZAHLWEISE                PIC  X(002).
                35 VERTRAG-WAEHRUNG         PIC  X(003).
                35 HAFTZ-BEGINN             PIC  X(010).
                35 HAFTZ-ENDE               PIC  X(010).
                35 PGV-PG                   PIC  X(004).
                35 PG-TARIF                 PIC  X(012).
                35 PROV-SATZ                PIC  9(003)V9(03).
                35 PROV-BETRAG              PIC S9(007)V9(02) COMP-3.
                35 PPFL-BEITRAG             PIC S9(007)V9(02) COMP-3.
                35 PG-BEITRAG               PIC S9(007)V9(02) COMP-3.
070403*         35 BELASTUNGSDAUER          PIC  9(003).
070403          35 ERSATZ-PIN               PIC  9(007).
030209          35 GHKZ                     PIC  X(002).
      *
      ******* ENDE COPY-MEMBER **************************************
      *                                                              *

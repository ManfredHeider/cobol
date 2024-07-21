      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1536.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1536.CPv  $
      *
      *   Rev 2.2   Nov 03 2006 13:31:52   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.1   Jul 18 2002 10:54:52   t13ex43
      *APA00206 : PEG S-Bausparkasse
      *
      *   Rev 2.0   04 Jul 2001 11:11:28   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1536  PEG-DATENSATZ GES. 75 - S-BAUSPARK.*
      *    ----------------------------------------------------------*
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    19.06.02                                        *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00206!19.06.02!2.1 ! NEU                       ! KLEEMEIER*
      * APA00452!03.11.06!2.2 ! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES PEG-DATENSATZES DES    *
      *    S-BAUSPARKASSE-DTAS.                                      *
      *                                                              *
      ****************************************************************
      *
             30 S-BAUSPAR-DATENSATZ.
                35 PGEB-GES                 PIC  9(002).
                35 FILLER                   PIC  X(005).
                35 VNR                      PIC  9(009).
                35 FILLER                   PIC  X(001).
                35 FAELLIG-VON              PIC  9(008).
031106*         35 PRODUKT                  PIC  9(002).
031106          35 TARIF                    PIC  9(002).
                35 PRODART                  PIC  9(001).
                35 PIN                      PIC  9(007).
                35 FILLER                   PIC  X(002).
                35 ABR-DAT-PG               PIC  9(008).
                35 KUNDE-NAME               PIC  X(020).
                35 AVERM                    PIC  9(007).
                35 FILLER                   PIC  X(001).
                35 BUCH-DAT-PG              PIC  9(008).
                35 PROV-SATZ                PIC  9(002)V9(03).
                35 PPFL-SUMME               PIC  9(007)V9(02).
                35 PROV-BETRAG              PIC  9(007)V9(02).
                35 HAFTZ-BEGINN             PIC  9(006).
                35 HAFTZ-ENDE               PIC  9(006).
      *
      ******* ENDE COPY-MEMBER **************************************
